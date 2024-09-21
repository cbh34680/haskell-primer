{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

{-
正格評価例

https://stackoverflow.com/questions/9567040/poor-performance-parsing-binary-file-in-haskell
-}

import System.IO
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Binary.Get
import Data.Bool
import Data.Either
import GHC.Word
import Debug.Trace

import qualified Data.ByteString.Lazy as BL

main = do
    hdl <- openBinaryFile "a.out" ReadMode
    input <- BL.hGetContents hdl

    x <- runExceptT (step input)

    when (isLeft x) $ do
        let Left (_, off, msg) = x
        putStrLn $ mconcat [show off, "/", msg]

    putStrLn "done."


type GetErr = (BL.ByteString, ByteOffset, String)


step :: BL.ByteString -> ExceptT GetErr IO Int
step input = do
    (_, off, ehdr) <- except $ runGetOrFail getEhdr input

    lift $ print off >> print ehdr

    (_, off, phdrs) <- except $ runGetOrFail (getPhdrs (e_phnum ehdr)) $
        BL.drop (fromIntegral $ e_phoff ehdr) input

    lift $ putStrLn "-----" >> print off >> (mapM_ print $ zipWith (,) [1..] phdrs)

    (_, off, shdrs) <- except $ runGetOrFail (getShdrs (e_shnum ehdr)) $
        BL.drop (fromIntegral $ e_shoff ehdr) input

    lift $ putStrLn "-----" >> print off >> (mapM_ print $ zipWith (,) [1..] shdrs)

    return 1



checkM msg p m = do
    x <- m
    bool (fail msg) (return x) (p x)

{-
Elf64_Addr 符号無し 64 ビットプログラムアドレス 
Elf64_Half 符号無し 16 ビットフィールド 
Elf64_Lword 符号無し 64 ビットフィールド 
Elf64_Off 符号無し 64 ビットファイルオフセット 
Elf64_Sword 符号付き 32 ビットフィールド 
Elf64_Sxword 符号付き 64 ビットフィールドまたは整数 
Elf64_Word 符号無し 32 ビットフィールド 
Elf64_Xword 符号無し 64 ビットフィールドまたは整数
-}

{-
https://github.com/torvalds/linux/blob/master/include/uapi/linux/elf.h
http://www.yosbits.com/opensonar/rest/man/freebsd/man/ja/man5/elf.5.html?l=ja

typedef struct
{
  unsigned char e_ident[EI_NIDENT]; /* Magic number and other info */
  Elf64_Half    e_type;         /* Object file type */
  Elf64_Half    e_machine;      /* Architecture */
  Elf64_Word    e_version;      /* Object file version */
  Elf64_Addr    e_entry;        /* Entry point virtual address */
  Elf64_Off     e_phoff;        /* Program header table file offset */
  Elf64_Off     e_shoff;        /* Section header table file offset */
  Elf64_Word    e_flags;        /* Processor-specific flags */
  Elf64_Half    e_ehsize;       /* ELF header size in bytes */
  Elf64_Half    e_phentsize;        /* Program header table entry size */
  Elf64_Half    e_phnum;        /* Program header table entry count */
  Elf64_Half    e_shentsize;        /* Section header table entry size */
  Elf64_Half    e_shnum;        /* Section header table entry count */
  Elf64_Half    e_shstrndx;     /* Section header string table index */
} Elf64_Ehdr;
-}

{-
1 ヘッダー情報を表示 (-eオプション)
1.1 ELFヘッダー情報を表示 (-hオプション)
1.2 プログラムヘッダー情報を表示 (-lオプション)
1.3 セクションヘッダー情報を表示 (-Sオプション)
2 セクションの詳細を表示 (-tオプション)
3 シンボルテーブルを表示 (-sオプション)
4 動的シンボルテーブルを表示 (--dyn-symsオプション)
5 リロケーション情報を表示 (-rオプション)
6 動的セクションを表示 (-dオプション)
7 バージョン情報の表示 (-Vオプション)

(oracle-cli) ubuntu@vm1:~/src/haskell-primer$ readelf -h a.out
ELF ヘッダ:
  マジック:   7f 45 4c 46 02 01 01 00 00 00 00 00 00 00 00 00 
  クラス:                            ELF64
  データ:                            2 の補数、リトルエンディアン
  Version:                           1 (current)
  OS/ABI:                            UNIX - System V
  ABI バージョン:                    0
  型:                                DYN (Position-Independent Executable file)
  マシン:                            Advanced Micro Devices X86-64
  バージョン:                        0x1
  エントリポイントアドレス:               0x1060
  プログラムヘッダ始点:          64 (バイト)
  セクションヘッダ始点:          14768 (バイト)
  フラグ:                            0x0
  Size of this header:               64 (bytes)
  Size of program headers:           56 (bytes)
  Number of program headers:         13
  Size of section headers:           64 (bytes)
  Number of section headers:         37
  Section header string table index: 36
-}

data Ehdr = Ehdr {
    e_entry :: !Word64
    ,e_phoff :: !Word64
    ,e_shoff :: !Word64
    ,e_ehsize :: !Word16
    ,e_phentsize :: !Word16
    ,e_phnum :: !Word16
    ,e_shentsize :: !Word16
    ,e_shnum :: !Word16
    ,e_shstrndx :: !Word16

    } deriving Show


getEhdr :: Get Ehdr
getEhdr = do
    checkM "EI_MAG0" (== 0x7f) getWord8
    checkM "EI_MAG1-3" (== "ELF") $ getByteString 3
    checkM "EI_CLASS" (== 2) getWord8
    checkM "EI_DATA" (== 1) getWord8
    checkM "EI_VERSION" (== 1) getWord8
    checkM "EI_OSABI" (== 0) getWord8
    checkM "EI_ABIVERSION" (== 0) getWord8

    --checkM "unuse" (all (== 0)) $ replicateM 7 getWord8
    skip 7

    checkM "e_type" (== 3) getWord16host
    checkM "e_machine" (== 62) getWord16host
    checkM "e_version" (== 1) getWord32host

    e_entry <- getWord64host
    e_phoff <- getWord64host
    e_shoff <- getWord64host
    --ehdr' <- Ehdr <$> getWord64host <*> getWord64host <*> getWord64host

    --checkM "unuse" (all (== 0)) $ replicateM 4 getWord8
    skip 4      -- e_flags

    {-
    e_ehsize <- getWord16host
    e_phentsize <- getWord16host
    e_phnum <- getWord16host
    e_shentsize <- getWord16host
    e_shnum <- getWord16host
    e_shstrndx <- getWord16host

    checkM "eh_size" (((==) . fromIntegral) e_ehsize) bytesRead

    return $! Ehdr {e_entry, e_phoff, e_shoff,
        e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx}
    -}

    --ehdr <- ehdr' <$>
    ehdr <- Ehdr e_entry e_phoff e_shoff <$>
        getWord16host <*> getWord16host <*> getWord16host <*>
        getWord16host <*> getWord16host <*> getWord16host

    checkM "eh_size" (((==) . fromIntegral) (e_ehsize ehdr)) bytesRead

    return ehdr


{-
typedef struct
{
  Elf64_Word    p_type;         /* Segment type */
  Elf64_Word    p_flags;        /* Segment flags */
  Elf64_Off     p_offset;       /* Segment file offset */
  Elf64_Addr    p_vaddr;        /* Segment virtual address */
  Elf64_Addr    p_paddr;        /* Segment physical address */
  Elf64_Xword   p_filesz;       /* Segment size in file */
  Elf64_Xword   p_memsz;        /* Segment size in memory */
  Elf64_Xword   p_align;        /* Segment alignment */
} Elf64_Phdr;

-}

data Phdr = Phdr {
    p_type :: !Word32
    ,p_flags :: !Word32
    ,p_offset :: !Word64
    ,p_vaddr :: !Word64
    ,p_paddr :: !Word64
    ,p_filesz :: !Word64
    ,p_memsz :: !Word64
    ,p_align :: !Word64

    } deriving Show

getPhdrs :: Integral a => a -> Get [Phdr]
getPhdrs num = do

    forM [1.. (fromIntegral num)] $ \_ ->

        Phdr <$> getWord32host <*> getWord32host <*>
            getWord64host <*> getWord64host <*> getWord64host <*>
            getWord64host <*> getWord64host <*> getWord64host


{-
typedef struct elf64_shdr {
  Elf64_Word sh_name;		/* Section name, index in string tbl */
  Elf64_Word sh_type;		/* Type of section */
  Elf64_Xword sh_flags;		/* Miscellaneous section attributes */
  Elf64_Addr sh_addr;		/* Section virtual addr at execution */
  Elf64_Off sh_offset;		/* Section file offset */
  Elf64_Xword sh_size;		/* Size of section in bytes */
  Elf64_Word sh_link;		/* Index of another section */
  Elf64_Word sh_info;		/* Additional section information */
  Elf64_Xword sh_addralign;	/* Section alignment */
  Elf64_Xword sh_entsize;	/* Entry size if section holds table */
} Elf64_Shdr;
-}

data Shdr = Shdr {
    sh_name :: !Word32
    ,sh_type :: !Word32
    ,sh_flags :: !Word64
    ,sh_addr :: !Word64
    ,sh_offset :: !Word64
    ,sh_size :: !Word64
    ,sh_link :: !Word32
    ,sh_info :: !Word32
    ,sh_addralign :: !Word64
    ,sh_entsize :: !Word64

    } deriving Show

getShdrs :: Integral a => a -> Get [Shdr]
getShdrs num = do

    forM [1.. (fromIntegral num)] $ \_ ->

        Shdr <$> getWord32host <*> getWord32host <*>
            getWord64host <*> getWord64host <*> getWord64host <*> getWord64host <*>
            getWord32host <*> getWord32host <*>
            getWord64host <*> getWord64host


-- EOF
