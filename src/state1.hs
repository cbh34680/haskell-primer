-- hugs98

import Debug.Trace
import Control.Applicative

data MyState s a = GenMyState { runState :: s -> (a, s) }


instance Monad (MyState s) where

    return x = GenMyState $ \s -> (x, s)        -- クロージャの生成

    (GenMyState h) >>= f = GenMyState $ \s ->
        let (a, newState)  = h s                -- 値と状態を取り出す
            (GenMyState g) = f a                -- クロージャを取り出す
            in g newState                       -- クロージャを新しい状態で実行


push x = GenMyState $ \s -> ((), (x:s))
pop    = GenMyState $ \(x:s) -> (x, s)


f = runState (return 5 >>= push . (+2) ) [10..15]

{--
1) return 5         \s -> (5, s)
2) push . (+2)      \s -> ((), (((+2) 5): s))

                    \[10..15] -> (5, [10..15]) ==>> \[10..15] -> ((), 7:[10..15])

--}

-- EOF

