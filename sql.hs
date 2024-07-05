import Control.Monad
import Control.Applicative

data Name = NewName { firstName::String, lastName::String }

instance Show Name where
    show (NewName f l) = mconcat [f, " ", l]

data Grade = Freshman | Sophomore | Junior | Senior deriving (Eq, Ord, Show, Enum)
data Student = NewStudent { studentId::Int, grade::Grade, studentName::Name } deriving Show

students::[Student]
students = [
    NewStudent 1 Senior (NewName "Audre" "Lorde"),
    NewStudent 2 Junior (NewName "Leslie" "Silko"),
    NewStudent 3 Freshman (NewName "Judith" "Butler"),
    NewStudent 4 Senior (NewName "Guy" "Debord"),
    NewStudent 5 Sophomore (NewName "Jean" "Budrillard"),
    NewStudent 6 Junior (NewName "Julia" "Kristeva") ]

_select::Monad m => (a -> b) -> m a -> m b
_select f xs = do
    x <- xs
    return $ f x

_where::(Monad m, Alternative m) => (a -> Bool) -> m a -> m a
_where f xs = do
    x <- xs
    guard (f x)
    return x

startsWith::Char -> String -> Bool
startsWith c cs = c == (head cs)

data Teacher = NewTeacher { teacherId::Int, teacherName::Name } deriving Show

teachers::[Teacher]
teachers = [
    NewTeacher 100 (NewName "Simone" "De Beauvior"),
    NewTeacher 200 (NewName "Susan" "Sotag") ]

data Cource = NewCource { courceId::Int, courceTitle::String, teacher::Int } deriving Show

cources::[Cource]
cources = [
    NewCource 101 "French" 100,
    NewCource 201 "English" 200 ]

_join::(Monad m, Alternative m, Eq c) => m a -> m b -> (a -> c) -> (b -> c) -> m (a,b)
_join xs ys fx fy = do
    x <- xs
    y <- ys
    guard (fx x == fy y)
    return (x, y)


sql1'::[Name]
sql1' = _select (teacherName . fst) $
    _where ((==) "English" . courceTitle . snd) $
    _join teachers cources teacherId teacher


hinq x z y = x . y $ z
-- hinq x z y = (\az -> (\ay -> x ay) (y az)) z

sql1::[Name]
sql1 = hinq
    (_select (teacherName . fst))
    (_join teachers cources teacherId teacher)
    (_where ((==) "English" . courceTitle . snd))

sql2::[String]
sql2 = hinq
    (_select (firstName . teacherName . fst))
    (_join teachers cources teacherId teacher)
    (_where (\x -> True))

data HINQ m a b = HINQ1 (m a -> m b) (m a) (m a -> m a) |
    HINQ2 (m a -> m b) (m a)

runHINQ (HINQ1 x y z) = hinq x y z
runHINQ (HINQ2 x y) = hinq x y (_where (\x -> True))

sql3 = runHINQ $ HINQ1
    (_select (teacherName . fst))
    (_join teachers cources teacherId teacher)
    (_where ((==) "English" . courceTitle . snd))

sql4 = runHINQ $ HINQ2
    (_select (firstName . teacherName . fst))
    (_join teachers cources teacherId teacher)

sql5 = runHINQ $ HINQ2
    (_select (firstName . teacherName . fst))
    (_join (Just (head teachers)) (Just (head cources)) teacherId teacher)



-- EOF --
