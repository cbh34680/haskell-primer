


zero = \f -> \x -> x

succ' = \n -> \f -> \x -> f (n f x)

add' = \m -> \n -> \f -> \x -> m   f      (n f x)
mul' = \m -> \n -> \f -> \x -> m   (n f)  x


church0 = zero
church1 = succ' zero
church2 = succ' (succ' zero)
church3 = succ' (succ' (succ' zero))
church4 = succ' (succ' (succ' (succ' zero)))

churchToNatural f = f (+1) 0

natural0 = churchToNatural $ church0
natural1 = churchToNatural $ church1
natural2 = churchToNatural $ church2
natural3 = churchToNatural $ church3
natural4 = churchToNatural $ church4

--naturalN n = churchToNatural $ foldr ($) zero (replicate n succ')
naturalN n = churchToNatural $ (iterate succ' zero) !! n



test1 = churchToNatural $ add' church1 church3
{-
test1)
    add': \m -> \n -> \f -> \x -> m f (n f x)
    church1: \f -> \x -> f x
    church3: \f -> \x -> f (f (f x))


    \m -> \n -> \f -> \x -> m f (n f x)   \f -> \x -> f x   \f -> \x -> f (f (f x))
                            m=1, n=3

    \f -> \x -> (\f -> \x -> f x)  f  (  (\f -> \x -> f (f (f x)))  f  x  )
                ^^^^^^^^^^^^^^^^^        ^^^^^^^^^^^^^^^^^^^^^^^^^
                      m:1                           n:3

    \f -> \x -> (\f -> \x -> f x)  f  (  f (f (f x)))  )

    f  (  f (f (f x)))  )

    ==> church4
-}



test2 = churchToNatural $ mul' church2 church3
{-
test2)
    mul' \m -> \n -> \f -> \x -> m (n f) z
    church2: \f -> \x -> f (f x)
    church3: \f -> \x -> f (f (f x))


    \m -> \n -> \f -> \x -> m (n f) x   \f -> \x -> f (f x)   \f -> \x -> f (f (f x))
                            m=2, n=3

    \f -> \x -> (\f -> \x -> f (f x))   ( (\f -> \x -> f (f (f x)))   f )   x
                ^^^^^^^^^^^^^^^^^^^^^     ^^^^^^^^^^^^^^^^^^^^^^^^^
                        m:2                         n:3

    \f -> \x -> (\f -> \x -> f (f x))   ( \x -> f (f (f x)) )    x

    ( \x -> f (f (f x)) )  ( (f (f (f x)) ) )

    f (f (f  (f (f (f x)) )  )) )

    ==> church6

-}


true = \t -> \f -> t
false = \t -> \f -> f

if' = \p -> \t -> \f -> p t f

test3 = churchToNatural $ if' true church1 church2
test3' = churchToNatural $ if' false church3 church4
{-
test3)
    true  100 200
    false 300 400
    if' p t f

true)
    if' p t f:  \p -> \t -> \f -> p t f
    true t f: \t -> \f -> t

    (\p -> \t -> \f -> p t f)   (\t -> \f -> t)   100   200
        p: (\t -> \f -> t)
        t: 100
        f: 200

    (\t -> \f -> t)   100   200
        t: 100
        f: 200

    100


false)
    if' p t f:  \p -> \t -> \f -> p t f
    false t f: \t -> \f -> f

    (\p -> \t -> \f -> p t f)   (\t -> \f -> f)   300   400
        p: (\t -> \f -> f)
        t: 300
        f: 400

    (\t -> \f -> f)   300   400
        t: 300
        f: 400

    400

-}


and' = \b -> \c -> b c    false
or'  = \b -> \c -> b true c

test41 = churchToNatural $ and' true  true  church1 church0
test42 = churchToNatural $ and' true  false church1 church0
test43 = churchToNatural $ and' false false church1 church0
test44 = churchToNatural $ and' false true  church1 church0

test45 = churchToNatural $ or' true  true  church1 church0
test46 = churchToNatural $ or' true  false church1 church0
test47 = churchToNatural $ or' false false church1 church0
test48 = churchToNatural $ or' false true  church1 church0

{-
test4)

and' true true church1 church0

    and': \b -> \c -> b c false
    true: \t -> \f -> t
    false: \t -> \f -> f
    church1: \f -> \x -> f x
    church0: \f -> \x -> x


    (\b -> \c -> b c false)   (\t -> \f -> t)   (\t -> \f -> t)   (\f -> \x -> f x)   (\f -> \x -> x)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        b: (\t -> \f -> t)
        c: (\t -> \f -> t)

    ((\t -> \f -> t)  (\t -> \f -> t)  (\t -> \f -> f))   (\f -> \x -> f x)   (\f -> \x -> x)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    (\t -> \f -> t)   (\f -> \x -> f x)   (\f -> \x -> x)
    ^^^^^^^^^^^^^^^

    (\f -> \x -> f x)

    ==> church1


and' true false church1 church0

    (\b -> \c -> b c false)   (\t -> \f -> t)   (\t -> \f -> f)   (\f -> \x -> f x)   (\f -> \x -> x)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        b: (\t -> \f -> t)
        c: (\t -> \f -> f)

    ((\t -> \f -> t)   (\t -> \f -> f)   (\t -> \f -> f))   (\f -> \x -> f x)   (\f -> \x -> x)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    (\t -> \f -> f)   (\f -> \x -> f x)   (\f -> \x -> x)

    (\f -> \x -> x)

    ==> church0

-}


not' = \b -> false true

test51 = churchToNatural $      true church1 church0
test52 = churchToNatural $ not' true church1 church0

test53 = churchToNatural $       or' true true  church1 church0
test54 = churchToNatural $ not' (or' true true) church1 church0













-- EOF
