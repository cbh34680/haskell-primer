

s f g x = (f x) (g x)
k x y = x
i x = x


{-
f (g x) -->
    S a b f g x
    (a f) (b f) g x
                        --> a f = S
                            a = KS      ... KSf -> S
    S (b f) g x
    (b f x) (g x)
                        --> b f x = f
                            b = K       ... K f x -> f

    S (KS)  K  f g x
      ^^^^  ^
       a    b
-}


{-
SKKz -->
    (\xyz -> xz (yz)) K K z
    \K K z -> K z (K z)
    z
    == Iz
-}
