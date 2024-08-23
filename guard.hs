import Control.Applicative

grd True = pure ()
grd False = empty

f = g [1..10]
    where
        g xs = do
            x <- xs

            grd (odd x)
            True <- return (odd x)

            return x
            
