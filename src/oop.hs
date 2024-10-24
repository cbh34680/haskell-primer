
newInstance a = (\f -> f a)

getData obj = obj (\a -> a)

f = getData $ newInstance 1
