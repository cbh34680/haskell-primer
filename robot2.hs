
newRobot (name, attach, hp) = \getter -> getter (name, attach, hp)

apply = \getter -> \robo -> robo getter

getName  = apply (\(name, _, _) -> name)
getAttach = apply (\(_, attach, _) -> attach)
getHP = apply (\(_, _, hp) -> hp)


setName name = apply (\(_, attach, hp) -> newRobot (name, attach, hp))
setAttach attach robo = robo (\(name, _, hp) -> newRobot (name, attach, hp))
setHP hp robo = robo (\(name, attach, _) -> newRobot (name, attach, hp))


printRobot :: Show a => ((a -> String) -> t) -> t
printRobot = apply (\t -> show t)

damage v robo = robo (\(name, attach, hp) -> newRobot (name, attach, hp - v))

