
newRobot (name, attach, hp) = \getter -> getter (name, attach, hp)

getName robo = robo (\(name, _, _) -> name)
getAttach robo = robo (\(_, attach, _) -> attach)
getHP robo = robo (\(_, _, hp) -> hp)


setName name robo = robo (\(_, attach, hp) -> newRobot (name, attach, hp))
setAttach attach robo = robo (\(name, _, hp) -> newRobot (name, attach, hp))
setHP hp robo = robo (\(name, attach, _) -> newRobot (name, attach, hp))

printRobot robo = robo (\t -> show t)

damage v robo = robo (\(name, attach, hp) -> newRobot (name, attach, hp - v))

fight attacher defender = damage (getAttach attacher) defender


main = do
    let a0 = newRobot ("a", 10, 100)
    let b0 = newRobot ("b", 5, 200)

    let a1 = fight b0 a0
    let b1 = fight a1 b0

    let a2 = fight b1 a1
    let b2 = fight a2 b1

    print $ printRobot a2
    print $ printRobot b2
