module Gpwh.Lesson10Robot where

robot (name, attack, hp) = \message -> message (name, attack, hp)

name (n, _, _) = n
attack (_, a, _) = a
hp (_, _, hp) = hp

getName = ($ name)
getAttack = ($ attack)
getHp = ($ hp)

setName aRobot newName = aRobot (\(n,a,h) -> robot(newName,a,h))
setAttack aRobot newAttack = aRobot (\(n,a,h) -> robot(n,newAttack,h))
setHp aRobot newHp = aRobot (\(n,a,h) -> robot(n,a,newHp))

damage aRobot dmg = aRobot (\(n,a,h) -> robot (n,a,max 0 (h - dmg)))

fight aRobot defender = damage defender attack
    where attack = if getHp aRobot > 0 then getAttack aRobot else 0

printRobot aRobot = aRobot $ \(n,a,h) -> n ++ " attack:" ++ (show a) ++ " hp:" ++ (show h)

-----

bob = robot ("Bob", 10, 40)
roman = robot ("Roman", 4, 60)
