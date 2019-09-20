import CodeWorld

botCircle, midCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
midCircle c = colored c (translated 0 ( 0) (solidCircle 1))
topCircle c = colored c (translated 0 ( 2.5)  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 7

trafficLight :: Integer -> Picture
trafficLight 1  = botCircle green & midCircle black & topCircle black & frame
trafficLight 2  = botCircle green & midCircle black & topCircle black & frame
trafficLight 3  = botCircle black & midCircle yellow & topCircle black &frame
trafficLight 4 = botCircle black & midCircle black & topCircle red   & frame
trafficLight 5 = botCircle black & midCircle black & topCircle red   & frame

trafficController :: Double -> Picture
trafficController t
  | (round (t) :: Int) `mod` 5 == 0 = trafficLight 1
  | (round (t) :: Int) `mod` 5 == 1 = trafficLight 2
  | (round (t) :: Int) `mod` 5 == 2 = trafficLight 3
  | (round (t) :: Int) `mod` 5 == 3 = trafficLight 4
  | (round (t) :: Int) `mod` 5 == 4 = trafficLight 5

main :: IO()
main = animationOf trafficController