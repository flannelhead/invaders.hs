import System.Console.ANSI

-- Generic Space Invaders model
data World = World { cannon :: Cannon
                   , cannonsLeft :: Int
                   , score :: Int
                   , aliens :: [Alien]
                   , bunkers :: [Bunker]
                   , alienBullets :: [AlienBullet]
                   , cannonBullets :: [CannonBullet] }

data Cannon = Cannon Point
data CannonBullet = CannonBullet Point Velocity

data Alien = Alien AlienBreed Point Velocity
data AlienBreed = Octopus | Crab | Medusa
data AlienBullet = AlienBullet Point Velocity

data Bunker = Bunker Point

-- Representation specific stuff begins

type Point = (Int, Int)
type Velocity = (Int, Int)

screenWidth = 80
screenHeight = 24

initialWorld = World { cannon = newCannon
                     , cannonsLeft = 2
                     , score = 0
                     , aliens = newAlienGroup
                     , bunkers = newBunkers
                     , alienBullets = []
                     , cannonBullets = [] }

newCannon = Cannon (0, screenHeight `div` 2)
newAlienGroup = []
newBunkers = []

cannonImage = [ "\\"
              , " >"
              , "/" ]
cannonColor = White

drawCannon :: Cannon -> IO ()
drawCannon (Cannon (x, y)) = do
    setCursorPosition (y - 2) x
    mapM_ drawCannonLine cannonImage
    where
        drawCannonLine :: String -> IO ()
        drawCannonLine line = do
            putStr line
            cursorDownLine 1
            setCursorColumn x

main = do
    clearScreen
    drawCannon (cannon initialWorld)

-- Auxiliary functions for drawing

putWithFgColor :: Color -> String -> IO ()
putWithFgColor color str = do
    setSGR [SetColor Foreground Vivid color]
    putStr str
    setSGR [Reset]

