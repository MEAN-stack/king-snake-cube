-- King snake cube solver
--
-- When we print a solution, it will be a list of steps
-- showing us how to convolve the snake by moving in the 
-- x/y/z direction
-- e.g. [+x,+y,+y,+y,-x,+z,+x,..] 

-- directions in three dimensions
data Direction = X | Y | Z | X_ | Y_ | Z_

instance Show Direction where
  show X  = "+x"
  show X_ = "-x"
  show Y  = "+y"
  show Y_ = "-y"
  show Z  = "+z"
  show Z_ = "-z"

-- snake is an array of 64 Ints
-- 0 if it is threaded straight threoug
-- 1 if it has a right-angled thread running through it
snake :: [Int]
snake =  [0,0,1,1,0,1,1,1,0,0,1,1,0,1,1,0,1,1,0,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,0,1,0,0,1,1,1,1,0,0,1,1,0,1,1,1,1,1,1,1,1,1,1,0,0,1,0]

-- A partial solution is a list of triples,
-- (x,y,z), (3-D coordinates within a 4x4x4 block)
-- some of which are occupied by a valid convolution of the snake
-- each new cubelet is added to the head of the list

-- we start with the first cubelet in the corner of out empty block
startingSolution :: [(Int,Int,Int)]
startingSolution = [(0,0,0)]

-- and we start by heading in a given direction
startingDirection :: Direction
startingDirection = X

-- this gives a different solution:
-- startingSolution = [(1,0,0)]
-- startingDirection = Y

-- validPartialSolutions is the (possibly empty) set of
-- partial solutions which are valid ways of placing the
-- next cubelet of our snake

-- If the cubelet currently occupying the list head in our
-- partial solution is threaded straight through, then there is
-- only one possible position for the next cubelet.
-- Otherwise there are four possible positions
validPartialSolutions :: [(Int,Int,Int)] -> [[(Int,Int,Int)]]
validPartialSolutions (x:xs)
    | cubelet == 0 {- keep going straight -} = if inBounds xs c then [(c:x:xs)] else []
    | otherwise {- up to four possibilities -} = map (:x:xs) cs
  where
    cs = filter (inBounds xs) $ changeDir dir x
    c = keepDir dir x
    cubelet = snake !! (length xs) -- the current cubelet occupying the head of the list
    dir = direction (x:xs)

-- return the next cubelet position going in the given direction
keepDir :: Direction -> (Int,Int,Int) -> (Int,Int,Int)
keepDir X  (x,y,z) = (x+1,y,z)
keepDir X_ (x,y,z) = (x-1,y,z)
keepDir Y  (x,y,z) = (x,y+1,z)
keepDir Y_ (x,y,z) = (x,y-1,z)
keepDir Z  (x,y,z) = (x,y,z+1)
keepDir Z_ (x,y,z) = (x,y,z-1)

-- return the list of four cubelet positions orthogonal to the given direction
changeDir :: Direction -> (Int,Int,Int) -> [(Int,Int,Int)]
changeDir X  (x,y,z) = [(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1)]
changeDir X_ (x,y,z) = [(x,y+1,z),(x,y-1,z),(x,y,z+1),(x,y,z-1)]
changeDir Y  (x,y,z) = [(x+1,y,z),(x-1,y,z),(x,y,z+1),(x,y,z-1)]
changeDir Y_ (x,y,z) = [(x+1,y,z),(x-1,y,z),(x,y,z+1),(x,y,z-1)]
changeDir Z  (x,y,z) = [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z)]
changeDir Z_ (x,y,z) = [(x+1,y,z),(x-1,y,z),(x,y+1,z),(x,y-1,z)]

-- examine the positions of the first two elements in a partial solution
-- determine the current direction in which the snake is heading
direction :: [(Int, Int, Int)] -> Direction
direction [_] = startingDirection                -- starting solution
direction ps
    | (getX ultimate) > (getX penultimate) = X
    | (getX ultimate) < (getX penultimate) = X_
    | (getY ultimate) > (getY penultimate) = Y
    | (getY ultimate) < (getY penultimate) = Y_
    | (getZ ultimate) > (getZ penultimate) = Z
    | (getZ ultimate) < (getZ penultimate) = Z_
  where
    ultimate = head ps
    penultimate = ps !! 1

-- get the x-coordinate of a triple
getX :: (Int,Int,Int) -> Int
getX (x,_,_) = x

-- get the y-coordinate of a triple
getY :: (Int,Int,Int) -> Int
getY (_,y,_) = y

-- get the z-coordinate of a triple
getZ :: (Int,Int,Int) -> Int
getZ (_,_,z) = z

-- return True if the triple represents a cell within our 4x4x4 block
inBounds :: [(Int,Int,Int)] -> (Int,Int,Int) -> Bool
inBounds xs (x,y,z) = (x `elem` [0..3]) &&
                   (y `elem` [0..3]) &&
                   (z `elem` [0..3]) &&
                   (x,y,z) `notElem` xs

solve :: [(Int,Int,Int)] -> [(Int,Int,Int)]
solve xs
    | (length xs) == (length snake) = xs                    -- solved it, return the solution
    | psols == []                   = []                    -- dead end, unwind
    | length psols == 1             = solve (head psols)    -- only one way forward, take it
    | validSols == []               = []
    | otherwise                     = head $ validSols

  where
    psols = validPartialSolutions xs
    sols = map solve psols
    validSols = filter (not . null) sols

direction' :: (Int, Int, Int) -> (Int,Int,Int) -> Direction
direction' a b = direction [b,a]

printResult r = print $ zipWith direction' r (tail r)

main = printResult $ reverse $ solve startingSolution
