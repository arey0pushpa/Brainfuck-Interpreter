module Main where
import Data.Array
import Data.Char (chr)
import qualified Data.Map as M
import System.Environment (getArgs)
import Test.HUnit

type Byte = Int

data World = World { wpc     :: Int
                   , pointer :: Int
                   , program :: (Array Int Instruction)
                   , memory  :: (M.Map Int Byte)
                   , output  :: [Byte]
                   } deriving (Show)

initWorld :: [Instruction] -> World
initWorld cmds = 
  World { pointer = 0
        , memory  = memMap
        , program = progAry
        , wpc     = 0
        , output  = [] }
 where 
   progAry = toInstructionArray cmds
   memMap = M.fromList $ zip [0.. maxMem -1] (cycle [0])

maxMem ::  Int
maxMem = 30000

data Mode = ShowProg | RunProg deriving (Eq)

main ::  IO ()
main = do
  args <- getArgs
  let mode = case args of
              ["--show"] -> ShowProg
              []         -> RunProg
              _other     -> error "usage: prog [--show]"
  _counts <- tests
  let f = case mode of
            ShowProg -> parseAndShow
            RunProg  -> parseAndRun
  interact f

parseToArray ::  String -> Array Int Instruction
parseToArray = toInstructionArray . parse

toInstructionArray :: [Instruction] -> Array Int Instruction
toInstructionArray cmds = array (0, length cmds - 1) $ zip [0..] cmds

parseAndRun ::  String -> String
parseAndRun = run . parse
parseAndShow ::  String -> String
parseAndShow = unlines . map show . zip [(0::Int)..] . parse

data Instruction = IncP | DecP | IncB | DecB | OutB | InpB | JmpF | JmpB deriving (Show, Enum, Eq, Ord)

parse :: String -> [Instruction]
parse cs = map parseOne $ filter legalInstructionChar cs

parseOne :: Char -> Instruction
parseOne '>' = IncP
parseOne '<' = DecP
parseOne '+' = IncB
parseOne '-' = DecB
parseOne '.' = OutB
parseOne ',' = InpB
parseOne '[' = JmpF
parseOne ']' = JmpB
parseOne other = error $ "illegal character: " ++ [other]

legalInstructionChar :: Char -> Bool
legalInstructionChar = flip elem "><+-.,[]"

run :: [Instruction] -> String
run cmds = let w = initWorld cmds
               w' = run' w
           in map chr $ reverse $ output $ w' 

run' :: World -> World
run' w = case nextInstruction w of
  Nothing -> w
  Just instr -> run' next
    where next = applyAndAdvance instr w

nextInstruction :: World -> Maybe Instruction
nextInstruction w 
  | wpc w > lastInstrAddr = Nothing
  | otherwise             = Just (program w ! wpc w)
  where 
    lastInstrAddr = snd $ bounds $ program w

applyAndAdvance ::  Instruction -> World -> World
applyAndAdvance instr w = pcChange $ apply instr w
  where
    pcChange = if handlesPC instr then id else incPC
    -- does the instruction take care of PC for itself or should it be advanced?
    handlesPC JmpF = True
    handlesPC JmpB = True
    handlesPC _ = False

incPC ::  World -> World
incPC w = w { wpc = wpc w + 1 }
modPointer :: (Int -> Int) -> World -> World
modPointer op w = w { pointer = newVal }
  where newVal = op (pointer w)

apply ::  Instruction -> World -> World
apply IncP w = modPointer (+1) w
apply DecP w = modPointer (subtract 1) w
apply IncB w = modByteAtPointer (+1) w
apply DecB w = modByteAtPointer (+(-1)) w
apply OutB w = w { output = newVal }
  where newVal = byteAtPointer w : output w 
apply InpB _w = error "Not implemented Input Byte"
apply JmpF w = if byteAtPointer w == 0 then jumpForward w else incPC w
apply JmpB w = if byteAtPointer w /= 0 then jumpBackward w else incPC w

jumpBackward ::  World -> World
jumpBackward = modPC jb
jumpForward ::  World -> World
jumpForward = modPC jf

modPC ::  (Int -> Array Int Instruction -> Int) -> World -> World
modPC f w = w { wpc = pc' }
  where pc' = f (wpc w) (program w)

jf :: Int -> Array Int Instruction -> Int
jf pc arr = jump pc arr Forward
jb :: Int -> Array Int Instruction -> Int
jb pc arr = jump pc arr Back

data JumpDir = Back | Forward deriving (Eq)

jump :: Int -> Array Int Instruction -> JumpDir -> Int
jump progc arr dir = jf' progc 0
  where
    (less, more) = lessAndMoreFor dir
    inc = pcIncFor dir
    jf' :: Int -> Int -> Int
    jf' pc depth | bt == less   = (if depth == 0 then pc'+1 else jf' pc' (depth-1))
                 | bt == more   = jf' pc' (depth + 1)
                 | otherwise    = jf' pc' depth
      where
        bt = arr ! pc' 
        pc' = pc + inc
    lessAndMoreFor Forward = (JmpB, JmpF)
    lessAndMoreFor Back    = (JmpF, JmpB)
    pcIncFor Forward = 1
    pcIncFor Back    = -1


byteAtPointer ::  World -> Byte
byteAtPointer w = memory w M.! pointer w

modByteAtPointer :: (Byte -> Byte) -> World ->  World
modByteAtPointer op w = w { memory = newMem }
  where
    newMem = M.adjust (cap . op) (pointer w) (memory w)


cap :: Byte -> Byte
cap v | v < -128  = v + 256
      | v > 127   = v - 256
      | otherwise = v
-- [-128, +127]

-- TODO: add case when there is no program to run or at least when the program is one instruction long.
--
tests ::  IO Counts
tests = runTestTT $ TestList [ jfTests, capTests]
capTests :: Test
capTests = TestList [ 127  ~=? cap (-129)
                    , 126  ~=? cap (-130)
                    , -128 ~=? cap (-128)
                    , 127  ~=? cap 127
                    , -128 ~=? cap 128
                    , -127 ~=? cap 129
                    ]
jfTests ::  Test
jfTests = TestList [ "jfsimple"           ~:  9 ~=? jf 1 simpleInstrs
                   , "jfnested all"       ~: 12 ~=? jf 1 nestedInstrs 
                   , "jfnested two layer" ~: 10 ~=? jf 3 nestedInstrs 
                   , "tiny f"             ~:  2 ~=? jf 0 tinyInstrs

                   , "tiny b"       ~: 1 ~=? jb 1 tinyInstrs
                   , "jbsimple"     ~: 2 ~=? jb 8 simpleInstrs
                   , "jbnested all" ~: 2 ~=? jb 11 nestedInstrs
                   , "jbnested two" ~: 4 ~=? jb 9 nestedInstrs
                   ]
  where 
    simpleInstrs = parseToArray "+[>>--++]-"
    nestedInstrs = parseToArray "+[>[>[-+]]-]+"
    tinyInstrs   = parseToArray "[]"

