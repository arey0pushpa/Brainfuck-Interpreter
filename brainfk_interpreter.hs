import Test.HUnit
import Data.List (foldl')
import Debug.Trace
import Data.Array

type Byte = Int
data World = World { pc :: Int
                   , pointer :: Int
                   , program :: Array Int Instruction
                   , memory :: Array Int Byte
                   , output :: [Byte]
                   }

initWorld :: [Instruction] -> World
initWorld cmds = 
  World { pointer = 0
        , memory = memAry
        , program = progAry
        , pc = 0
        , output = [] }
 where 
   progAry = toInstructionArray cmds
   memAry = array (0, maxMem - 1) $ zip [0.. maxMem -1] (cycle [0])
maxMem = 30000
main = do
  _counts <- tests
  interact process

parseToArray = toInstructionArray . parse

toInstructionArray :: [Instruction] -> Array Int Instruction
toInstructionArray cmds = array (0, length cmds - 1) $ zip [0..] cmds

process = run . parse

data Instruction = IncP | DecP | IncB | DecB | OutB | InpB | JmpF | JmpB deriving (Show, Enum, Eq, Ord)

toInstruction :: Byte -> Instruction
toInstruction = toEnum

toByte :: Instruction -> Byte
toByte = fromEnum

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
           in traceShow cmds $ show $ reverse $ output $ w' 

run' :: World -> World
run' w = case nextInstruction w of
  Nothing -> w
  Just instr -> run' $ applyAndAdvance instr w

nextInstruction :: World -> Maybe Instruction
nextInstruction w | pc w > (snd $ bounds $ program w) = Nothing
                  | otherwise                         = Just (program w ! pc w)

applyAndAdvance instr w = pcChange $ apply instr w
  where
    pcChange = if handlesPC instr then id else incPC
    incPC w = w { pc = pc w + 1 }
    -- does the instruction take care of PC for itself or should it be advanced?
    handlesPC JmpF = True
    handlesPC JmpB = True
    handlesPC _ = False

apply IncP w = w { pointer = pointer w + 1 } 
apply DecP w = w { pointer = pointer w - 1 } 
apply IncB w = modByteAtPointer (+1) w
apply DecB w = modByteAtPointer ((-)1) w
apply OutB w = w { output = byteAtPointer w : output w }
apply InpB w = error "Not implemented Input Byte"
apply JmpF w = if byteAtPointer w == 0 then jumpForward w else w
apply JmpB w = if byteAtPointer w == 0 then jumpBackward w else w

jumpForward w = w { pc = pc' }
  where pc' = jf (pc w) (program w)

jf :: Int -> Array Int Instruction -> Int
jf pc arr = jump pc arr Forward
jb :: Int -> Array Int Instruction -> Int
jb pc arr = jump pc arr Back

data JumpDir = Back | Forward deriving (Eq)

lessAndMoreFor Forward = (JmpB, JmpF)
lessAndMoreFor Back = (JmpF, JmpB)

jump pc arr dir = jf' pc arr 0
  where
    (less, more) = lessAndMoreFor dir
    jf' pc arr depth | arr ! pc' == less = (if depth == 0 then pc'+1 else jf' pc' arr (depth-1))
                     | arr ! pc' == more = jf' pc' arr (depth + 1)
                     | otherwise         = jf' pc' arr depth
      where pc' = pc + 1

jumpBackward w = w { pc = pc' }
  where pc' = undefined

byteAtPointer w = memory w ! pointer w

modByteAtPointer :: (Byte -> Byte) -> World ->  World
modByteAtPointer op w = w { memory = memory w // [(pointer w, op (memory w ! pointer w))] }

-- TODO: add case when there is no program to run or at least when the program is one instruction long.
--
tests = runTestTT $ TestList [ jfTests]
jfTests ::  Test
jfTests = TestList [ "jfsimple"           ~: 9 ~=? jf 1 simpleInstrs
                   , "jfnested all"       ~: 12 ~=? jf 1 nestedInstrs 
                   , "jfnested two layer" ~: 10 ~=? jf 3 nestedInstrs 
                   , "tiny f" ~: 2 ~=? jf 0 tinyInstrs
                   , "tiny b" ~: 1 ~=? jb 2 tinyInstrs
                   ]--, "jbsimple" ~: 2 ~?= jf 8 instrs ]
  where 
    simpleInstrs  = parseToArray "+[>>--++]-"
    nestedInstrs  = parseToArray "+[>[>[-+]]-]+"
    tinyInstrs  = parseToArray "[]"

