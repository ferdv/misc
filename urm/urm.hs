-- A simple URM machine simulator.
-- Based on the CS-275 module @ Swansea University
-- 
-- Written by Ferdinand Vesely <csfvesely@swansea.ac.uk> (see LICENSE for 
-- copyright details)

import qualified Data.Map.Strict as Map
import Data.List (stripPrefix)
import Control.Monad (liftM)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

-- Register references are just integers >= 0
type Reg = Integer

-- We only allow Integer values
type Value = Integer

-- The program counter, i.e., the pointer to the current instruction
type Pc = Integer

-- The 3 URM instructions
data Instr = Succ Reg
           | Pred Reg
           | IfZero Reg Pc
           deriving Show

-- A program is a list of instructions
type Prog = [Instr]

-- Registers are kept as a mapt from register references to values
type Regs = Map.Map Reg Value

-- The state of the machine consists of a program, the program counter and the 
-- current register state
type UrmState = (Prog, Pc, Regs)

-- Fetch the instruction from the program
fetchInstr :: Prog -> Pc -> Maybe Instr
fetchInstr [] _ = Nothing
fetchInstr (i : _) 0 = Just i
fetchInstr (_ : prog) n | n > 0 = fetchInstr prog (n - 1)

-- Get the value of a register
getReg :: Regs -> Reg -> Value
getReg regs reg = Map.findWithDefault 0 reg regs

-- Set the value of a register
setReg :: Regs -> Reg -> Value -> Regs
setReg regs reg val = Map.insert reg val regs

-- A step of the URM machine
urmStep :: UrmState -> Maybe UrmState
urmStep (prog, pc, regs) = do
  instr <- fetchInstr prog pc
  case instr of
    Succ r -> 
      let old = getReg regs r in
      return (prog, pc + 1, succReg regs r)
    Pred r ->
      return (prog, pc + 1, predReg regs r)
    IfZero r i ->
      let v = getReg regs r in
      if v == 0 then
        return (prog, i, regs)
      else
        return (prog, pc + 1, regs)
  where
    succReg :: Regs -> Reg -> Regs
    succReg regs reg =
      let v = getReg regs reg in
      Map.insert reg (v + 1) regs

    predReg :: Regs -> Reg -> Regs
    predReg regs reg = 
      let v = getReg regs reg in
      if v == 0 then
        regs
      else 
        Map.insert reg (v - 1) regs


-- Perform a full computation.
urmSteps :: UrmState -> UrmState
urmSteps state =
  let result = urmStep state in
  case result of
    Nothing -> state
    Just state' -> urmSteps state'

urmRun :: String -> [Value] -> UrmState
urmRun s args = 
  let prog = urmLines s;
      regs = Map.fromList $ zip [0 ..] args in
  urmSteps (prog, 0, regs)

urmRunFile :: String -> [Value] -> IO UrmState
urmRunFile fn args = do
  prog <- readFile fn
  state <- return $ urmRun prog args
  putStr $ showState state
  return state

urmExec :: String -> [Value] -> IO [Value]
urmExec file args = justRegsM $ urmRunFile file args

urmStepsIO :: UrmState -> IO UrmState
urmStepsIO state = do
  putStr $ showState state;
  putStr "Press Enter..."
  hFlush stdout
  getLine
  putChar '\n'
  result <- return $ urmStep state
  case result of 
    Nothing -> return state
    Just state' -> urmStepsIO state'

urmRunStepped :: String -> [Value] -> IO UrmState
urmRunStepped s args =
  let prog = urmLines s;
      regs = Map.fromList $ zip [0 ..] args in
  do 
    state <- urmStepsIO (prog, 0, regs)
    putStrLn "Finished."
    return state

urmRunFileStepped :: String -> [Value] -> IO UrmState
urmRunFileStepped fn args = do
  prog <- readFile fn
  urmRunStepped prog args

main = do
  args <- getArgs
  if null args then
    putStrLn "\
      \A simple URM machine simulator based on the CS-275 module.\n\
      \Usage: urm [--step] FILE reg0 reg1 ...\n\
      \\n\
      \Arguments:\n\
      \    --step\t\t(optional) pause after each step of the machine\n\
      \\n\    
      \    FILE\t\tURM program file\n\
      \    reg0 reg1 ...\tinitial values for registers 0, 1, etc.\n\
      \\n\
      \Example:\n\
      \  urm --step add.urm 12 13"
  else if head args == "--step" then do
    urmRunFileStepped (args!!1) (map read $ drop 2 args)
    return ()
  else do
    urmRunFile (head args) (map read $ tail args)
    return ()

justRegs :: UrmState -> [Value]
justRegs (_, _, regs) = (snd . unzip . Map.toList) regs

justRegsM :: Monad m => m UrmState -> m [Value]
justRegsM = liftM justRegs

-- Pretty-printing
-- (not very efficient)

showInstr :: Instr -> String
showInstr instr = 
  case instr of
    Succ r -> regOp r "+"
    Pred r -> regOp r ".-"
    IfZero r k -> "if " ++ showReg r ++ " = 0 then goto " ++ show k
  where regOp r op = showReg r ++ " := " ++ showReg r ++ " " ++ op ++ " 1"

showLine :: Pc -> Instr -> String
showLine n instr = show n ++ ":  " ++ showInstr instr

showProg :: Prog -> String
showProg prog = showProg' "" 0 prog
  where showProg' aux _ [] = aux
        showProg' aux n (instr : prog') = 
          showProg' (aux ++ showLine n instr ++ "\n") (n + 1) prog'

showProgH :: Pc -> Prog -> String
showProgH i prog = showProgH' "" 0 prog
  where showProgH' aux n [] = if i >= n then aux ++ "-> \n" else aux ++ "\n"
        showProgH' aux n (instr : prog') = 
          showProgH' (aux ++ showLine' n instr ++ "\n") (n + 1) prog'
        showLine' n instr =
          if (n == i) then
            "-> " ++ showLine n instr
          else
            "   " ++ showLine n instr

showReg :: Reg -> String
showReg r = 'R' : show r

showRegs :: Regs -> String
showRegs = foldRegs "" . Map.toList
  where foldRegs aux [] = aux
        foldRegs aux [rv] = aux ++ showReg' rv
        foldRegs aux (rv : regs) = foldRegs (aux ++ showReg' rv ++ "   ") regs
        
        showReg' (r, v) = showReg r ++ ": " ++ show v

showState :: UrmState -> String
showState (prog, pc, regs) = 
  "- URM State ----------------------------------\n"
  ++ "PC: " ++ show pc 
  ++ "\nRegisters:\n   " ++ showRegs regs 
  ++ "\nProgram:\n" ++ showProgH pc prog

------- Parsing

alt :: (a -> Maybe b) -> (a -> Maybe b) -> a -> Maybe b
alt f g a = 
  case f a of
  Nothing -> g a
  just_b -> just_b
infixl 0 `alt`

lit :: Char -> String -> Maybe String
lit _ [] = Nothing
lit c (h : rest) = 
  if h == c then 
    Just rest
  else
    Nothing

opt :: (String -> Maybe String) -> String -> Maybe String
opt f s = 
  case f s of
  Nothing -> Just s
  s' -> s'

lits :: String -> String -> Maybe String
lits = Data.List.stripPrefix

empty :: Monad m => a -> m a
empty = return

sptb, sptbnl  :: Monad m => String -> m String
sptb = return . dropWhile (`elem` " \t")
sptbnl = return . dropWhile (`elem` " \t\n")

returnWith :: Monad m => a -> String -> m (a, String)
returnWith a s = return (a, s)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(f >=> g) a =
  f a >>= g

urmLines :: String -> [Instr]
urmLines s = urmLines' 1 s
  where 
    urmLines _ "" = []
    urmLines' n s = do
      case urmLine n s of
        Just (instr, "") -> [instr]
        Just (instr, s') -> instr : urmLines' (n + 1) s'
        Nothing -> error $ "Parse error on line " ++ show n
  
urmLine :: Int -> String -> Maybe (Instr, String)
urmLine n s = do
  (num, s') <- label s
  (instr, s') <- instruction n s'
  opt sptbnl s' 
    >>= returnWith instr

label :: String -> Maybe (Integer, String)
label s = do
  (num, s') <- (sptb >=> natural) s
  (lit ':' >=> sptb) s' 
    >>= returnWith num

natural :: String -> Maybe (Integer, String)
natural s = do
  (intStr, s') <- digits s
  i <- return $ read intStr
  return (i, s')

instruction :: Int -> String -> Maybe (Instr, String)
instruction n = instrIfZero n `alt` instrRegUpdate n

instrIfZero :: Int -> String -> Maybe (Instr, String)
instrIfZero n s = do
    s' <- (lits "if " >=> sptb) s
    (reg, s') <- lit 'R' s' >>= natural
    s' <- (
      infixToken "=" 
      >=> zeroOrError 
      >=> infixToken "then " 
      >=> infixToken "goto ") s'
    (target, s') <- natural s'
    return (IfZero reg target, s')
  where
    zeroOrError = 
      lits "0 " `alt` (error $ "Line " ++ show n ++ ": if can only compare to 0")
  

instrRegUpdate :: Int -> String -> Maybe (Instr, String)
instrRegUpdate n s = do
    (reg1, s') <- register s
    s' <- infixToken ":=" s'
    (reg2, s') <- register s'
    checkRegsAreSame reg1 reg2
    (name, s') <-
      (infixToken "+" >=> returnWith Succ
      `alt` infixToken ".-" >=> returnWith Pred) s'
    s' <- lit '1' s'
    return (name reg1, s')
  where
    checkRegsAreSame r1 r2 = 
      if r1 /= r2 then
        error $ "Line " ++ show n ++ ": Registers R" ++ show r1 ++ " and R" ++ show r2 ++ " must be the same."
      else
        return ()


register :: String -> Maybe (Reg, String)
register = lit 'R' >=> natural

infixToken :: String -> String -> Maybe String
infixToken op = sptb >=> lits op >=> sptb

digits :: String -> Maybe (String, String)
digits s = 
  let ds = span (`elem` "0123456789") s in
  if null (fst ds) then
    Nothing
  else
    return ds

