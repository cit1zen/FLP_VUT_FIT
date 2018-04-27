import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Debug.Trace


-- Data and Type definitions
-- because I am too lazy to put it into separate file

type NonTerm = String

type Term = String

data Rule = Rule { left :: NonTerm
                 , right :: [String]
                 } deriving (Show)

data Grammar = Grammar { nonterms :: [NonTerm]
                       , terms :: [Term]
                       , rules :: [Rule]
                       , beg :: NonTerm
                       } deriving (Show)

type State = Integer

type Symbol = String

data Transition = Transition { origin :: State
                             , symbol :: Symbol
                             , dest :: State
                             } deriving (Show)

data Machine = Machine { states :: [State]
                       , alphabet :: [Symbol]
                       , start_state :: State
                       , transitions :: [Transition]
                       , end_states :: [State] 
                       } deriving (Show)


-- MAIN and input parsing

-- # MAIN
main :: IO ()
main = do
  -- read arguments and parse arguments
  args <- getArgs 
  let (option, inputSource) = parseArgs args
  -- read input
  input <- fmap lines $ if (inputSource==[]) 
      then getContents 
      else readFile inputSource
  -- parse input into Grammar
  let grammar = parseInputGrammar input
  case option of 0 -> printGrammar grammar
                 1 -> printGrammar $ transformGrammar grammar
                 2 -> printMachine $ convertGrammarToMachine $ transformGrammar grammar
  return ()

-- Argument parsing
parseArgs :: [String] -> (Integer, String)
parseArgs [] = (2, [])
parseArgs [x]
-- Grammar will be passed using stdin
  | x=="-i" = (0, [])
  | x=="-1" = (1, [])
  | x=="-2" = (2, [])
  | otherwise = (2, [])
-- Grammar is in some file
parseArgs [x,y]
  | x=="-i" = (0, y)
  | x=="-1" = (1, y)
  | x=="-2" = (2, y)
  | otherwise = (2, y)
parseArgs _ = error "Something is wrong with the arguments."

-- Parse input into our data structure
parseInputGrammar :: [String] -> Grammar
parseInputGrammar (nonterms : terms : beg_nonterm : rules) =
  Grammar (splitOn "," nonterms)
          (splitOn "," terms)
          -- Parse rules
          [Rule (head dummy) [[x] | x <- (last dummy)]| dummy  <- [splitOn "->" r | r <- rules]]
          beg_nonterm


-- Grammar manipulation and simplification
-- Functions working with grammar

-- Generate new nonterm
createNonterm :: NonTerm -> [NonTerm] -> NonTerm
createNonterm nonterm all_nonterms = 
  if nonterm `elem` all_nonterms
  -- 2 is here instead of 1 to be similar to official output
  then creator (head nonterm) 2 all_nonterms
  else nonterm
  where
    creator base suffix nonterms =
      if ([base] ++ show suffix) `elem` nonterms
      then creator base (suffix + 1) nonterms
      else [base] ++ show suffix

-- Algorithm from TIN page 26 sentence 3.2
transformGrammar :: Grammar -> Grammar
transformGrammar (Grammar nonterms terms rules beg) =
  let new_rules = transformRules nonterms terms rules
      new_nonterms = sort $ getNonTerminals new_rules terms
  in Grammar new_nonterms terms new_rules beg

-- Transform all rules into A->aB or A-># form
transformRules :: [NonTerm] -> [Term] -> [Rule] -> [Rule]
transformRules nonterms terms rules =
  let (not_simple, simple) = splitRules nonterms rules
      new_not_simple = transformNotSimpleRules nonterms terms not_simple
  in new_not_simple ++ removeSimpleRules nonterms simple new_not_simple

-- Points 2 and 3 of the TIN algorithm 
transformNotSimpleRules :: [NonTerm] -> [Term] -> [Rule] -> [Rule]
transformNotSimpleRules nonterms terms (rule:rules) = 
  let new_rules = transformRule nonterms rule
      -- Maybe new nonterms have been created
      new_nonterms = (getNonTerminals new_rules terms) ++ nonterms
  in new_rules ++ transformNotSimpleRules new_nonterms terms rules
transformNotSimpleRules _ _ [] = []

-- Point 1 of the TIN algorithm
-- Split rules into 2 groups:
-- 1st = A->a, A->aB, A->#, ... 
-- 2nd = A->B a.k.a simple rules
splitRules :: [NonTerm] -> [Rule] -> ([Rule], [Rule])
splitRules nonterms (rule:rules) = 
  if ((length (right rule)) == 1) && (last (right rule) `elem` nonterms)
  then let result = splitRules nonterms rules
       in (fst result, snd result ++ [rule])
  else let result = splitRules nonterms rules
       in (fst result ++ [rule], snd result)
splitRules _ [] = ([], [])

-- Tranform rule like A->abC into A->aA1, A1->bC
transformRule :: [NonTerm] -> Rule -> [Rule]
transformRule nonterms (Rule left right) = 
  case length right of 1 -> if head right `elem` nonterms
                            -- Remove this one
                            then error "Rule in form A->B in transformRule"
                            -- A -> a OR A -> #
                            else if head right == "#"
                                 -- A -> #
                                 then [Rule left ["#"]]
                                 -- A -> a
                                 else let new_nonterm = createNonterm left nonterms
                                      in [Rule left [head right, new_nonterm], 
                                          Rule new_nonterm ["#"]]
                       2 -> if last right `elem` nonterms
                            -- A->aB
                            then [Rule left right]
                            -- A->ab
                            else let new_nonterm = createNonterm left nonterms
                                 in [Rule left [head right, new_nonterm]]
                                     ++ transformRule (nonterms ++ [new_nonterm]) (Rule new_nonterm (tail right))
                       -- A->aaaaB, ...
                       x -> let new_nonterm = createNonterm left nonterms
                            in [Rule left [head right, new_nonterm]] 
                               ++ transformRule (nonterms ++ [new_nonterm]) (Rule new_nonterm (tail right))

-- Print Grammar at stdout
printGrammar :: Grammar -> IO ()
printGrammar grammar = do putStrLn (intercalate "," (nonterms grammar))
                          putStrLn (intercalate "," (terms grammar))
                          putStrLn (beg grammar)
                          mapM_ putStrLn $ sort [(left r) ++ "->" ++ (intercalate "" (right r)) | r <- rules grammar]

-- All nonterms reachable by simple rules
reachableBySimple :: NonTerm -> [NonTerm] -> [Rule] -> [NonTerm]
reachableBySimple nonterm found_epsilon epsilon_rules = let epsilon_nonterms = [last (right r) |
                                                                                   r <- epsilon_rules,
                                                                                   (left r == nonterm) && (not (last (right r) `elem` found_epsilon))]
                                                            new_found_epsilon = found_epsilon ++ epsilon_nonterms
                                                            nonterms = concat [reachableBySimple new_nonterm new_found_epsilon epsilon_rules | 
                                                                                new_nonterm <- epsilon_nonterms]
                                                            -- Deduplication because I am so random
                                                            -- TODO special function
                                                            deduplicate (x : xs) = if x `elem` xs
                                                                                   then deduplicate xs
                                                                                   else [x] ++ deduplicate xs
                                                            deduplicate [] = []
                                                        in deduplicate (nonterms ++ new_found_epsilon)

-- Remove one particular epsilon rule
removeSimpleRule :: NonTerm -> [NonTerm] -> [Rule] -> [Rule]
removeSimpleRule left_side reach_epsilon normal_rules = [(Rule left_side (right r)) |
                                                          r <- normal_rules,
                                                          (left r) `elem` reach_epsilon]


-- Remove all simple rules 
removeSimpleRules :: [NonTerm] -> [Rule] -> [Rule] -> [Rule]
removeSimpleRules (nonterm:rest) epsilon_rules normal_rules = removeSimpleRule nonterm (reachableBySimple nonterm [] epsilon_rules) normal_rules
                                                               ++ removeSimpleRules rest epsilon_rules normal_rules
removeSimpleRules [] epsilon_rules normal_rules = []

-- Return all nonterminals
getNonTerminals :: [Rule] -> [Term] -> [NonTerm]
getNonTerminals rules terms = deduplicate (parse rules terms)
                              where
                                parse (rule : rules) terms = [left rule] ++ [nonterm | nonterm <- right rule, not (nonterm `elem` (["#"] ++ terms))] ++ parse rules terms
                                parse [] terms = []
                                deduplicate (x : xs) = if x `elem` xs
                                                       then deduplicate xs
                                                       else [x] ++ deduplicate xs
                                deduplicate [] = []


-- Grammar to machine conversion and Machine manipulation

-- Convert grammar processed by transformGrammar to Nondeterministic Finite Machine
convertGrammarToMachine :: Grammar -> Machine
convertGrammarToMachine (Grammar nonterms terms rules beg) = let mapping = createMapping nonterms
                                                                 states = [snd x | x <- mapping]
                                                                 transitions = convertRulesToTransitions rules mapping
                                                                 start_state = convertNonTermToState beg mapping
                                                                 end_states = [convertNonTermToState (left rule) mapping
                                                                               | rule <- rules, (length $ right rule) == 1]
                                                             in Machine states terms start_state transitions end_states

-- Convert grammar rules into machine transitions
-- A-># rules are skipped
convertRulesToTransitions :: [Rule] -> [(NonTerm, State)] -> [Transition]
convertRulesToTransitions (rule : rules) mapping =
  if (length $ right rule) == 1
  then convertRulesToTransitions rules mapping
  else [ruleToTransition rule mapping] ++ (convertRulesToTransitions rules mapping)
  where ruleToTransition (Rule left right) mapping = Transition (convertNonTermToState left mapping)
                                                     (head right)
                                                     (convertNonTermToState (last right) mapping)
convertRulesToTransitions [] _ = []

-- Convert grammar nonterm into machine state using provided mapping
convertNonTermToState :: NonTerm -> [(NonTerm, State)] -> State
convertNonTermToState nonterm (x : xs) =
  if fst x == nonterm
  then snd x
  else convertNonTermToState nonterm xs
convertNonTermToState nonterm [] = error "There is no mapping for nonterm."

-- Create nonterm to state mapping
createMapping :: [NonTerm] -> [(NonTerm, State)]
createMapping nonterms = function nonterms 1
                         where
                          function (x : xs) i = [(x, i)] ++ function xs (i + 1)
                          function [] i = []

-- Print Machine at stdout  
printMachine :: Machine -> IO ()
printMachine (Machine states _ start_state transitions end_states) =
  do putStrLn (intercalate "," [show s | s <- states])
     putStrLn $ show start_state
     putStrLn (intercalate "," [show s | s <- end_states])
     mapM_ putStrLn $ sort $ [(show $ origin t)
                     ++ ","
                     ++ symbol t
                     ++ ","
                     ++ (show $ dest t) 
                     | t <- transitions]
