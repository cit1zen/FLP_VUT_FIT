-- FLP 1st project - PLG-2-NKA - 2018
-- Adam Ormandy

import System.IO
import System.Environment
import Data.Char
import Data.List
import Data.List.Split
import Debug.Trace
import Data.Maybe
import System.Exit


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

simplifyRule :: [NonTerm] -> Rule -> [Rule]
simplifyRule nonterms (Rule left right) = 
  case length right of 1 -> if head right `elem` nonterms
                            -- Remove this one
                            then error "Rule in form A->B in simplifyRule"
                            -- A -> a OR A -> #
                            else if head right == "#"
                                 -- A -> #
                                 then [Rule left ["#"]]
                                 -- A -> a
                                 else let new_nonterm = createState left nonterms
                                      in [Rule left [head right, new_nonterm], 
                                          Rule new_nonterm ["#"]]
                       2 -> if last right `elem` nonterms
                            -- A->aB
                            then [Rule left right]
                            -- A->ab
                            else let new_nonterm = createState left nonterms
                                 in [Rule left [head right, new_nonterm]]
                                     ++ simplifyRule (nonterms ++ [new_nonterm]) (Rule new_nonterm (tail right))
                       -- TODO refactor
                       x -> do { new_nonterm <- [createState left nonterms]
                               ; [Rule left [head right, new_nonterm]]
                                 ++ simplifyRule (nonterms ++ [new_nonterm]) (Rule new_nonterm (tail right))
                               }


simplifyRules :: [NonTerm] -> [Term] -> [Rule] -> [Rule]
simplifyRules nonterms terms rules =
  let (not_simple, simple) = splitRules nonterms rules
      new_not_simple = innerSimplifyRules nonterms terms not_simple 
  in new_not_simple ++ removeEpsilonRules nonterms simple new_not_simple
    -- TODO epsilon rules


innerSimplifyRules :: [NonTerm] -> [Term] -> [Rule] -> [Rule]
innerSimplifyRules nonterms terms (rule:rules) = 
  let new_rules = simplifyRule nonterms rule
      -- Maybe new nonterms have been created
      new_nonterms = (getNonTerminals new_rules terms) ++ nonterms
  in new_rules ++ innerSimplifyRules new_nonterms terms rules
innerSimplifyRules nonterms terms [] = []


-- TODO rename
-- prva lajna najde vsetky nove neterminali cez ktore sa vieme dostat cez epsilon
-- TODO refactor this bitch
-- TODO what if A ->* A
-- TODO test this one
-- Returns all the nonterminal reachable by 
findEpsilonSomething :: NonTerm -> [NonTerm] -> [Rule] -> [NonTerm]
findEpsilonSomething nonterm found_epsilon epsilon_rules = let epsilon_nonterms = [last (right r) |
                                                                                   r <- epsilon_rules,
                                                                                   (left r == nonterm) && (not (last (right r) `elem` found_epsilon))]
                                                               new_found_epsilon = found_epsilon ++ epsilon_nonterms
                                                               nonterms = concat [findEpsilonSomething new_nonterm new_found_epsilon epsilon_rules | 
                                                                                  new_nonterm <- epsilon_nonterms]
                                                               -- Deduplication because I am so random
                                                               -- TODO special function
                                                               deduplicate (x : xs) = if x `elem` xs
                                                                                      then deduplicate xs
                                                                                      else [x] ++ deduplicate xs
                                                               deduplicate [] = []
                                                           in deduplicate (nonterms ++ new_found_epsilon)


-- TODO what if epsilon rule is there, some error shit
-- Removes epsilon rule
removeEpsilonRule :: NonTerm -> [NonTerm] -> [Rule] -> [Rule]
removeEpsilonRule left_side reach_epsilon normal_rules = [(Rule left_side (right r)) |
                                                          r <- normal_rules,
                                                          (left r) `elem` reach_epsilon]


-- TODO
removeEpsilonRules :: [NonTerm] -> [Rule] -> [Rule] -> [Rule]
removeEpsilonRules (nonterm:rest) epsilon_rules normal_rules = removeEpsilonRule nonterm (findEpsilonSomething nonterm [] epsilon_rules) normal_rules
                                                               ++ removeEpsilonRules rest epsilon_rules normal_rules
removeEpsilonRules [] epsilon_rules normal_rules = []


-- ([Rule], [Rule])
-- 1st = A->a, A->aB, A->#, ...
-- 2nd = A->B
splitRules :: [NonTerm] -> [Rule] -> ([Rule], [Rule])
splitRules nonterms (rule:rules) = if ((length (right rule)) == 1) && (last (right rule) `elem` nonterms)
                                   then let result = splitRules nonterms rules
                                        in (fst result, snd result ++ [rule])
                                   else let result = splitRules nonterms rules
                                        in (fst result ++ [rule], snd result)
splitRules nonterms [] = ([], [])


getNonTerminals :: [Rule] -> [Term] -> [NonTerm]
getNonTerminals rules terms = deduplicate (parse rules terms)
                              where
                                parse (rule : rules) terms = [left rule] ++ [nonterm | nonterm <- right rule, not (nonterm `elem` (["#"] ++ terms))] ++ parse rules terms
                                parse [] terms = []
                                deduplicate (x : xs) = if x `elem` xs
                                                       then deduplicate xs
                                                       else [x] ++ deduplicate xs
                                deduplicate [] = []


simplifyGrammar :: Grammar -> Grammar
simplifyGrammar (Grammar nonterms terms rules beg) = let simple_rules = simplifyRules nonterms terms rules
                                                         new_nonterms = getNonTerminals simple_rules terms
                                                     in Grammar new_nonterms terms simple_rules beg


-- TODO S cez epsilon koncovy stav

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


convertGrammarToMachine :: Grammar -> Machine
convertGrammarToMachine (Grammar nonterms terms rules beg) = let mapping = createMapping nonterms
                                                                 states = [snd x | x <- mapping]
                                                                 transitions = convertRulesToTransitions rules mapping
                                                                 start_state = findMappedNonTerm beg mapping
                                                                 end_states = [findMappedNonTerm (left rule) mapping
                                                                               | rule <- rules, (length $ right rule) == 1]
                                                             in Machine states terms start_state transitions end_states


convertRulesToTransitions :: [Rule] -> [(NonTerm, State)] -> [Transition]
convertRulesToTransitions (rule : rules) mapping =
  if (length $ right rule) == 1
  then convertRulesToTransitions rules mapping
  else [ruleToTransition rule mapping] ++ (convertRulesToTransitions rules mapping)
  where ruleToTransition (Rule left right) mapping = Transition (findMappedNonTerm left mapping)
                                                     (head right)
                                                     (findMappedNonTerm (last right) mapping)
convertRulesToTransitions [] _ = []


findMappedNonTerm :: NonTerm -> [(NonTerm, State)] -> State
findMappedNonTerm nonterm (x : xs) = if fst x == nonterm
                                     then snd x
                                     else findMappedNonTerm nonterm xs
findMappedNonTerm nonterm [] = error "There is no mapping for nonterm."


createMapping :: [NonTerm] -> [(NonTerm, State)]
createMapping nonterms = function nonterms 1
                         where
                          function (x : xs) i = [(x, i)] ++ function xs (i + 1)
                          function [] i = []


createState :: NonTerm -> [NonTerm] -> NonTerm
createState nonterm states = 
  if nonterm `elem` states
    then customState nonterm states 1
    else nonterm
      where
        customState nonterm states suffix = 
          if (nonterm ++ show suffix) `elem` states
            then customState nonterm states (1 + suffix)
            else nonterm ++ show suffix


-- TODO check input
-- TODO test everything

-- #MAIN
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
                 1 -> printGrammar $ simplifyGrammar grammar
                 2 -> printMachine $ convertGrammarToMachine $ simplifyGrammar grammar
  return ()


-- Parse input arguments
parseArgs :: [String] -> (Integer, String)
parseArgs [] = (2, [])
parseArgs [x]
-- Grammar will be at stdin
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


-- Print Grammar at stdout
printGrammar :: Grammar -> IO ()
printGrammar grammar = do putStrLn (intercalate "," (nonterms grammar))
                          putStrLn (intercalate "," (terms grammar))
                          putStrLn (beg grammar)
                          mapM_ putStrLn [(left r) ++ "->" ++ (intercalate "" (right r)) | r <- rules grammar]


-- Print Machine at stdout  
printMachine :: Machine -> IO ()
printMachine (Machine states _ start_state transitions end_states) =
  do putStrLn (intercalate "," [show s | s <- states])
     putStrLn $ show start_state
     putStrLn (intercalate "," [show s | s <- end_states])
     mapM_ putStrLn [(show $ origin t)
                     ++ ","
                     ++ (show $ symbol t)
                     ++ ","
                     ++ (show $ dest t) 
                     | t <- transitions]
