type NonTerm = String

type Term = String

-- data RuleSymbols = NonTerm | Term

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
                                 then [Rule left ["#", "END"]]
                                 -- A -> a
                                 else let new_nonterm = createState left nonterms
                                      in [Rule left [head right, new_nonterm], 
                                          Rule new_nonterm ["#", "END"]]
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


simplifyRules :: [NonTerm] -> [Rule] -> [Rule]
simplifyRules nonterms rules = let splitted = splitRules nonterms rules
                                   simplified = innerSimplifyRules nonterms (fst splitted) 
                               in simplified
                               -- TODO epsilon rules


innerSimplifyRules :: [NonTerm] -> [Rule] -> [Rule]
innerSimplifyRules nonterms (rule:rules) = simplifyRule nonterms rule ++ innerSimplifyRules nonterms rules
innerSimplifyRules nonterms [] = []


-- TODO rename
findEpsilonRoute :: Nonterm -> [Rule] -> [Nonterm]


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
                                parse (rule : rules) terms = [left rule] ++ [nonterm | nonterm <- right rule, not (nonterm `elem` terms)] ++ parse rules terms
                                parse [] terms = []
                                deduplicate (x : xs) = if x `elem` xs
                                                       then deduplicate xs
                                                       else [x] ++ deduplicate xs
                                deduplicate [] = []


simplifyGrammar :: Grammar -> Grammar
simplifyGrammar (Grammar nonterms terms rules beg) = let simple_rules = simplifyRules nonterms rules
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


-- TODO end states
-- TODO transitions
convertGrammarToMachine :: Grammar -> Machine
convertGrammarToMachine (Grammar nonterms terms rules beg) = let mapping = createMapping nonterms
                                                                 states = [snd x | x <- mapping]
                                                                 transitions = [ruleToTransition rule mapping| rule <-rules] 
                                                                 start_state = findMappedNonTerm beg mapping
                                                                 end_states = [ snd x | x <- mapping, "END" == fst x]
                                                             in Machine states terms start_state transitions end_states


-- TODO check
-- TODO some function
ruleToTransition :: Rule -> [(NonTerm, State)] -> Transition
ruleToTransition (Rule left right) mapping = Transition (findMappedNonTerm left mapping)
                                                        (head right)
                                                        (findMappedNonTerm (last right) mapping)


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


-- TODO refactor
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

