-- A,B
-- a,b,c
-- A
-- A->aaB
-- A->ccB
-- B->bB
-- B->#

type State = String

type Symbol = Char


type NonTerm = String

type Term = Char

-- data Transition = Transition
--   {beg_state::
--   }


-- createRules states term rules =

addState x states = do
  states <- states ++ [x]
  return states

addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b)  

createState :: NonTerm -> [State] -> State
createState nonterm states = 
  if nonterm `elem` states
    then customState nonterm states 1
    else nonterm
      where
        customState nonterm states suffix = 
          if (nonterm ++ show suffix) `elem` states
            then customState nonterm states (1 + suffix)
            else nonterm ++ show suffix

splitRules :: Term -> [String] -> [String]
splitRules x rules = [tail rule | rule <- rules, head rule == x]

getNonTerms :: [[String]] -> [String] -> [String]
getNonTerms (rule:rules) terms = [state | state <- rule, not (state `elem` terms)] ++ getNonTerms rules terms
getNonTerms [] terms = []

-- d :: [Term] -> [NonTerm] -> [[]]
-- d terms nonterms rules = 

-- TODO rules has to be sanitazed before using
-- simplifyRule :: NonTerm -> [b] -> [String] -> [String] -> [a]
simplifyRule beg rule nonterms terms =
  case length rule of 1 -> if head rule `elem` nonterms
                           then [(beg, "#", head rule)]
                           else [(beg, head rule, "END")]
                      2 -> if last rule `elem` terms
                           then [(beg, head rule, createState beg nonterms), (createState beg nonterms, last rule, "END")]
                           else [(beg, head rule, last rule)] 
                      x ->  concat [[(beg, head rule, createState beg nonterms)] ++ simplifyRule (createState beg nonterms) (tail rule) (nonterms ++ [createState beg nonterms]) terms]

-- simplifyGrammar :: [String] -> [String] -> [[String]] -> [a]
simplifyGrammar nonterm terms (rule:rules) = simplifyRule (head rule) (tail rule) nonterm terms ++ simplifyGrammar nonterm terms rules
simplifyGrammar nonterm terms [] = []

-- main = do  
--     putStrLn "Hello, what's your name?"  
--     name <- getLine  
--     putStrLn ("Hey " ++ name ++ ", you rock!")