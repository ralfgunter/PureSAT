module Main where

import Data.List (nub, partition, delete)
import qualified Data.Map as Map
import Control.Arrow ((&&&))
import Control.Monad (liftM)

-- Command line arguments & DIMACS parsing.
import System.IO
import System (getArgs)

-- Strictly speaking, it's not a model until it satisfies the sentence, but...
data Assignment = T | F | U deriving (Show, Eq)
type Variable = Int
type Clause   = [Variable]
type Model    = Map.Map Variable Assignment


---- Partial logic.
-- Since DPLL evaluates formulas with incomplete models (i.e. those in which 
-- not all variables have had their truth value set), we need to implement the
-- basic logic operations.

-- Not.
neg :: Assignment -> Assignment
neg T = F
neg F = T
neg U = U -- the negation of an undetermined variable is just as undetermined.

-- And.
(>&<) :: Assignment -> Assignment -> Assignment
T >&< a = a
F >&< _ = F -- we implement shortcircuiting to avoid redundant evaluations.
U >&< T = U -- an undetermined clause might make a formula have undetermined evaluation...
U >&< F = F -- ... but a false clause always makes the entire thing false.
U >&< U = U

-- Or.
(>|<) :: Assignment -> Assignment -> Assignment
T >|< _ = T -- shortcircuiting for performance.
F >|< a = a
U >|< T = T -- a single true value in a clause is enough to make the entire clause true.
U >|< F = U
U >|< U = U

-- Used while finding pure symbols and unit clauses; we have to look at
-- undetermined clauses to find pure symbols. Finally, they may also be
-- unit clauses whose variable hasn't been set yet.
undeterminedClauses :: Model -> [Clause] -> [Clause]
undeterminedClauses model = filter (\clause -> evaluateClause model clause == U)

-- Used while looking for pure symbols and unit clauses; the DIMACS representation
-- of a positive literal is, well, positive, and same for negative literals.
assign :: Variable -> Assignment
assign var | var > 0   = T
           | otherwise = F


---- Pure symbols (from DPLL).
-- We check if the formula has both Var and -Var. Not exactly the most efficient way, but..
pureSymbols :: [Clause] -> [Variable]
pureSymbols formula = filter (\var -> (-var) `notElem` flatFormula) flatFormula
    where flatFormula = nub $ concat formula

-- WOULD REDO: rewrite this (and findUnitClauses) to be less redundant.
findPureSymbols :: [Clause] -> Model -> [(Variable, Assignment)]
findPureSymbols clauses model = map (abs &&& assign) pure
    where potentialClauses = undeterminedClauses model clauses
          -- A pure symbol is only considered pure if it hasn't been assigned yet.
          pure = filter ((== U) . evaluateVariable model) (pureSymbols potentialClauses)


---- Unit clauses (from DPLL).
-- Not sure if the terminology is correct; singletons should be one-element lists.
isSingleton :: [a] -> Bool
isSingleton = null . tail

-- WOULD REDO: rewrite this (and findPureSymbols) to be less redundant.
findUnitClauses :: [Clause] -> Model -> [(Variable, Assignment)]
findUnitClauses clauses model = map (abs &&& assign) units
    where potentialClauses = undeterminedClauses model clauses
          undeterminedSymbols = filter (\symbol -> evaluateVariable model symbol == U)
          -- Unit clauses 1) are undetermined, and 2) have only one undetermined symbol.
          units = concat $ filter isSingleton (map undeterminedSymbols potentialClauses)


---- Evaluation
-- Positive literals get the variable's truth value in the model, whereas
-- negative literals should get their negation.
evaluateVariable :: Model -> Variable -> Assignment
evaluateVariable model var | var > 0   = model Map.! var
                           | otherwise = neg $ model Map.! (-var)

-- We use foldr here to take advantage of shortcircuiting.
evaluateClause :: Model -> Clause -> Assignment
evaluateClause model = foldr ((>|<) . evaluateVariable model) F

evaluateFormula :: Model -> [Clause] -> Assignment
evaluateFormula model = foldr ((>&<) . evaluateClause model) T


---- Main algorithm (DPLL; see RN page 261).
-- The association of users of terminals with 80 columns would like to apologize for the mess that follows.
dpll :: [Clause] -> [Variable] -> Model -> (Bool, Model)
dpll clauses ss model | eval == T = (True, model)
                      | eval == F = (False, Map.empty)
                      | otherwise = let pures = findPureSymbols clauses model
                                    in  if not (null pures)
                                           then dpll clauses (delete (fst $ head pures) ss) (uncurry Map.insert (head pures) model)
                                           else let units = findUnitClauses clauses model
                                                in  if not (null units)
                                                       then dpll clauses (delete (fst $ head units) ss) (uncurry Map.insert (head units) model)
                                                       else chooseAssignment (dpll clauses (tail ss) (Map.insert (head ss) T model)) (dpll clauses (tail ss) (Map.insert (head ss) F model))
    where eval = evaluateFormula model clauses
          -- This surfaces possible variable assignments from the recursive
          -- bowels of DPLL.
          chooseAssignment (b, m) (b', m') = if b then (b, m) else (b', m') 


---- DIMACS parsing.
parseDIMACS :: [String] -> (Int, Int, [Variable], [Clause])
parseDIMACS string = (numVars, numClauses, symbols, clauses)
    where commentsStripped = filter (\line -> head line /= 'c') string
          (cnfOpts:_, clausesStr) = partition (\line -> head line == 'p') commentsStripped
          processClause = map read . init . words
          clauses = map processClause clausesStr
          symbols = nub $ map abs $ concat clauses
          [numVars, numClauses] = map read $ drop 2 $ words cnfOpts


---- Printing the result for our friendly graders!
printResult :: (Bool, Model) -> IO ()
printResult (False, _)    = putStrLn "UNSATISFIABLE"
printResult (True, model) = putStrLn (prettify $ Map.elems $ Map.mapWithKey reassign model)
    where reassign n T =  n
          reassign n F = -n
          reassign n U =  n
          prettify = unwords . map show


---- Well; main.
main = do filePath <- liftM head getArgs
          withFile filePath ReadMode
            (\h -> do (numVars, numClauses, symbols, clauses) <- liftM (parseDIMACS . lines) (hGetContents h)
                      let model  = Map.fromList $ zip symbols (replicate numVars U)
                      let result = dpll clauses symbols model
                      printResult result)
