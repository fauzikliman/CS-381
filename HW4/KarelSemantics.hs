module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState
import KarelExamples

-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t) w r = not (test t w r)
test (Facing c) _ r = c == getFacing r
test (Clear d) w r = isClear (relativePos d r) w
test Beeper w r = hasBeeper (getPos r) w
test Empty _ r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt Move _ w r = let p = relativePos Front r
                  in if isClear p w
                     then OK w (setPos p r)
                     else Error ("Blocked at: " ++ show p)
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper _ w r = let p = getPos r 
                       in if not (isEmpty r)
                          then OK (incBeeper p w) (decBag r)
                          else Error "No beeper to put."
stmt (Turn d) _ w r = OK w (setFacing (cardTurn d (getFacing r)) r)
stmt (Call m) defs w r = case lookup m defs of
                         Just s -> stmt s defs w r
                         Nothing -> Error ("Undefined macro: " ++ m)
stmt (Iterate i s) defs w r | i <= 0 = OK w r 
                            | otherwise = case stmt s defs w r of
                                          OK w' r' -> stmt (Iterate (i - 1) s) defs w' r'
                                          Error e -> Error e 
                                          Done r' -> Done r'
stmt (If t s1 s2) defs w r = if test t w r
                             then stmt s1 defs w r
                             else stmt s2 defs w r
stmt (While t s) defs w r = if test t w r
                            then case stmt s defs w r of
                                 OK w' r' -> stmt (While t s) defs w' r'
                                 Error e -> Error e
                                 Done r' -> Done r'
                            else OK w r
stmt (Block stmts) defs w r = case stmts of
                              [] -> OK w r
                              (s:ss) -> case stmt s defs w r of
                                        OK w' r' -> stmt (Block ss) defs w' r'
                                        Error e -> Error e
                                        Done r' -> Done r'
-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
