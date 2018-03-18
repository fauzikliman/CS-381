module HW2 where

import Prelude hiding(Num)

type Num = Int
type Var = String
type Macro = String

type Prog = [Cmd]

data Mode = Down
          | Up
  deriving(Eq, Show)

data Expr = Ref Var
          | Lit Num
          | Add Expr Expr
  deriving(Eq, Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving(Eq, Show)

-- define line (x1, y1, x2, y2) {
--   pen up; move (x1, y1);
--   pen down; move (x2, y2)
-- }
line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"] [
  Pen Up, Move (Ref "x1") (Ref "y1"), 
  Pen Down, Move (Ref "x2") (Ref "y2")]

-- define nix (x, y, w, h) {
--   call line (x, y, x + w, y + h);
--   call line (x, y + h, x + w, y);
-- }
nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"] [
  Call "line" [Ref "x", Ref "y", Add (Ref "x") (Ref "w"), Add (Ref "y") (Ref "h")],
  Call "line" [Ref "x", Add (Ref "y") (Ref "h"), Add (Ref "x") (Ref "w"), Ref "y"]]

steps :: Int -> Prog
steps 0 = [Pen Up, Move (Lit 0) (Lit 0), Pen Down]
steps n = steps (n - 1) ++ [Move (Lit (n - 1)) (Lit n),
                            Move (Lit n) (Lit n)]

macros :: Prog -> [Macro]
macros [] = []
macros ((Define m _ p):xs) = m : macros p ++ macros xs
macros (_:xs) = macros xs

pretty :: Prog -> String
pretty [] = ""
pretty (c:cs) = case c of (Pen m) -> prettyPen m ++ pretty cs
                          (Move l r) -> "move(" ++ prettyExpr l ++ ", " ++ prettyExpr r ++ ");\n" ++ pretty cs
                          (Define m vs p) -> "define " ++ m ++ " (" ++ prettyVars vs ++ ") {\n" ++ pretty p ++ "}\n" ++ pretty cs
                          (Call m es) -> "call " ++ m ++ " (" ++ prettyExprs es ++ ");\n" ++ pretty cs

prettyPen :: Mode -> String
prettyPen (Down) = "pen down;\n"
prettyPen (Up) = "pen up;\n"

prettyExpr :: Expr -> String
prettyExpr (Ref v) = v 
prettyExpr (Lit n) = show n
prettyExpr (Add l r) = (prettyExpr l) ++ "+" ++ (prettyExpr r)

prettyExprs :: [Expr] -> String
prettyExprs [] = ""
prettyExprs (x:[]) = prettyExpr x 
prettyExprs (x:xs) = prettyExpr x ++ ", " ++ prettyExprs xs

prettyVars :: [Var] -> String
prettyVars [] = ""
prettyVars (v:[]) = v
prettyVars (v:vs) = v ++ ", " ++ prettyVars vs

optE :: Expr -> Expr
optE (Lit n) = Lit n
optE (Ref v) = Ref v 
optE (Add l r) = case (l, r) of (Lit x, Lit y) -> Lit (x + y)
                                (Add _ _, Add _ _) -> optE (Add (optE l) (optE r))                                
                                (x, Add _ _) -> Add x (optE r)
                                (Add _ _, y) -> Add (optE l) y
                                _ -> Add l r

optP :: Prog -> Prog
optP [] = []
optP (c:cs) = case c of (Pen m) -> (Pen m) : optP cs
                        (Move le re) -> (Move (optE le) (optE re)) : optP cs
                        (Define m vs p) -> (Define m vs (optP p)) : optP cs
                        (Call m es) -> (Call m (map optE es)) : optP cs
                        