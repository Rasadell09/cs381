-- Name: Yunfan Li
-- onid: liyunf@os-class.engr.oregonstate.edu
-- OSU ID: 932530195

module MiniLogo where

import Prelude hiding (EQ, Num)
import Data.List hiding (Num)
  
type Num = Int
type Var = String
type Marco = String

type Prog = [Cmd]

data Mode = Down
          | Up
          deriving Show

data Expr = LitV Var
          | LitN Num
          | Plus Expr Expr
          deriving Show

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Marco [Var] Prog
         | Call Marco [Expr]
         deriving Show

-- define line (x1, y1, x2, y2) {
--     pen up;
--     move (x1, y1);
--     pen down;
--     move (x2, y2);
-- }
line :: Cmd
line = Define "line" ["x1", "y1", "x2", "y2"]
       [Pen Up, Move (LitV "x1") (LitV "y1"), 
        Pen Down, Move (LitV "x2") (LitV "y2")]

-- define nix (x, y, w, h) {
--     call line (x, y, x+w, y+h);
--     call line (x+w, y, x, y+h);
-- }
nix :: Cmd
nix = Define "nix" ["x", "y", "w", "h"]
      [Call "line" [LitV "x", LitV "y", Plus (LitV "x") (LitV "w"), Plus (LitV "y") (LitV "h")],
       Call "line" [Plus (LitV "x") (LitV "w"), LitV "y", LitV "x", Plus (LitV "y") (LitV "h")]]

--steps function
steps :: Int -> Prog
steps 0 = [Pen Up, Move (LitN 0) (LitN 0), Pen Down]
steps x = (steps (x-1)) ++ [Move (LitN (x-1)) (LitN x), Move (LitN x) (LitN x)]

--marcos function
isMarcos :: Cmd -> Marco
isMarcos (Define name _ _) = name

marcos :: Prog -> [Marco]
marcos xs = map isMarcos xs

--pretty function
prtVarList :: [Var] -> String
prtVarList [x] = x
prtVarList (x:xs) = x++", "++(prtVarList xs)

prtExprList :: [Expr] -> String
prtExprList [x] = prtExpr x
prtExprList (x:xs) = (prtExpr x)++", "++(prtExprList xs)

prtExpr :: Expr -> String
prtExpr (LitV name) = name
prtExpr (LitN numb) = show numb
prtExpr (Plus aex bex) = (prtExpr aex)++"+"++(prtExpr bex)

prtCmd :: Cmd -> String
prtCmd (Pen Down) = "\tpen down;"
prtCmd (Pen Up) = "\tpen up;"
prtCmd (Move aex bex) = "\tmove ("++(prtExpr aex)++", "++(prtExpr bex)++");"
prtCmd (Define name params prog) = "define "++name++" ("++(prtVarList params)++") {\n"++(pretty prog)++"}"
prtCmd (Call name exprs) = "\tcall "++name++" ("++(prtExprList exprs)++");"

pretty :: Prog -> String
pretty [] = []
pretty (x:xs) =  (prtCmd x)++"\n"++(pretty xs)
