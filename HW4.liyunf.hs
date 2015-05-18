-- | Name: Yunfan Li
--   onid: liyunf@onid.oregonstate.edu
--   OSU ID: 932530195

module HW4 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--   * MiniMiniLogo.hs has the definitions of Mode, Cmd, and Prog!
--   * Render.hs has the definitions of Point and Line!

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen m)    (s,p) = case m of
                        Down -> ((Down,p),Nothing)
                        Up   -> ((Up,p),Nothing)
cmd (Move x y) (s,p) = case s of
                        Down -> ((Down,(x,y)),Just (p,(x,y)))
                        Up   -> ((Up,(x,y)),Nothing)


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog []     (s,p) = ((s,p), [])
prog (x:xs) (s,p) = case (cmd x (s,p)) of
                     (m,Just l)  -> (fst (prog xs m), l:(snd (prog xs m)))
                     (m,Nothing) -> (fst (prog xs m), snd (prog xs m))
                     
--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing =  [Pen Up, Move 13 15, Pen Down, Move 15 10, Move 13 5, Pen Up, Move 15 15, Pen Down, Move 19 5, Pen Up, Move 17 10, Pen Down, Move 15 5, Pen Up, Move 17 12, Pen Down, Move 21 12, Pen Up, Move 19 8, Pen Down, Move 21 8]
