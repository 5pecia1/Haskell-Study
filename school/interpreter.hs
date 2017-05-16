module Semantics where

------ Syntax of While Language and Types --------------------------
type Var = String
data Aexp = N Int | V Var | Add Aexp Aexp | Mult Aexp Aexp | Sub Aexp Aexp 
           deriving (Eq, Show)

data Bexp = T | F | E Aexp Aexp | Le Aexp Aexp | Neg Bexp | And Bexp Bexp
           deriving (Eq, Show)

data Stm = Ass Var Aexp | Skip | Comp Stm Stm | If Bexp Stm Stm | While Bexp Stm
           deriving (Eq, Show)

type State = [(Var,Int)]

------- Evaluators -----------------------------------------------
eval :: Aexp -> Int
eval (N n)              = n
eval (Add a1 a2)        = eval a1 + eval a2
eval (Mult a1 a2)       = eval a1 * eval a2
eval (Sub a1 a2)        = eval a1 - eval a2

aEval :: Aexp -> State -> Int
aEval (N n) s           = n
aEval (V var) s         = case (lookup var s) of
                            Just a -> a
                            Nothing -> error "can't not found val"
aEval (Add a1 a2) s     = aEval a1 s + aEval a2 s
aEval (Mult a1 a2) s    = aEval a1 s * aEval a2 s
aEval (Sub a1 a2) s     = aEval a1 s - aEval a2 s

bEval :: Bexp -> State -> Bool
bEval T s               = True
bEval F s               = False
bEval (E a1 a2) s       = aEval a1 s == aEval a2 s
bEval (Le a1 a2) s      = aEval a1 s <= aEval a2 s
bEval (Neg b) s         = not $ bEval b s
bEval (And b1 b2) s     = bEval b1 s && bEval b2 s

evalStm :: Stm -> State -> State  
evalStm (Ass var aexp) s     = update var (aEval aexp s) s
evalStm (Comp stm1 stm2) s   = evalStm stm2 (evalStm stm1 s)
evalStm Skip s               = s
evalStm (If be stm1 stm2) s  = if bEval be s then evalStm stm1 s else evalStm stm2 s
evalStm (While bexp stm ) s  = if bEval bexp s 
                                 then evalStm (Comp stm (While bexp stm)) s
                                 else evalStm Skip s

------------ State Update -------------------------------------------
update ::  Var -> Int -> State -> State
update v n []     = [(v,n)]     -- add if it is not registered yet.
update v n ((v',n'):xs) 
      | v == v'     = (v,n):xs  -- change the value if it has been registered
      | otherwise   = (v',n') : update v n xs

--------- Test Data  -----------------------------------------------
state1 :: State
state1 = [("x", 10), ("y", 20), ("z", 30)]

aexp1 :: Aexp    -- 1 + ((x) + 3)
aexp1 =  Add (N 1) (Add (V "x") (N 3))

aexp2 :: Aexp  -- (2 + (x * 3)) - y
aexp2  =  Sub (Add (N 2) (Mult (V "x") (N 3))) (V "y")

bexp1 :: Bexp  -- !((x <= 2) & true)
bexp1 = Neg (And (Le (V "x") (N 2)) T)

stm1 :: Stm -- x := y + (3 * x)
stm1 =  Ass "x" (Add (V "y") (Mult (N 3) (V "x")))

stm2 :: Stm -- {z := x;  {x := y;  y := z}}
stm2 = Comp (Ass "z" (V "x")) (Comp (Ass "x" (V "y")) (Ass "y" (V "z")))

stm3 :: Stm -- {z := x;  x := y};  y := z
stm3 = Comp (Comp (Ass "z" (V "x")) (Ass "x" (V "y"))) (Ass "y" (V "z"))

stm4 :: Stm -- x := 1;  {y := 2; z := z + (x * y)}
stm4 = Comp (Ass "x" (N 1))
       (Comp (Ass "y" (N 2)) (Ass "z" (Add (V "z") (Mult (V "x") (V "y")))))

stm10 :: Stm  -- if !(x = y) then z := y else Skip;  y := 0
stm10 =  Comp (If (Neg (E (V "x") (V "y"))) (Ass "z" (V "y")) Skip) 
             ( Ass "y" (N 0))

fac :: Stm  -- x := 4;  y := 1;  while !(x=1) {y := y*x; x := x-1 }
fac = Comp (Ass "x" (N 4))   -- n = 4
         (Comp (Ass "y" (N 1)) 
           (While (Neg (E (V "x") (N 1)))
             (Comp (Ass "y" (Mult (V "y") (V "x")))
                   (Ass "x" (Sub (V "x") (N 1))))))

aexp10 = Add (Add (N 1) (Mult (V "x") (N 3))) (N 1)    -- (1 + (x * 3)) + 1
aexp20 :: Aexp     -- ((x*y2)-x)+10 
aexp20 = Add (Sub (Mult (V "z") (V "y2")) (V "x")) (N 10)    

bexp10 :: Bexp     -- (x*2) <= (y2-(x+1)) 
bexp10 = Le (Mult (V "x") (N 1)) (Sub (V "y2") (Add (V "x") (N 1)))

fac1 0 = 1
fac1 n = n  * fac1(n-1)
-- max2 
-- x ;= 10; y ;= 20; if (y < x) then max := x else max := y 
max2 :: Stm 
max2 = Comp (Comp (Ass "x" (N 10)) (Ass "y" (N 20))) (If (Le (V "y") (V "x")) (Ass "max" (V "x")) (Ass "max" (V "y")))
