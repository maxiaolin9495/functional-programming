module Exercise_14 where



data AExp = Cst Int | Var String | Sum AExp AExp | Prod [AExp]
  deriving (Eq, Show)

simplify (Sum a1 a2) = case (simplify a1, simplify a2) of
                            (Cst i1, Cst i2) -> Cst (i1 + i2)
                            (Cst 0, Var s) -> Var s
                            (Var s, Cst 0) -> Var s
                            (a11, a12) -> Sum a11 a12
