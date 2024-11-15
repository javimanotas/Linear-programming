module Expression where


data Term = Term { coefficient :: Int
                 , variable :: String
                 } deriving (Eq, Ord)

instance Show Term where
    show (Term coef var) = show coef ++ var


newtype Expr = Expr [Term] deriving Eq

instance Show Expr where
    show (Expr e) = unwords $ map show e


data Equation = ExprEQ { expression :: Expr, target :: Int }
              | ExprLT { expression :: Expr, target :: Int }
              | ExprLET { expression :: Expr, target :: Int }
              | ExprGT { expression :: Expr, target :: Int }
              | ExprGET { expression :: Expr, target :: Int }
              deriving Eq

instance Show Equation where
    show equation = show (expression equation) ++ symbol ++ show (target equation)
        where
            symbol = case equation of
                ExprEQ expr n -> " = "
                ExprLT expr n -> " < "
                ExprLET expr n -> " <= "
                ExprGT expr n -> " > "
                ExprGET expr n -> " >= "