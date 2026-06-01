data Value = BVal Bool
           | IVal Int
           deriving (Show,Eq)

data Expr = Val Value 
          | Var String
          | Plus Expr Expr
          | Times Expr Expr 
          | If Expr Expr Expr 
          | And Expr Expr
          | Or Expr Expr
          | Not Expr 
          | Lt Expr Expr
          deriving (Show,Eq)

type Env = String -> Maybe Value

emptyEnv :: Env
emptyEnv _ = Nothing 

bind :: Env -> String -> Value -> Env 
bind env nm v nm' | nm == nm' = Just v
bind env nm v nm' | otherwise = env nm' 

lookupEnv :: Env -> String -> Maybe Value
lookupEnv env nm = env nm 

--Task - Write an evaluator for Expr

eval :: Env -> Expr -> Maybe Value
eval env ()