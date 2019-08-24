module Grammar
     where
import Token

data Var   =  Var  String  deriving(Show,Eq)

data Expr     =   SNode Operator Expr Expr
                | PNode Operator Expr Expr
                | UNode Operator Expr
                | NumNode Double
                | VNode Var
                | Terror
                | TPar Expr
                | T
                | F
                | BVar Var
                | BBOp Operator Expr  Expr
                | ROp  Operator Expr Expr
                | BOp  Operator Expr
                | FuncCall String [Parameters]
        deriving(Show,Eq)

data Stmt  =  AStmt String  Expr
           |  RtStmt  Expr
           |  IfStmt Expr [Stmt] 
           |  Function String  [Parameters ] [Stmt]
           |  CallStmt  Expr
        deriving(Show,Eq)
data Program = Program [Stmt] deriving (Eq,Show)
data Parameters = Ident String
                | Val Expr
             deriving(Show,Eq)

