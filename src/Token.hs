module Token
   where
import Data.Char

data Operator = Plus
         | Minus
         | Mull
         | Div
         | Gt
         | Lt
         | Ge
         | Le
         | Equ
         | AND
         | OR
         | NOT
        deriving(Show,Eq)

data Token = TokOperator Operator
            |TokIdentifier String
            |TokNum Double
            |TokAssign
            |TokLparen
            |TokRparen
            |TokEnd
            |TokOCurl
            |TokCCurl
            |TokColon
            |TokIf
            |TokTrue
            |TokFalse
            |TokReturn
            |TokDef
            |LineEnd
            |TokSep
        deriving(Show,Eq)

