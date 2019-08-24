module Tokenizer
   where
import Token
import Data.Char

isOperator :: Char -> Bool
isOperator c = elem c "+-*/&|~"
isNum :: Char -> Bool
isNum x = elem x "0123456789."
char_to_Operator :: Char -> Operator
char_to_Operator x
                  | x == '+' = Plus
                  | x == '-' = Minus
                  | x == '*' = Mull
                  | x == '/' = Div
                  | x == '&' = AND
                  | x == '|' = OR
                  | x == '~' = NOT
                  | x == '>' = Gt
                  | x == '<' = Lt
isRel  :: Char  ->  Bool
isRel  x  = elem x  "><="

isIdentifier :: Char -> Bool
isIdentifier x =    not $ elem x "|1234567890+-={}()%/#$@^*!~`&:;, '\"\\/"

relOp ::  String    ->   Operator
relOp  x   
          |  x ==">=" =  Ge
          |  x =="<=" =  Le
          |  x == "==" = Equ
          |  otherwise  = error $ "Error !! Invalid operator" ++ show x


is_space :: Char -> Bool
is_space x = elem x "' '\n\t" 
tokenizer :: String -> [Token]
tokenizer "" = []
tokenizer (c:rest)
                 | isNum c = accumulate c rest
                 | isRel c =  findRelation (c:rest)
                 | isOperator c =   TokOperator (char_to_Operator c) : tokenizer rest
                 | c == '(' = TokLparen : tokenizer rest
                 | c == ')' = TokRparen : tokenizer rest
                 | is_space c = if  elem c "\n" then LineEnd:tokenizer rest else tokenizer rest
                 | c == ':'  = TokDef : tokenizer rest
                 | isIdentifier c = identifier c rest
                 | c  == '{' = TokOCurl : tokenizer rest
                 | c  == '}' = TokCCurl : tokenizer rest
                 | c  == ';'= TokColon  : tokenizer rest
                 | c == ',' = TokSep    : tokenizer rest
                 |  otherwise = error $ "Error Cann't tokenize "++[c]
skipit  ::  String  ->  [Token]
skipit []    =  []
skipit (x:xs) | x  == 'n' ||  x=='t' = tokenizer xs
              | otherwise  =  tokenizer xs

findRelation [s]        = (TokOperator (char_to_Operator s)) : (tokenizer [])
findRelation (s:s1:str) | isRel s1  = TokOperator (relOp (s:s1:[])): tokenizer str
                        | s == '='  = TokAssign : tokenizer (s1:str)
                        | otherwise  = (TokOperator (char_to_Operator s)) : tokenizer (s1:str)

accumulate :: Char -> String -> [Token]
accumulate c cs =
     let (digit,cs') = span isNum cs in
     TokNum ((read (c:digit))::Double): tokenizer cs'


identifier :: Char ->String -> [Token]
identifier c cs =  
     let (alpha,cs') = span isIdentifier cs in
         if  (c:alpha) == "if"  
         then TokIf :tokenizer cs'
         else  if  (c:alpha)  == "return"
               then  TokReturn : tokenizer cs'
               else if (c:alpha) == "True" 
                    then TokTrue:tokenizer cs'
                    else if (c:alpha) == "False"
                         then TokFalse: tokenizer cs'
                         else TokIdentifier (c:alpha):tokenizer cs'

