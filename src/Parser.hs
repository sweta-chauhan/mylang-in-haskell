module Parser 
     where
import Grammar
import Tokenizer(tokenizer)
import Token

data ExpType  = SumN
              | Fact
              | Paren
              | Ot
        deriving(Show,Eq)

opTable =  [TokOperator Plus,TokOperator Minus,TokOperator Div,TokOperator Mull,TokOperator Gt,TokOperator Ge,TokOperator Lt,TokOperator Le,TokOperator Equ,TokOperator AND,TokOperator OR,TokOperator NOT]

merge :: [Expr] -> [Expr]

merge (e : (TPar Terror):xs)   = (TPar e):xs
merge ((e : (UNode x Terror) :xs)) = (UNode x e): xs
merge ((e : (SNode Plus x Terror ):xs))  = (SNode Plus x e):xs
merge ((e : (SNode Minus x Terror):xs)) =  (SNode Minus x e):xs
merge ((e : (PNode Mull x Terror ):xs))  = (PNode Mull x e) :xs
merge ((e : (PNode Div x Terror):xs)) = (PNode Div x e) : xs
merge ((e :(BBOp AND x Terror):xs))     = (BBOp AND x e) : xs
merge ((e :(BBOp OR x Terror):xs))     = (BBOp OR x e) : xs
merge ((e :(ROp Gt x Terror):xs))     = (ROp Gt x e) : xs
merge ((e :(ROp Ge x Terror):xs))     = (ROp Ge x e) : xs
merge ((e :(ROp Lt x Terror):xs))     = (ROp Lt x e) : xs
merge ((e :(ROp Le x Terror):xs))     = (ROp Le x e) : xs
merge ((e :(ROp Equ x Terror):xs))     = (ROp Equ x e) : xs
merge xs  = xs
--merge ((e :(BBOp Gt x Terror):xs))     = (BBOp NOT e) : xs

unmerge :: [Expr] -> (Expr,[Expr])

unmerge (SNode op e e': tree) = (e',(SNode op e Terror:tree))
unmerge (PNode op e e': tree) = (e',(PNode op e Terror: tree))


epattern :: Expr -> ExpType

epattern (SNode op e e')     = SumN
epattern (PNode op e e')     = Fact
epattern (TPar e)            = Paren
epattern _                   = Ot

isErrorFree :: Expr -> Bool

isErrorFree (SNode op e e')  |e'== Terror = False
                             | otherwise = True

isErrorFree (PNode op e e')  |e'== Terror = False
                             | otherwise = True

isErrorFree (TPar e')        | e' == Terror  = False
                             | otherwise = True
isErrorFree (FuncCall x plist) = True

isErrorFree (NumNode w)      = True
isErrorFree (UNode op Terror) = False
isErrorFree (UNode op _ )     = True

isErrorFree (BBOp op e e') | e' == Terror = False
                           |otherwise = True
isErrorFree (ROp op e e')  | e' ==  Terror = False 
                           | otherwise = True
isErrorFree (VNode (Var x))    = True

nextToken :: [Token] -> Token 
nextToken [] = TokEnd
nextToken (x:xs) = x

remain :: [Token] -> [Token]
remain [] = error $ "No more token remain to consume "
remain (x:xs) = xs

nextAst ::  [Expr]  ->   Expr
nextAst  [x]   =   Terror
nextAst  (a:ast) =  a

doMergeUntilLastparen   ::  [Expr]  ->  [Expr]
--doMergeUntilLastparen  [x]     =  [x]
doMergeUntilLastparen  (x:ast) 
                                 | epattern  x  == Paren = chkMerge (x:ast)
                                 | otherwise   =   doMergeUntilLastparen  (merge(x:ast))
chkMerge  ::  [Expr]   ->   [Expr]
chkMerge  [x]  =  [x]
chkMerge  (x:y:ast) | epattern y  == Fact   =  merge(x:y:ast)
                    | otherwise             =  (x:y:ast)
isUnary :: Operator -> Bool
isUnary x = elem x [Plus,Minus]

isOperator :: Token -> Bool
isOperator (TokOperator x) = elem x [Plus,Minus,Mull,Div]
isOperator _ = False

tokTo :: Token -> Expr
tokTo (TokNum x) = NumNode x

tokOp :: Token -> Operator
tokOp (TokOperator x) = x

unodeRemoval :: [Token] -> (Expr,[Token])
unodeRemoval [x] = error $ "Operator without operand"
unodeRemoval (x:x1:xs) | isOperator x1 = error $ " Operator after operator not allowed "
                       | otherwise = (UNode (tokOp x) (tokTo x1),xs)

parse :: [Token] -> Expr
parse []      = error "No Token is applied"      
parse tokens  = parse' tokens []

parse' :: [Token] -> [Expr]-> Expr

parse' [] (e:ast)                           | length (e:ast) == 1 = if isErrorFree e then e
                                              else error "Incomplete expression "
                                            | length (e:ast) == 2 = if isErrorFree e then head (merge (e:ast))
                                              else error "Incomplete expression " 
                                            | otherwise  = error "Invalid ArithExpession"

parse' [LineEnd] (e:ast)                    | length (e:ast) == 1 = if isErrorFree e then e
                                              else error "Incomplete expression "
                                            | length (e:ast) == 2 = if isErrorFree e then head (merge (e:ast))
                                              else error "Incomplete expression " 
                                            | otherwise  = error "Invalid ArithExpession"
parse' [TokCCurl] (e:ast)                    | length (e:ast) == 1 = if isErrorFree e then e
                                              else error "Incomplete expression "
                                            | length (e:ast) == 2 = if isErrorFree e then head (merge (e:ast))
                                              else error "Incomplete expression " 
                                            | otherwise  = error "Invalid ArithExpession"


parse' (TokNum x :  tokens) []             = parse' tokens (NumNode x:[]) 
parse' (TokIdentifier x :  tokens) []        |nextToken tokens == TokLparen = parseFuncCall  (TokIdentifier x:tokens) []
                                             | otherwise = parse' tokens (VNode (Var x):[]) 
                                             
parse' (TokLparen:tokens) []               | isOperator ((nextToken tokens))  = 
                                             if isUnary (tokOp(nextToken tokens))  
                                             then parse'  (snd(unodeRemoval tokens)) (fst(unodeRemoval tokens):(TPar Terror):[])
                                             else error "Invalid Expression"
                                           |otherwise  = parse' tokens (TPar Terror:[])

parse' (TokRparen:tokens) []               = error "Invalid ArithExpession: ')' without '(' before "

parse' (TokOperator op:tokens) []          | isUnary op = parse' tokens ((UNode op Terror):[]) 
                                           | otherwise = error " is Not a Unary operator "

parse' (TokNum x : tokens) (a:ast)         | nextToken tokens == TokLparen = error "Invalid ArithExpession"
                                           | epattern a == Paren = parse' tokens (NumNode x :a: ast) 
                                           |otherwise = parse' tokens (merge(NumNode x:a:ast))

parse' (TokIdentifier x : tokens) (a:ast)  | nextToken tokens == TokLparen = parseFuncCall (TokIdentifier x:tokens) (a:ast)
                                           | epattern a == Paren = parse' tokens (VNode (Var x) :a: ast) 
                                           | otherwise = parse' tokens (merge(VNode (Var x):a:ast))

parse' (TokOperator Plus : tokens) (a:ast) | isOperator ((nextToken tokens))  = 
                                             if isUnary (tokOp(nextToken tokens))  
                                             then 
                                             parse' (snd (unodeRemoval tokens)) (SNode Plus a (fst (unodeRemoval tokens)):ast) 
                                             else error "Invalid ArithExpession"
                                           | nextToken tokens == TokRparen = error "Invalid ArithExpession ')' after operator is not allowed"
                                           | otherwise  = parse' tokens ((SNode Plus a Terror):ast) 

parse' (TokOperator Minus : tokens) (a:ast) | isOperator ((nextToken tokens))  = 
                                            if isUnary (tokOp(nextToken tokens))  
                                            then 
                                            parse' (snd (unodeRemoval tokens)) (SNode Minus a (fst (unodeRemoval tokens)):ast) 
                                            else error "Invalid ArithExpession"
                                           | nextToken tokens == TokRparen = error "Invalid ArithExpession ')' after operator is not allowed"
                                           | otherwise  = parse' tokens ((SNode Minus a Terror):ast) 



parse' (TokOperator Mull : tokens) (a:ast) | isOperator ((nextToken tokens))  = 
                                               if isUnary (tokOp(nextToken tokens))
                                               then if epattern a == SumN 
                                                    then parse' (snd (unodeRemoval tokens)) (PNode Mull (fst(unmerge (a:ast))) (fst (unodeRemoval tokens)):snd(unmerge(a:ast)))
                                                    else parse' (snd (unodeRemoval tokens)) (PNode Mull a (fst (unodeRemoval tokens)) :ast)
                                               else error "Invalid ArithExpession" 
                                           | nextToken tokens == TokRparen = error "Invalid ArithExpession ')' after operator is not allowed" 
                                           | otherwise =
                                              if epattern a == SumN 
                                              then  parse'  tokens (PNode Mull (fst(unmerge (a:ast))) Terror :snd(unmerge(a:ast))) 
                                              else parse' tokens ((PNode Mull a Terror):ast) 

parse' (TokOperator Div : tokens) (a:ast) |  isOperator ((nextToken tokens))  = 
                                               if isUnary (tokOp(nextToken tokens))
                                               then if epattern a == SumN 
                                                    then parse' (snd (unodeRemoval tokens)) (PNode Div (fst(unmerge (a:ast))) (fst (unodeRemoval tokens)):snd(unmerge(a:ast)))
                                                    else parse' (snd (unodeRemoval tokens)) (PNode Div a (fst (unodeRemoval tokens)) :ast) 
                                               else error "Invalid ArithExpession" 
                                           | nextToken tokens == TokRparen = error "Invalid ArithExpession ')' after operator is not allowed" 
                                           | otherwise =
                                              if epattern a == SumN 
                                              then  parse'  tokens (PNode Div (fst(unmerge (a:ast))) Terror :snd(unmerge(a:ast))) 
                                              else parse' tokens ((PNode Div a Terror):ast) 

parse' (TokRparen : tokens) (a':ast) 
                                  |(length (a':ast) == 1) && isErrorFree (a')= a'
                                  |otherwise = parse' tokens (doMergeUntilLastparen(a':ast))


parse' (TokLparen : tokens) (a' : ast) | isOperator ((nextToken tokens))  = 
                                             if isUnary (tokOp(nextToken tokens))  
                                             then parse'  (snd(unodeRemoval tokens)) (fst(unodeRemoval tokens):(TPar Terror):a':ast)
                                             else error "Invalid Expression"
                                       |otherwise  = parse' tokens ((TPar Terror):a':ast)

parse' (TokOperator x : tokens) (a:ast)   | elem x [Gt,Lt,Ge,Le,Equ] = parse' tokens ((ROp x a Terror):ast)
                                          | x == NOT  =  parse' tokens (BOp x a:ast)
                                          | elem x [AND,OR] = parse' tokens ((BBOp x a Terror) :ast)
                                          | otherwise  =  error $ "Error !! Invalid Operator"

parseFuncCall :: [Token] ->[Expr]-> Expr

parseFuncCall [] (a:ast)  = a
parseFuncCall (TokRparen : tokens)  (a:ast)  =  parse' tokens (a:ast)
parseFuncCall (TokRparen : tokens)  []       =  Terror
parseFuncCall (TokIdentifier x : tokens) [] =   parseFuncCall tokens'  ((FuncCall x param) :[])
                                                where
                                                (param,tokens') = collectParam (tokens) []

parseFuncCall (TokIdentifier x : tokens) (a:ast) = parseFuncCall tokens'  (merge((FuncCall x param) : a : ast))
                                                   where
                                                   (param,tokens') = collectParam (tokens) []

parseFuncCall (TokLparen : tokens) ast = parseFuncCall tokens  ast
parseFuncCall (LineEnd: tokens) ast  =  parseFuncCall tokens ast

collectParam :: [Token] ->[Parameters] -> ([Parameters],[Token])
collectParam []  param  =   (param,[])
collectParam (TokLparen:tokens)  param  = collectParam tokens param
collectParam (TokRparen:tokens)  param =  (param,tokens)
collectParam (TokIdentifier x:tokens) param
                                           |  not (elem (nextToken tokens) opTable) =  (collectParam tokens (param++[Ident x]))
                                           |  nextToken tokens  ==  TokRparen   = (param++[Ident x],remain tokens)
                                           |  otherwise = collectParam ts'' (param++[para])
                                                                              where
                                                                              (ts',ts'') =  span (/=TokSep) (TokIdentifier x:tokens)
                                                                              para  =  Val (parse $  ts')
collectParam (TokSep:tokens)  param = collectParam tokens param

collectParam (TokNum x:tokens) param 
                                      | not $ elem (nextToken tokens) opTable =  collectParam tokens (param++[Val (NumNode x)])

                                      | otherwise=  collectParam ts'' (param++[para])
                                                                      where
                                                                      (ts',ts'') =  span (/=TokSep) (tokens)
                                                                      para  =  (Val (parse ts'))


isFuncDef :: [Token] -> Bool
isFuncDef []  =  False
isFuncDef (t:ts)
                |t == TokDef = True
                |t == LineEnd = False
                |otherwise  = isFuncDef ts

splitUpToCloseCurl :: [Token] ->  ([Token],[Token])
splitUpToCloseCurl ts = splitIt  ([],ts) 0  0

splitIt (h1,[]) openCount  closeCount
                                  | openCount == closeCount  && (closeCount /= 0) = (h1,[])
                                  | otherwise = error $ "Closed Curly Braces Missed"
splitIt (h1,t:ts) openCount  closeCount
                                   | t == TokOCurl  = splitIt  (h1++[t],ts)  (openCount+1) (closeCount)
                                   | t == TokCCurl  = splitIt  (h1++[t],ts)  (openCount) (closeCount+1)
                                   | openCount == closeCount  && (closeCount /= 0) = (h1++[LineEnd],ts)
                                   | otherwise  =  splitIt (h1++[t],ts) openCount closeCount

--collectOpenToClose :: [Token] ->  ([Token],[Token])
--collectOpenToClose

isInsideblock :: [Token] -> Bool
isInsideblock [] = False
isInsideblock (x:xs) 
                    | x == LineEnd = False
                    | x == TokOCurl = True
                    | otherwise = isInsideblock xs

parserMain  ::  String ->  Program

parserMain  str =  parseProgram (tokenizer str) (Program [])

parseProgram  :: [Token] -> Program -> Program
parseProgram [] prog = prog

parseProgram (TokIdentifier x:tokens) (Program stmt)  | isFuncDef tokens = parseProgram tokens' (Program (stmt++[s]))
                                                                                 where
                                                                                (tokens'',tokens') =splitUpToCloseCurl (TokIdentifier x:tokens)
                                                                                s = (parseStmt tokens'')
parseProgram (LineEnd:tokens) prog   =  parseProgram tokens prog

--parserMain  tokens'  =  parserMain'  tokens'  []

parseStmt  ::  [Token]  ->  Stmt

parseStmt ts   = parseStmt' ts []


parseStmt' [] [stmt] = stmt
parseStmt' [LineEnd] [stmt]  =  stmt
parseStmt' (LineEnd:ts) ast  =  parseStmt' ts ast

parseStmt' (TokIdentifier x:TokAssign:ts)  ast = parseStmt' ts'' (ast++[stmt])
                                                 where
                                                 (ts',ts'') =   span (/=LineEnd) ts
                                                 stmt= AStmt x (parse $ ts')
parseStmt' (TokOCurl:ts) ast  = parseStmt' ts ast

parseStmt' (TokReturn:ts)  ast | nextToken ts == LineEnd = parseStmt' ts  (RtStmt Terror:ast)
                               | ts == []     = parseStmt' ts (RtStmt Terror:ast)
                               | otherwise = parseStmt' ts'' (ast++[stmt])
                                             where
                                             (ts',ts'') =  span (/=LineEnd) ts 
                                             stmt = RtStmt (parse $ ts')
parseStmt' (TokRparen:ts) ast = parseStmt' ts ast
parseStmt' (TokIf:ts)  ast   =  parseStmt' ts''' (ast++[stmt])
                                where
                                (ts',ts'') = splitIf ts []
                                stmt = IfStmt (parse $ ts') (stmtl)
                                (stmtl,ts''') = collectStmt ts'' [] 
parseStmt' (TokIdentifier x:ts) ast |  isFuncDef ts   = parseStmt'  ts''  (ast++[Function x param stmt])
                                                        where
                                                        (param,ts') = collectParam ts []
                                                        (stmt,ts'') = collectStmt  ts' [] 

parseStmt' (TokCCurl:ts) ast  = parseStmt' ts ast
parseStmt' (TokDef:ts) ast  =  parseStmt' ts ast

splitSt  :: [Token] -> ([Token],[Token])
splitSt ts 
             | isLineCurlAdjacent ts =  span (/=TokCCurl) ts 
             | otherwise = span (/=LineEnd) ts

isLineCurlAdjacent :: [Token] -> Bool
isLineCurlAdjacent []  = False
isLineCurlAdjacent (x:xs) | x == LineEnd && ((nextToken xs) == TokOCurl || (nextToken xs) == TokCCurl) = True
                          | x == LineEnd && (nextToken xs) /=TokCCurl  = False
                          |otherwise = isLineCurlAdjacent xs

collectStmt :: [Token] -> [Stmt]  -> ([Stmt],[Token])
collectStmt  [] stmtlist  = (stmtlist,[])
collectStmt (TokCCurl:LineEnd:ts) stmtlist = if length ts > 0 then collectStmt ts stmtlist 
                                             else (stmtlist,ts)
collectStmt (TokOCurl:ts) stmtlist   = collectStmt ts stmtlist 
collectStmt (TokRparen:ts) stmtlist  = collectStmt ts stmtlist 
collectStmt (TokDef:ts) stmtlist     = collectStmt ts stmtlist
collectStmt  (LineEnd:ts) stmtlist  = collectStmt ts stmtlist 
collectStmt  (t:ts) stmtlist   = collectStmt (ts') (stmtlist++[stmt]) 
                                 where
                                 (ts'',ts') =   splitSt (t:ts)
                                 stmt = parseStmt ts''
                               
 
                               
--collectParameters :: [Token] -)

splitIf :: [Token] -> [Token]-> ([Token],[Token])
splitIf [] pa = (pa,[])
splitIf (x:xs) pa | x == LineEnd = (pa,xs)
                  |otherwise  = splitIf xs (pa++[x])


{-eval  :: Expr  ->  Double
eval (TPar e) = eval e
eval (NumNode x)        =  x
eval (SNode Plus e e')  =  eval e  + eval e'
eval (SNode Minus e e') =  eval e  - eval e'
eval (PNode Mull e e')  =  eval e  * eval e'
eval (PNode Div e e')   =  eval e  / eval e'
eval (UNode op e)       =  
                        case op of
                        Plus  ->  eval e
                        Minus ->  -(eval e)
-}
