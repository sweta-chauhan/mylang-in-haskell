module Intepreter 
     where
import Token
import Tokenizer
import Parser
import Grammar
import Data.Fixed
import qualified Data.Map as M


type SymTab = M.Map String Double

lookUp :: String -> SymTab  -> (Double,SymTab)
lookUp str symtab =
            case M.lookup str symtab of
            Just v -> (v,symtab)
            Nothing -> error $ "Variable Not Defined" ++ str

addSymbol :: String -> Double ->  SymTab -> ((),SymTab)
addSymbol str val symtab =
                         let symtab' = M.insert str val symtab
                         in ((),symtab')

insertallParameters :: [Parameters] ->  [Parameters] -> SymTab -> SymTab
insertallParameters [] [] symt  =  symt
insertallParameters (p:ps) (p':ps') symt =  



chkMain :: Program -> Integer ->  (Integer,Bool)
chkMain (Program []) 0 = (-1,False)
chkMain (Program (Function name par body :ps)) n | name == "main" = (n,True)
                                                 | otherwise      = chkMain (Program ps) (n+1)

chkMain (Program (p:ps)) n  =  chkMain (Program ps) (n+1)
interpretate :: Program -> IO()
interpretate (Program [])  =  print $ "Succesfully Interpretated :) "
interpretate (Program ps)  | snd (chkMain (Program ps) 0) == True = print ps
                           | otherwise             =   print $ "Main is Not defined in program !!"


executeFunction  ::  Stmt  ->  SymTab -> [Parameters] -> IO()
executeFunction (Function name par (s:stmtl)) parameters symtab =  interPretate s symtab'
                                                         where symtab'= insertallParameters par parameters symtab



interPretate :: Stmt  ->  SymTab  ->  IO()
interPretate (RtStmt expr) symtab =  print $ fst (eval expr symtab)

eval  ::  Expr ->  SymTab  -> (Double,SymTab)


eval (VNode (Var x)) symtab =   lookUp x symtab
eval (NumNode x) symtab   =   (x,symtab)
eval (SNode op e e') symtab  = 
                  let (x1,symtab')= eval e symtab
                      (y1,symtab'') = eval e' symtab'
                  in 
                      case op of
                      Plus -> (x1 + y1,symtab'')
                      Minus -> (x1 - y1,symtab'')

