module Eval where

import qualified Data.Map as M
import Parseur


--------------------------------------------------------------------------------
-- Les expressions et la traduction de Sexp en expression (Exp)
--------------------------------------------------------------------------------

-- type a = b indique que a est un synonyme de b
-- il ne s'agit pas vraiment d'un nouveau type
type Type = Symbol
type Constructor = Symbol
type DataConstructor = (Constructor, [Type])

type CasePattern = (Symbol, [Symbol], Exp)

data Mutability = Constant
                | Mutable
                deriving (Eq, Show)

data Scope = Lexical
           | Dynamic
           deriving (Eq, Show)

-- Les expressions du langages ouf
-- Vous n'avez pas à modifier ce datatype
data Exp = EInt Int
         | EVar Symbol
         | EDefine Symbol Exp
         | EApp Exp Exp
         | ELam Symbol Exp
         | ESet Symbol Exp
         | ELet [(Symbol, Exp)] Exp
         | EData Type [DataConstructor]
         | ECase Exp [CasePattern]
         | EOufScope Symbol Scope
         | EOufMutability Symbol Mutability
         deriving (Eq, Show)

type Error = String

reservedKeywords :: [Symbol]
reservedKeywords = ["lambda", "let", "case", "data", "define", "set", "ouf", "Error"]


var2Exp :: Sexp -> Either Error Exp
var2Exp (SSym ident) = Right $ EVar ident
var2Exp _ = Left "Doit être un identificateur"

var2Symbol :: Sexp -> Either Error Symbol
var2Symbol (SSym ident) = Right ident
var2Symbol _ = Left "Doit être un identificateur"

-- Vous devez compléter une partie cette fonction
sexp2Exp :: Sexp -> Either Error Exp
sexp2Exp (SNum x) = Right $ EInt x
sexp2Exp (SSym ident) =
  if ident `elem` reservedKeywords
  then Left $ ident ++ " is a reserved keyword"
  else Right $ EVar ident
sexp2Exp (SList ls@((SSym s) : xs)) | s `elem` reservedKeywords
  = specialForm2Exp ls
sexp2Exp (SList (func : [])) =
  Left "Function application must provide at least one parameter"

-- Il faut écrire le cas pour les fonctions
sexp2Exp (SList (func : a1: args)) = do
  fexp <- sexp2Exp func
  a1exp <- sexp2Exp a1
  argsexp <- sequence $ map sexp2Exp args
  --Right $ processApp Params (EApp fexp a1 exp) argssexp
  return $ foldl (\args1 args2 -> (EApp args1 args2)) (EApp fexp a1exp) (argsexp)

sexp2Exp _ = Left "Erreur de syntaxe"

-- Il faut compléter cette fonction qui gère
-- toutes les formes spéciales (lambda, let ...)
specialForm2Exp :: [Sexp] -> Either Error Exp
specialForm2Exp ((SSym "lambda") :
                 (SList []) :
                 _ : []) = Left "Syntax Error : No parameter"

specialForm2Exp ((SSym "lambda") :
                 (SList params) :
                 body :
                 []) = do
  body' <- sexp2Exp body
  params' <- sequence  $ reverse $ map var2Symbol params
  return $ foldl (\b s -> ELam s b)
                 (ELam (head params') body')
                 (tail params')

specialForm2Exp((SSym "define") : sVar : sExp :[]) = do
  var' <- var2Symbol sVar
  exp' <- sexp2Exp sExp
  return $ EDefine var' exp'

specialForm2Exp ((SSym "let") : (SList params): body : []) =  do
  body' <- sexp2Exp body
  params' <- sequence $ map (\(SList(a:b:[])) -> do
    a' <- var2Symbol a
    b' <- sexp2Exp b
    return (a',b')) params
  return  (ELet params' body')

specialForm2Exp ((SSym "data") : types : constructor) = do
                                                        types' <- var2Symbol types
                                                        constructor' <- sequence $ map (\(SList x) -> do
                                                                                        x' <- sequence $ [var2Symbol a | a <- x]
                                                                                        --Remember that it is split into two cells 1 for types and the rest as cons
                                                                                        return ((head x'),(tail x')))
                                                                                        (constructor)

                                                        return (EData types' constructor')


specialForm2Exp ((SSym "case"):
                  caseName :
                  (SList branchList) :[]) = do
                                             caseName' <- var2Exp caseName
                                             branchList' <- sequence $ (map (\(SList ((SList cons): exp:[])) -> do
                                                                            exp' <- sexp2Exp exp
                                                                            name <- sequence $ map var2Symbol cons
                                                                            return ((head name), (tail name), exp')) branchList)

                                             return (ECase caseName' branchList')



specialForm2Exp _ = Left "Syntax Error : Unknown special form"



    -------------------------------------------------
-- L'évaluation
--------------------------------------------------------------------------------

-- Les valeurs retournées par l'évaluateur
-- Vous n'avez pas à modifier ce datatype
data Value = VInt Int
           | VLam Symbol Exp LexicalEnv
           | VPrim (Value -> Value)
           | VData Type Type [Value]
           | VUnit

instance Show Value where
  show (VInt n) = show n
  show (VData t c d) = "VData " ++ t ++ " (" ++
    (unwords $ show c : map show d) ++ ")"
  show VUnit = "VUnit"
  show (VPrim _) = "<primitive>"
  show (VLam s e env) = "VLamda [" ++ s ++ (unwords [",", show e, ",", show env])
    ++ "]"

instance Eq Value where
  (VInt n1) == (VInt n2) = n1 == n2
  VUnit == VUnit = True
  (VData t1 c1 d1) == (VData t2 c2 d2) =
     t1 == t2 && c1 == c2 && d1 == d2
  -- Functions and primitives are not comparable
  _ == _ = False

-- Un environnement pour portée lexicale
-- Vous n'avez pas à modifier ce datatype
type LexicalEnv = [(Symbol, Value)]

-- L'environnement. Au début, comme celui de la portée lexicale
-- Vous devrez modifier ce type pour la portée dynamique
-- et les instructions ouf
type Env = LexicalEnv

-- lookup de la librairie standard utilise Maybe
-- au lieu de Either
lookup2 :: [(Symbol, a)] -> Symbol -> Either Error a
lookup2 [] sym = Left $ "Not in scope " ++ sym
lookup2 ((s, v) : _) sym | s == sym = Right v
lookup2 (_ : xs) sym = lookup2 xs sym

-- Recherche un identificateur dans l'environnement
lookupVar :: Env -> Symbol -> Either Error Value
lookupVar = lookup2

-- Ajoute une variable dans l'environnement
insertVar :: Env -> Symbol -> Value -> Env
insertVar e s v =  (s, v) : e

-- Insert plusieurs variables dans l'environnement
-- La première variable de la liste est la dernière insérée
insertVars :: Env -> [(Symbol, Value)] -> Env
insertVars env xs = foldr (\(s, v) env -> insertVar env s v) env xs

primDef :: [(Symbol, Value)]
primDef = [("+", prim (+)),
           ("-", prim (-)),
           ("*", prim (*))]
  where prim op =
          VPrim (\ (VInt x) -> VPrim (\ (VInt y) -> VInt (x `op` y)))

envEmpty :: Env
envEmpty = []

env0 :: Env
env0 = insertVars envEmpty primDef


-- L'évaluateur au niveau global
-- L'évaluateur retourne une valeur et un environnement mis à jour
-- L'environnement mis à jour est utile pour de nouvelles définitions
-- avec define ou data ou lorsque les variables sont
-- modifiées par set par exemple.
evalGlobal :: Env -> Exp -> Either Error (Env, Value)
evalGlobal env (EDefine sym exp) = do
  (_, value) <- eval env exp
  return $ ((insertVar env sym value), value)
evalGlobal env (EData t cs) = Right $ ((insertVars env (map (\(x,s)-> (x, (VData t x (map (\y -> VUnit) s)))) cs)), VInt 0)
evalGlobal env e = eval env e -- Autre que Define et Data, eval prend le relais

-- L'évaluateur pour les expressions
eval :: Env -> Exp -> Either Error (Env, Value)
eval _ (EDefine _ _) = Left $ "Define must be a top level form"
eval _ (EData _ _) = Left $ "Data must be a top level form"
eval env (EInt x) = Right (env, VInt x)
eval env (EVar sym) = do
  v <- lookupVar env sym
  return (env, v)

eval env (ESet sym e) = Left "Vous devez compléter cette partie 1"

eval env (ELam sym body) =  Right $ (env, (VLam sym body env))



eval env (EApp func arg) = do
  (env' ,func') <- eval env func
  (env'',arg'') <- eval env arg
  case func' of
    VPrim fprim -> return $ (env, (fprim arg''))
    VLam symbol body lEnv -> do
      (_, result) <- eval (insertVar lEnv symbol arg'') body
      return (env, result)



    VData t1 t2 cons -> let check = (\unit -> if unit == VUnit then False else True)

                            first = filter check cons
                            (item:last) = filter ((\notUnit -> if notUnit == VUnit then True else False)) cons

                          --  (item:last) = let removeCommon xs ys = (filter (\x -> x `notElem` ys) xs)
                            --                  (item:last) = removeCommon (filter check cons) (cons)
                              --            in (item:last)

                        in return (env, (VData t1 t2 (first ++ (arg'':[]) ++ last)))
    _ -> Left "Error"


eval env (ELet decls e) = do
                           customEnv <- recursiveInsertion env decls
                           (_, val) <- eval customEnv e
                           return (env, val)

--  (_,symbol) <- eval  env (fst decls)
--(_ , exp)  <- eval  env (snd decls)

eval env (ECase e patterns) = do
                              (env1, valueA) <- eval env e
                              value <- caseMatchedValue env valueA patterns
                              return (env, value)

eval env (EOufScope sym scope) = Left "Vous devez compléter cette partie 6"

eval env (EOufMutability ident mutability) = Left "Vous devez compléter cette partie 7"


--Custom Function to extract tuples into a new Environment which exist outside of our current environment
recursiveInsertion :: Env -> [(Symbol, Exp)] -> Either Error Env
recursiveInsertion env [] = Right $ env
recursiveInsertion env ((sym,val):customEnv) = do
                                         (_, val') <- eval env val
                                         newEnv <- recursiveInsertion (insertVar env sym val' ) customEnv
                                         return newEnv

caseMatchedValue :: Env -> Value -> [CasePattern] -> Either Error Value
caseMatchedValue env (VData t1 t2 ls1) casePattern = do
                      (mainSym,lsSym,exp) <- testCases (VData t1 t2 ls1) casePattern
                      (env, v) <- eval (insertVars env (zip lsSym ls1)) exp
                      return v
caseMatchedValue env _ casePattern  = Left "Only data for case"




-- Exp pour valueA (le principale) et
testCases :: Value -> [CasePattern] -> Either Error CasePattern
testCases valueA (x:xs) = do
                         bool <- verify valueA x
                         case bool of
                              (True) -> Right x
                              (False) -> testCases valueA xs
testCases (VData _ dataType mainLs@(x:xs)) _ = Left "Erreur de match: aucun trouvee"




verify:: Value -> CasePattern ->  Either Error Bool
verify (VData _ dataType mainLs@(x:xs)) (comparedType,secLs,exp) = if dataType == comparedType
                                                          && (length secLs) == (length mainLs)
                                                          then Right True else Right False

verify (VData _ dataType []) (comparedType,[],exp) = if dataType == comparedType then Right True else Left (show dataType)

