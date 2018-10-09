module Types.Infer where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.RWS.Lazy
import           Control.Monad.State
import qualified Data.Map               as Map
import           Data.Maybe
import qualified Data.Set               as Set

import           Frontend
import           Name
import           Types.Pred
import           Types.Subst
import           Types.Subst
import           Types.Type

-- Errors

data InferError
  -- | Two types that don't match were attempted to be unified
  = CannotUnify Type Type

  -- | A TVar is bound to a Type that already contains it
  | OccursCheckFailed Name Type

  -- | The value of an unknow identifier was read
  | UnknownIdentifier Name

  -- | Classes differ when unifying
  | ClassesDiffer
  deriving (Show)

-- Environmnet

newtype Env = Env (Map.Map Name Type)
  deriving (Show)

emptyEnv :: Env
emptyEnv = Env Map.empty

instance FreeVars Env where
  freeVars (Env env) = let allTypes = Map.elems env
                       in Set.unions (map freeVars allTypes)

-- Infer Monad

type Infer a = (ReaderT
               Env -- typing environment
               (StateT
               InferState --
               (Except
               InferError))
                 a)

data InferState = InferState
  { count    :: Int         -- ^ Name supply
  , preds    :: [Pred]      -- ^ Typeclass predicates
  , classEnv :: ClassEnv    -- ^ Class environment
  , subst    :: Subst       -- ^ Current substition
  }

runInfer :: Env -> Infer a -> Either InferError a
runInfer env m = runExcept $ evalStateT (runReaderT m env) initInfer

initInfer :: InferState
initInfer = InferState
  { count = 0
  , preds = []
  , classEnv = initialClassEnv
  , subst = mempty
  }

getSubst :: Infer Subst
getSubst = do
  st <- get
  return $ subst st

unify :: Type -> Type -> Infer ()
unify t1 t2 = do
  s <- getSubst
  u <- mgu (apply s t1) (apply s t2)
  extSubst u

extSubst :: Subst -> Infer ()
extSubst s = do
  s' <- getSubst
  modify (\st -> st { subst = s <> s' })

-- Unification

-- | Find the most general unifier
-- substition such that apply s t1 == apply s t2
mgu :: Type -> Type -> Infer Subst
mgu (TApp l r) (TApp l' r') = do
  s1 <- mgu l l'
  s2 <- mgu (apply s1 r) (apply s1 r')
  return (s2 <> s1)
mgu (TVar u) t = varBind u t
mgu t (TVar u) = varBind u t
mgu (TCon tc1) (TCon tc2)
  | tc1 == tc2   = return nullSubst
mgu t1 t2 = throwError $ CannotUnify t1 t2

-- | Unify a variable with a type
-- check that the variable is in the types set of type variables
varBind :: TVar -> Type -> Infer Subst
varBind u t | t == TVar u = return mempty
            | u `elem` ftv t = throwError $
                               OccursCheckFailed (getName u) t
            | otherwise = return (u +-> t)

-- | Find substitution such that apply s t1 == t2
-- "one-way match"
match :: Type -> Type -> Infer Subst
match (TApp l r) (TApp l' r') = do
  sl <- match l l'
  sr <- match r r'
  merge sl sr
match (TVar u) t
  -- | kind u == kind t = return (u +-> t)
  = return (u +-> t)
match (TCon tc1) (TCon tc2)
  | tc1 == tc2 = return nullSubst
match t1 t2 = throwError $ CannotUnify t1 t2

-- | Ensure that the two substituions agree at every variable
merge :: Subst -> Subst -> Infer Subst
merge subst1@(Subst s1) subst2@(Subst s2) =
  if agree then return $ Subst (Map.union s1 s2) else fail "merge fails"
  where agree = all (\v -> apply subst1 (TVar v) == apply subst2 (TVar v))
                tvars
        tvars = Map.keys $ Map.intersection s1 s2

mguPred, matchPred :: Pred -> Pred -> Infer Subst
mguPred = liftPred mgu
matchPred = liftPred match

liftPred :: (Type -> Type -> Infer a) -> Pred -> Pred -> Infer a
liftPred m (IsIn i t) (IsIn i' t')
  | i == i' = m t t'
  | otherwise = throwError ClassesDiffer

-- Classes

-- | (list of super classes, list of instances)
type Class = ([Name], [Inst])

-- | requirements for instance and predicate information
-- ex. [IsIn "Ord" "a", IsIn "Ord" "b"] :=> IsIn "Ord" ("a", "b")
type Inst = (Qual Pred)

data ClassEnv = ClassEnv
  { classes  :: Map.Map Name Class
  , defaults :: [Type]
  } deriving (Show)

-- | list of super classes
super :: ClassEnv -> Name -> [Name]
super ce i = case Map.lookup i (classes ce) of
  Just (is, its) -> is

-- | list of instances of a given class
insts :: ClassEnv -> Name -> [Inst]
insts ce i = case Map.lookup i (classes ce) of
  Just (is, its) -> its

defined :: Either a b -> Bool
defined (Right _) = True
defined (Left _)  = False

findClass :: ClassEnv -> Name -> Maybe Class
findClass ce n = Map.lookup n (classes ce)

modifyEnv :: ClassEnv -> Name -> Class -> ClassEnv
modifyEnv ce n c = ce { classes =
                        Map.insert n c (classes ce)}

initialClassEnv :: ClassEnv
initialClassEnv = ClassEnv
  { classes = Map.empty
  , defaults = [tyInt, tyDouble]
  }

type EnvTransformer = ClassEnv -> Infer ClassEnv

infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) ce = do
  ce' <- f ce
  g ce'

addClass :: ClassDecl -> EnvTransformer
addClass (CL preds name vars decls) ce
  | isJust (findClass ce name) = fail "class already defined"
  | any (not . isJust . findClass ce) predNames = fail "superclass not defined"
  | otherwise                           = return (modifyEnv ce name (predNames, []))
  where
    predNames = map getName preds

addModuleClasses :: Module -> Infer ClassEnv
addModuleClasses (Module _ decls) = envT initialClassEnv
  where
    decls' = [ d | d@ClassDecl{} <- decls ]
    cDecls = map (\(ClassDecl d) -> d) decls'
    envTs = map addClass cDecls
    envT = foldl1 (<:>) envTs

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
  | not (isJust (findClass ce i)) = fail "no class for instance"
  | any (overlap p) qs = fail "overlapping instance"
  | otherwise = return (modifyEnv ce i c)
    where its = insts ce i
          qs = [ q | (_ :=> q) <- its ]
          c = (super ce i, (ps :=> p) : its)

overlap :: Pred -> Pred -> Bool
overlap p q = defined $ runInfer emptyEnv (mguPred p q)

-- if predicate p holds, then so must all of the superclasses
-- bySuper returns a list of all predicates that must hold based only
--   on super class information (might be duplicates)
bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn i t)
  = p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

-- returns list of predicates that are applicable for given
--   predicate p by looking only at the instance information
-- we use matchPred because the type must be able to be unified from the
--   type in the p predicate
byInst :: ClassEnv -> Pred -> Infer [Pred]
byInst ce p@(IsIn i t) = do
  ps <- mapM tryInst (insts ce i)
  let ps' = join ps
  return ps'
  where
    tryInst :: Qual Pred -> Infer [Pred]
    tryInst (ps :=> h) = do u <- matchPred h p
                            let preds = map (apply u) ps
                            return preds

-- Given a particular class environment ce, the intention here is that
--   entail ce ps p will be True if, and only if, the predicate p will
--   hold whenever all of the predicates in ps are satisfied:
-- First check if p can be deduced by looking only at superclasses
--   next, look at matching instance and generate list of predicates qs
--   as a new goal, each of which must be entail with ps
entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
                 case runInfer emptyEnv $ byInst ce p of
                   Left _ -> False
                   Right qs -> all (entail ce ps) qs

-- head-normal form: lambda-term that has a top level
--   abstraction where the body does not have lambda sub-terms
--   that can be beta-reduced
--   http://barrywatson.se/lsi/lsi_head_normal_form.html
-- inHnf checks if a predicate is in head-normal form
inHnf :: Pred -> Bool
inHnf (IsIn c t) = hnf t
  where hnf (TVar v)   = True
        hnf (TCon tc)  = False
        hnf (TApp t _) = hnf t
        hnf (TArr t _) = hnf t

toHnfs :: ClassEnv -> [Pred] -> Infer [Pred]
toHnfs ce ps = do
  pss <- mapM (toHnf ce) ps
  return (concat pss)

toHnf :: ClassEnv -> Pred -> Infer [Pred]
toHnf ce p | inHnf p = return [p]
           | otherwise = do
               ps <- byInst ce p
               toHnfs ce ps

-- reduce list of predicates to simpiler, yet equal, list of predicates
-- we can reduce by building up a new list of predicates where each pred
--   in the list is not entailed by the new list
simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
  where loop rs [] = rs
        loop rs (p:ps) | entail ce (rs++ps) p = loop rs ps
                       | otherwise = loop (p:rs) ps

-- reduce a list of predicates by converting to HNF, then simplifying
reduce :: ClassEnv -> [Pred] -> Infer [Pred]
reduce ce ps = do
  qs <- toHnfs ce ps
  return (simplify ce qs)

-- version of simplify that makes use of only superclass entails
scEntail :: ClassEnv -> [Pred] -> Pred -> Bool
scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)
