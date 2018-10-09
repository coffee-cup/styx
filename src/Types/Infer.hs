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
  { classes  :: Name -> Maybe Class
  , defaults :: [Type]
  }

-- | list of super classes
super :: ClassEnv -> Name -> [Name]
super ce i = case classes ce i of Just (is, its) -> is

-- | list of instances of a given class
insts :: ClassEnv -> Name -> [Inst]
insts ce i = case classes ce i of Just (is, its) -> its

defined :: Either a b -> Bool
defined (Right _) = True
defined (Left _)  = False

modifyEnv :: ClassEnv -> Name -> Class -> ClassEnv
modifyEnv ce i c = ce { classes = \j ->
                       if i == j
                       then Just c
                       else classes ce j }

initialClassEnv :: ClassEnv
initialClassEnv = ClassEnv
  { classes = \_ -> fail "class not defined"
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
  | isJust (classes ce name) = fail "class already defined"
  | any (not . isJust . classes ce) predNames = fail "superclass not defined"
  | otherwise                           = return (modifyEnv ce name (predNames, []))
  where
    predNames = map getName preds

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn i _) ce
  | not (isJust (classes ce i)) = fail "no class for instance"
  | any (overlap p) qs = fail "overlapping instance"
  | otherwise = return (modifyEnv ce i c)
    where its = insts ce i
          qs = [ q | (_ :=> q) <- its ]
          c = (super ce i, (ps :=> p) : its)

overlap :: Pred -> Pred -> Bool
overlap p q = defined $ runInfer emptyEnv (mguPred p q)

