{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Pretty where

import           Data.List        (intercalate, intersperse)
import qualified Data.Map         as Map
import           Text.PrettyPrint

import CompilerError
import qualified Frontend         as S
import           Name
import           Types.Infer
import           Types.Kind
import           Types.Pred
import           Types.Scheme
import           Types.Subst
import           Types.Type

class Pretty p where
  ppr :: Int -> p -> Doc

  {-# INLINE pp #-}
  pp :: p -> Doc
  pp = ppr 0

  {-# INLINE ppg #-}
  ppg :: p -> String
  ppg = render . pp

instance Pretty Name where
  ppr _ (Name x)   = text x
  ppr _ (Gen nm i) = pp nm <> integer i

instance Pretty String where
  ppr _ x = text x

instance Pretty Int where
  ppr _ x = int x


-- Printer Utils

spaced :: Pretty a => Int -> [a] -> Doc
spaced p = hsep . fmap (ppr p)

parensIf :: Bool -> Doc -> Doc
parensIf True  = parens
parensIf False = id

tysig :: Pretty a => Name -> a -> Doc
tysig f ty = pp f <+> "::" <+> pp ty

spaces :: Int -> String
spaces n
  | n <= 0    = ""
  | otherwise = replicate n ' '

indent :: Int -> Doc -> Doc
indent i d = hang (text (spaces i) <> d) i empty

block :: Pretty a => [a] -> Doc
block xs =
  char '{'
  $$ nest 2 (vcat (punctuate semi (fmap pp xs)))
  $$ char '}'

commafy :: [Doc] -> Doc
commafy = hsep . punctuate comma

ppmaybe :: Pretty a => Maybe a -> Doc
ppmaybe = maybe empty pp

ppif :: Bool -> Doc -> Doc
ppif c d = if c then d else empty

banner :: String
banner = render $
  text (ascii ++
  " Styx 0.1.0\n")
  where
    ascii = "  ___ _            \r\n / __| |_ _  ___ __\r\n \\__ \\  _| || \\ \\ /\r\n |___/\\__|\\_, /_\\_\\\r\n          |__/     \r\n\n"

-- Compiler

instance Pretty CompilerError where
  ppr _ e = case e of
    FileNotFound fname -> "File:" <+> pp fname <+> " not found"
    ReplCommandError s -> pp s
    ParseError s -> pp s
    InferError e -> pp e

-- Frontend

ppexprs :: [S.Expr] -> Doc
ppexprs xs = nest 2 (vcat (fmap pp xs))

ppapp :: Int -> S.Expr -> Doc
ppapp p e = parensIf (p>0) $ ppr p f <+> args
  where
    (f, xs) = S.viewApp e
    args = sep $ fmap (ppr (p+1)) xs


instance Pretty S.Expr where
  ppr p ex = case ex of
    S.EVar x -> pp x

    S.ELit (S.LitInt x) -> integer x
    S.ELit (S.LitDouble x) -> double x
    S.ELit (S.LitChar x) -> quotes $ char x
    S.ELit (S.LitString x) -> doubleQuotes $ text x

    e@(S.EApp {}) -> ppapp p e
    e@(S.EInApp {}) -> ppapp p e
    e@(S.EPreApp e1 e2) ->
      ppr p e1 <> ppr p e2

    e@(S.ELam _ xs) ->
      parensIf (p>0) $ char '\\' <> hsep vars <+> "->" <+> body
      where
        body = ppexprs xs
        vars = fmap pp (S.viewVars e)

    S.EAss n e -> pp n <+> pp e

    S.EIf c t f ->
      hang (text "if" <+> pp c) 2
             (vcat [ hang (text "then") 2 (ppexprs t)
                   , hang (text "else") 2 (ppexprs f)
                   ])

    -- S.EAnn x ty -> parens $ pp x <+> ":" <+> pp ty

    S.EParens e -> parens (pp e)

instance Pretty S.Match where
  ppr p (S.Match lhs rhs) =
    sep (fmap (ppr p) lhs) <+> "->" <+> (ppexprs rhs)

instance Pretty S.Literal where
  ppr _ l = case l of
    S.LitInt x    -> integer x
    S.LitDouble x -> double x
    S.LitChar x   -> quotes $ char x
    S.LitString x -> doubleQuotes $ text x

instance Pretty S.Pattern where
  ppr p e = case e of
    S.PVar a -> ppr p a
    S.PLit a -> ppr p a
    S.PWild -> "_"
    S.PCon f xs ->
      let args = fmap (ppr (p+1)) xs in
      parensIf (length args > 0) $ ppr p f <+> sep args

instance Pretty [S.BindGroup] where
  ppr _ xs = vcat (fmap pp xs)

instance Pretty S.BindGroup where
  ppr p (S.BindGroup n pats sc) =
    (maybe empty (tysig n) sc)
    $+$
    vcat (fmap (prefix . ppMatch) pats)
    where
      prefix = (pp n <+>)

      -- toplevel matches use (=) instead of (->)
      ppMatch (S.Match lhs rhs) =
        sep (fmap (ppr p) lhs) <+> "=" <+> "=" <+> (ppexprs rhs)

instance Pretty S.Module where
  ppr p prg =
    ("module" <+> pp n)
    $$ vcat (intersperse "" (fmap pp decls))
    where
      (S.Module n decls) = S.groupTopLevel prg

instance Pretty S.Decl where
  ppr p decl = case decl of
    S.FunDecl b -> ppr p b
    S.TypeDecl n sc -> tysig n sc

    S.DataDecl (S.DTCL n vars cons) ->
      "type" <+> ppr p n <+> spaced p vars <+>
      "where" $+$ nest 2 (vcat (fmap pp cons))

    S.ClassDecl (S.CL preds n vars decls) ->
      "class" <+> ppcontext preds <+> ppr p n <+> spaced p vars <+>
      "where" $+$ nest 2 (vcat (fmap pp (S.groupTopLevelDecls decls)))

    S.InstDecl (S.INST preds n t decls) ->
      "instance" <+> ppcontext preds <+> ppr p n <+> pp t <+>
      "where" $+$ nest 2 (vcat (fmap pp decls))

instance Pretty S.ConDecl where
  ppr _ (S.ConDecl n xs) = pp n <+> sep (fmap pp xs)

ppexpr :: S.Expr -> String
ppexpr = ppg

ppsdecl :: S.Decl -> String
ppsdecl = ppg

ppmodule :: S.Module -> String
ppmodule = ppg

-- Types

isArrow :: Type -> Bool
isArrow TArr{} = True
isArrow _      = False

instance Pretty Type where
  ppr p ty = case ty of
    TArr a b ->
      (parensIf (isArrow a) (ppr p a)) <+> "->" <+> ppr p b

    TVar a -> ppr p a

    TCon a | a == unitTyCon -> "()"
    TCon a -> ppr p a

    TApp a b
      | a == tyList -> brackets (ppr p b)
    TApp (TApp a b) c
      | a == tyPair -> parens $ ppr p b <> char ',' <+> ppr p c
    TApp a b
      | isArrow b -> parensIf (p>0) $ ppr p a <+> parens (ppr (p+1) b)
      | otherwise -> parensIf (p>0) $ ppr p a <+> ppr (p+1) b

instance Pretty TVar where
  ppr _ (TV x) = pp x

instance Pretty TyCon where
  ppr _ (AlgTyCon a)  = pp a
  ppr _ (PrimTyCon a) = pp a

instance Pretty Pred where
  ppr p (IsIn n t) = pp n <+> ppr p t

instance Pretty t => Pretty (Qual t) where
  ppr p (ps :=> t) = ppcontext ps <+> pp t

instance Pretty Scheme where
  ppr p (Forall _ qual) = ppr p qual

instance Pretty Subst where
  ppr _ (Subst s) =
    "{" <+> vcat psubsts <+> "}"
    where
      psubsts =
        punctuate comma
        [ pp k <+> "-->" <+> pp v | (k, v) <- Map.toList s ]

instance Pretty Inst where
  ppr p (Inst q) = ppr p q

instance Pretty Class where
  ppr _ (Class n supers insts) =
    sep psupers <+> parrow <+> pp n <+> ppipe
    <+> nest 2 pinsts
    where
      psupers = punctuate comma (fmap pp supers)
      pinsts = vcat (fmap pp insts)
      parrow = ppif (length supers > 0) (text "=>")
      ppipe = ppif (length insts > 0) (text "|")

instance Pretty ClassEnv where
  ppr _ (ClassEnv classes _) = vcat asdf
    where
      asdf = [ pp c | (_, c) <- Map.toList classes ]

instance Pretty InferError where
  ppr _ e = case e of
    CannotUnify t1 t2 ->
      "Cannot unify" <+> pp t1 <+> "with" <+> pp t2
    OccursCheckFailed name t ->
      "Occurs check failed:" <+> pp name <+> "alraedy appears in"
            <+> pp t
    UnknownIdentifier name ->
      "Unknown identifier:" <+> pp name
    ClassNotDefined name ->
      "Class `" <> pp name <> "` is not defined"
    ClassAlreadyDefined name ->
      "Class `" <> pp name <> "` is already defined"
    OverlappingInstance name t ->
      "Instance for class `" <> pp name <> "` is already defined for type `" <> pp t <> "`"

ppcontext :: [Pred] -> Doc
ppcontext ps =
  if length ps > 0
  then pppreds ps <+> "=>"
  else empty

pppreds :: [Pred] -> Doc
pppreds []     = empty
pppreds [pred] = pp pred
pppreds ps     = parens (hcat (punctuate comma (fmap pp ps)))

pptype :: Type -> String
pptype = ppg

pptvar :: TVar -> String
pptvar = ppg

ppsignature :: (Name, Type) -> String
ppsignature (a, b) = render $ pp a <+> "::" <+> pp (pptype b)

ppksignature :: (Name, Kind) -> String
ppksignature (a, b) = render $ pp a <+> "::" <+> pp (ppkind b)

pppred :: Pred -> String
pppred = ppg

-- Kinds

isKArrow :: Kind -> Bool
isKArrow KArr{} = True
isKArrow _      = False

instance Pretty Kind where
  ppr p (KArr a b) =
    (parensIf (isKArrow a) (ppr p a)) <+> text "->" <+> ppr p b

  ppr _ (KStar)  = "*"
  ppr _ (KVar s) = pp s

ppkind :: Kind -> String
ppkind = ppg
