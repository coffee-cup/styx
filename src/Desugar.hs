module Desugar where

import           Data.List (partition)
import           Data.Map  as Map hiding (foldl, partition)

import           Frontend
import           Name

-- | Groups related type and function declarations into single BindGroup
groupBindings :: Module -> Module
groupBindings (Module n decls) =
  let (tfDecls, other) = partition isTypeOrFunDecl decls
      groupedBindings = foldl createGroupBGs Map.empty tfDecls
      groupedFunDecls = fmap (FunDecl . snd) (Map.toList groupedBindings)
  in Module n $ other ++ groupedFunDecls

isTypeOrFunDecl :: Decl -> Bool
isTypeOrFunDecl (FunDecl _)    = True
isTypeOrFunDecl (TypeDecl _ _) = True
isTypeOrFunDecl _              = False

createGroupBGs :: Map.Map Name BindGroup -> Decl -> Map Name BindGroup
createGroupBGs m (FunDecl bg@(BindGroup name matches _)) =
  case Map.lookup name m of
    Just _ ->
      Map.adjust (\bg' -> bg' { _matchPats =
                                _matchPats bg' ++ matches }) name m
    Nothing ->
      Map.insert name bg m
createGroupBGs m (TypeDecl name sc) =
  case Map.lookup name m of
    Just _ ->
      Map.adjust (\bg -> bg { _matchScheme = Just sc}) name m

    Nothing ->
      Map.insert name (BindGroup name [] (Just sc)) m
createGroupBGs _ _ =
  error "cannot createGroupBGs for this type of declaration"
