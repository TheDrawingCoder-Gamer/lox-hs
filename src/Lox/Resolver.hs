{-# LANGUAGE ConstraintKinds #-}
module Lox.Resolver where

import Lox.Types
import Polysemy
import Polysemy.State
import Polysemy.Fail
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Traversable (traverse)
import Data.Foldable    (traverse_)
import Data.List (uncons)
import Data.Maybe (maybe)
resolve :: ResolverMembers r => [LxStmt] -> Sem r [LxStmt]
resolve = traverse resolveStmt 
resolveStmt :: ResolverMembers r => LxStmt -> Sem r LxStmt
resolveStmt (LxBlock ss) = 
  LxBlock <$> (beginScope *> resolve ss <* endScope)
resolveStmt (LxVar name init) = do 
  declare name 
  rexpr <- case init of 
            Nothing -> pure Nothing
            Just re -> Just <$> resolveExpr re
  define name
  pure $ LxVar name rexpr
resolveStmt (LxFunDecl decl@(FunDecl name _)) = do 
  declare name 
  define name
  LxFunDecl <$> resolveFunction decl
resolveStmt (LxExprStmt expr) = LxExprStmt <$> resolveExpr expr
resolveStmt (LxIf cond thenBranch elseBranch) = LxIf <$> resolveExpr cond <*> resolveStmt thenBranch <*> case elseBranch of
  Nothing -> pure Nothing
  Just e  -> Just <$> resolveStmt e
resolveStmt (LxPrint expr) = LxPrint <$> resolveExpr expr
resolveStmt (LxReturn expr') = LxReturn <$> maybe (pure Nothing) (fmap Just . resolveExpr) expr'
resolveStmt (LxWhile cond body) = LxWhile <$> resolveExpr cond <*> resolveStmt body
resolveStmt (LxClassDecl{}) = fail "unimplemented"
resolveFunction :: ResolverMembers r => FunDecl -> Sem r FunDecl
resolveFunction (FunDecl' name args ss) = do 
  beginScope
  traverse_ (\arg -> declare arg *> define arg) args
  ss' <- resolve ss
  endScope
  pure $ FunDecl' name args ss'
resolveExpr :: ResolverMembers r => LxExpr -> Sem r LxExpr
resolveExpr expr@(LxIdent infos@(IdentInfo_ name)) = do 
  scope' <- currentScope
  case scope' of 
    Just scope -> 
      case HM.lookup name scope of 
        Just False -> fail "Can't read local variable in its own initializer"
        _ -> pure ()
    _ -> pure ()
  LxIdent <$> resolveLocal infos
resolveExpr(LxEAssign ident@(IdentInfo_ name) expr) = do 
  expr' <- resolveExpr expr
  LxEAssign <$> resolveLocal ident <*> pure expr'
resolveExpr (LxBinop lexpr rexpr t) = do 
  LxBinop <$> resolveExpr lexpr <*> resolveExpr rexpr <*> pure t
resolveExpr (LxCall callee args) = do 
  LxCall <$> resolveExpr callee <*> traverse resolveExpr args
resolveExpr (LxGroup expr) = LxGroup <$> resolveExpr expr
resolveExpr (LxLit lit) = pure $ LxLit lit
resolveExpr (LxUnary pf t rexpr ) = LxUnary pf t <$>resolveExpr rexpr
resolveLocal :: ResolverMembers r => IdentInfo -> Sem r IdentInfo
resolveLocal e@(IdentInfo_ name) = do 
  scopes <- get
  pure $ resolveRec scopes 0 
  where 
    resolveRec :: [Scope] -> Int -> IdentInfo
    resolveRec (x:xs) i =
      case HM.lookup name x of 
        Nothing -> resolveRec xs (i + 1)
        Just _  -> e { identDepth = Just i }
    resolveRec _ _ = e
      
      
      
      
beginScope :: ResolverMembers r => Sem r ()
beginScope = modify (HM.empty :)

endScope   :: ResolverMembers r => Sem r ()
endScope   = modify emptyTail

withCurrentScope :: ResolverMembers r => (Scope -> Scope) -> Sem r ()
withCurrentScope f = do 
  scopes <- get
  case uncons scopes of 
    Nothing -> pure ()
    Just (scope, rest) -> put (f scope:rest)
currentScope :: ResolverMembers r => Sem r (Maybe Scope)
currentScope = do
  scopes <- get @[Scope]
  pure $ case uncons scopes of 
    Nothing -> Nothing
    Just (scope, _) -> Just scope
declare    :: ResolverMembers r => T.Text -> Sem r () 
declare k  = withCurrentScope (HM.insert k False)

define     :: ResolverMembers r => T.Text -> Sem r ()
define  k  = withCurrentScope (HM.insert k True)
emptyTail [] = []
emptyTail xs = tail xs
type Scope = HM.HashMap T.Text Bool
type ResolverMembers r = Members [State [Scope], Fail] r
