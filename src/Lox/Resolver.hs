{-# LANGUAGE ConstraintKinds #-}
module Lox.Resolver where

import Lox.Types
import Polysemy
import Polysemy.State
import Polysemy.Fail
import Data.HashMap.Lazy qualified as HM
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
resolveExpr expr@(LxExpr (LxIdent name) u p) = do 
  scope' <- currentScope
  case scope' of 
    Just scope -> 
      case HM.lookup name scope of 
        Just False -> fail "Can't read local variable in its own initializer"
        _ -> pure ()
    _ -> pure ()
  resolveLocal expr name
  pure expr
resolveExpr e@(LxExpr (LxEAssign ident@(LxExpr (LxIdent name) _ _) expr) u p) = do 
  expr' <- resolveExpr expr
  resolveLocal ident name
  pure $ e { exprNode = LxEAssign ident expr' }
resolveExpr (LxExpr LxEAssign{} _ _) = fail "unreachable"
resolveExpr (LxExpr (LxBinop lexpr rexpr t) u p) = 
  (\x -> LxExpr x u p) <$> (LxBinop <$> resolveExpr lexpr <*> resolveExpr rexpr <*> pure t)
resolveExpr (LxExpr (LxCall callee args) u p) = do 
  (\x -> LxExpr x u p) <$> (LxCall <$> resolveExpr callee <*> traverse resolveExpr args)
resolveExpr (LxExpr (LxGroup expr) u p) = LxExpr <$> (LxGroup <$> resolveExpr expr) <*> pure u <*> pure p
resolveExpr e@(LxExpr (LxLit _) _ _) = pure e
resolveExpr (LxExpr (LxUnary pf t rexpr ) u p) = LxExpr <$> (LxUnary pf t <$>resolveExpr rexpr) <*> pure u <*> pure p
resolveLocal :: ResolverMembers r => LxExpr -> T.Text -> Sem r ()
resolveLocal e@(LxExpr _ uniq srcpos) name = do 
  scopes <- gets scopes
  resolveRec scopes 0 
  where 
    resolveRec :: ResolverMembers r => [Scope] -> Int -> Sem r ()
    resolveRec (x:xs) i =
      case HM.lookup name x of 
        Nothing -> resolveRec xs (i + 1)
        Just _  -> do
            st <- get @ResolverState
            locs <- gets locals
            put $ st { locals = HM.insert e i locs }
    resolveRec _ _ = pure()
      
      
      
      
beginScope :: ResolverMembers r => Sem r ()
beginScope = modify (\e -> e { scopes = HM.empty : scopes e})

endScope   :: ResolverMembers r => Sem r ()
endScope   = modify (\e -> e { scopes = emptyTail (scopes e) } )

withCurrentScope :: ResolverMembers r => (Scope -> Scope) -> Sem r ()
withCurrentScope f = do 
  scopes <- gets scopes
  case uncons scopes of 
    Nothing -> pure ()
    Just (scope, rest) -> do 
      st <- get 
      put st { scopes = f scope:rest }
currentScope :: ResolverMembers r => Sem r (Maybe Scope)
currentScope = do
  scopes <- gets scopes
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
data ResolverState = ResolverState
  { locals  :: HM.HashMap LxExpr Int 
  , scopes :: [Scope]} 
type ResolverMembers r = Members [State ResolverState, Fail] r
