{-# LANGUAGE ConstraintKinds #-}
module Lox.Resolver where
{-

import Lox.Types
import Lox.Helpers
import Polysemy
import Polysemy.State
import Polysemy.Fail
import Data.HashMap.Lazy qualified as HM
import Data.Text qualified as T
import Data.Traversable (traverse)
import Data.Foldable    (traverse_)
import Data.List (uncons)
import Data.Maybe (maybe)
resolve :: ResolverMembers r => [Stmt] -> Sem r ()
resolve = traverse_ resolveStmt 
resolveStmt :: ResolverMembers r => Stmt -> Sem r ()
resolveStmt (Block ss) = 
  beginScope *> resolve ss <* endScope
resolveStmt (VarDef name init) = do 
  declare name 
  case init of 
    Nothing -> pure ()
    Just re -> resolveExpr re
  define name
resolveStmt (LFunDecl decl@(FunDecl name _)) = do 
  declare name 
  define name
  resolveFunction decl
resolveStmt (Eval expr) = resolveExpr expr
resolveStmt (LIf cond thenBranch elseBranch) = do 
  resolveExpr cond 
  resolveStmt thenBranch
  case elseBranch of
    Nothing -> pure ()
    Just e  -> resolveStmt e
resolveStmt (Print expr) = resolveExpr expr
resolveStmt (LReturn expr') = maybe (pure ()) resolveExpr expr'
resolveStmt (LWhile cond body) = resolveExpr cond *> resolveStmt body
resolveStmt (ClassDecl{}) = fail "unimplemented"
resolveFunction :: ResolverMembers r => FunDecl -> Sem r ()
resolveFunction (FunDecl' name args ss) = do 
  beginScope
  traverse_ (\arg -> declare arg *> define arg) args
  ss' <- resolve ss
  endScope
resolveExpr :: ResolverMembers r => Expr -> Sem r ()
resolveExpr expr@(Expr (Identifier name) p) = do 
  scope' <- currentScope
  case scope' of 
    Just scope -> 
      case HM.lookup name scope of 
        Just False -> fail "Can't read local variable in its own initializer"
        _ -> pure ()
    _ -> pure ()
  resolveLocal expr name
resolveExpr e@(Expr (Assign name expr) p) = do 
  resolveExpr expr
  resolveLocal ident name
resolveExpr (Expr (Binop lexpr rexpr t) p) = 
  (\x -> LxExpr x u p) <$> (LxBinop <$> resolveExpr lexpr <*> resolveExpr rexpr <*> pure t)
resolveExpr (Expr (Call callee args) p) = do 
  (\x -> LxExpr x u p) <$> (LxCall <$> resolveExpr callee <*> traverse resolveExpr args)
resolveExpr (Expr (Grouping expr) p) = LxExpr <$> (LxGroup <$> resolveExpr expr) <*> pure u <*> pure p
resolveExpr e@(Expr (Literal{}) _) = pure e
resolveExpr (Expr (Unary t rexpr ) p) = LxExpr <$> (LxUnary t <$>resolveExpr rexpr) <*> pure u <*> pure p
resolveLocal :: ResolverMembers r => T.Text -> Sem r ()
resolveLocal name = do 
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
  { locals  :: HM.HashMap Expr Int 
  , scopes :: [Scope]} 
type ResolverMembers r = Members [State ResolverState, Fail] r
-}
