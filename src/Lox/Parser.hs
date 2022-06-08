module Lox.Parser (parseLox, parseLxExpr) where

import Lox.Types
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char qualified as MC
import Data.Functor ((<&>), ($>))
import Data.Text qualified as T
import Control.Monad.Combinators.Expr
import Data.Char
import Data.Set qualified as S
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Control.Monad (when, (>=>))
import Data.Unique
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
parseLox :: LoxParser (Either [Stmt] Expr)
parseLox = choice 
  [try (many parseLxDecl <* eof)  <&> Left
  , parseLxExpr <&> Right] -- todo: if both fail show stmt error as it's more likely what is wanted
parseLxDecl :: LoxParser Stmt
parseLxDecl = choice 
  [parseClassDecl
  ,parseFunDecl FunNormal
  ,parseVarDecl
  ,parseLxStmt]
parseClassDecl :: LoxParser Stmt
parseClassDecl = try $ do 
  lxclass 
  ClassDecl
    <$> lxpprident 
    <*> (M.fromList . map (\(LFunDecl (FunDecl name info)) -> (name, info)) <$> between lxlbrace lxrbrace (many (parseFunDecl FunMethod)))
parseVarDecl :: LoxParser Stmt
parseVarDecl = try $ uncurry VarDef <$> do 
  lxvar
  name <- lxident
  option (name, Nothing) (do 
    lxassign
    (,) name . Just <$> parseLxExpr) <* lxsemicolon
parseFunDecl :: FunKind -> LoxParser Stmt
parseFunDecl FunNormal = try $ do 
  lxfun 
  LFunDecl <$> parseFun
parseFunDecl FunMethod = LFunDecl <$> parseFun
parseFun :: LoxParser FunDecl
parseFun = try $ do 
  name <- lxident 
  params <- parseParams
  when (length params >= 255) $ registerCustomFailure "Can't have more than 255 params"
  Block ss <- parseBlock
  pure $ FunDecl name $ FunInfo params (ss ++ [LReturn Nothing])
parseParams :: LoxParser [T.Text] 
parseParams = try $ between lxlparen lxrparen $ sepBy lxident lxcomma
parseLxStmt :: LoxParser Stmt
parseLxStmt = choice
  [ lxdumpheap $> HeapDump
  , lxdumpstack $> StackDump
  , lxdumpclosures $> ClosuresDump
  , (lxprint *> (parseLxExpr <* lxsemicolon)) <&> Print
  , parseReturnStmt
  , parseBlock
  , parseIfStmt
  , parseForStmt
  , parseWhileStmt
  , parseExprStmt
  ]
parseExprStmt :: LoxParser Stmt
parseExprStmt = (parseLxExpr <* lxsemicolon) <&> Eval
parseBlock :: LoxParser Stmt
parseBlock = between lxlbrace lxrbrace $ many parseLxDecl <&> Block
parseIfStmt :: LoxParser Stmt
parseIfStmt = try $ do 
  lxif 
  expr <- between lxlparen lxrparen parseLxExpr 
  thenBranch <- parseLxStmt
  option (LIf expr thenBranch Nothing) $ do 
   lxelse 
   LIf expr thenBranch . Just <$> parseLxStmt
parseWhileStmt :: LoxParser Stmt 
parseWhileStmt = try $ do 
  lxwhile 
  LWhile 
    <$> between lxlparen lxrparen parseLxExpr 
    <*> parseLxStmt
parseForStmt :: LoxParser Stmt
parseForStmt = try $ do 
  lxfor 
  lxlparen
  initializer <- try (lxsemicolon $> Nothing) <|> try (Just <$> parseVarDecl) <|> (Just <$> parseExprStmt)
  initPos <- getSourcePos
  condition <- optional parseLxExpr 
  condPos <- getSourcePos
  lxsemicolon
  incrementer <- optional parseLxExpr
  incrPos <- getSourcePos
  lxrparen
  body <- parseLxStmt
  let whileBody = Block [body, Eval $ fromMaybe (Expr (Literal LoxNil) incrPos getCount) incrementer]
      whileLoop = LWhile (fromMaybe (Expr (Literal (LoxBool True)) condPos getCount) condition) whileBody
  pure $ Block [fromMaybe (Eval (Expr (Literal LoxNil) condPos getCount)) initializer, whileLoop] 
parseReturnStmt :: LoxParser Stmt
parseReturnStmt = try $ do 
  lxreturn
  option (LReturn Nothing) (LReturn . Just <$> parseLxExpr) <* lxsemicolon


parseLxExpr :: LoxParser Expr
parseLxExpr = parseLxCall <|> parseOpExpr

parseLxCall :: LoxParser Expr
parseLxCall = try $ do 
  daFunCall <- Call <$> parsePrimary <*> parseArgs
  srcPos <- getSourcePos
  let expr = Expr daFunCall srcPos getCount
  manyLxArgs expr
    
manyLxArgs :: Expr -> LoxParser Expr
manyLxArgs e = do 
  option e $ do 
    srcPos <- getSourcePos
    manyLxArgs . (\args -> Expr (Call e args) srcPos getCount) =<< parseArgs
parseArgs :: LoxParser [Expr]
parseArgs = do 
  args <- between lxlparen lxrparen $ sepBy parseLxExpr lxcomma
  when (length args >= 255) $ registerCustomFailure "Can't have more than 255 arguments"
  pure args
parseOpExpr = try $ do 
  expr <- makeExprParser parsePrimary [
      [lxprefix lxminus Negate
      ,lxprefix lxbang  Not]
    , [lxinfixl lxstar Times
      ,lxinfixl lxslash Divide]
    , [lxinfixl lxplus Plus
      ,lxinfixl lxminus Minus]
    , [lxinfixl lxgreatereq GreaterEq
      ,lxinfixl lxgreater   Greater
      ,lxinfixl lxless      Less
      ,lxinfixl lxlesseq    LessEq]
    , [lxinfixl lxequals    Equals
      ,lxinfixl lxnequal    Unequal]
    , [lxinfixl lxand       And]
    , [lxinfixl lxor        Or]
    , [lxassignop lxassign]]
  (case expr of 
    Expr (LxParseFail f) _ _-> registerCustomFailure f
    _ -> pure ()) $> expr
registerCustomFailure = registerFancyFailure . S.singleton . ErrorCustom

lxassignop m = InfixR (m $> \a@(Expr _ pos idn) b -> 
  case a of
    Expr (Identifier name) _ _ -> Expr (Assign name b) pos idn
    _ -> Expr (LxParseFail "Expected identifier on left side of assignment") pos idn)
lxinfixl m t = InfixL (m $> \a@(Expr _ pos _) b -> Expr (Binop a b t) pos getCount)
lxinfixr m t = InfixR (m $> \a@(Expr _ pos _) b -> Expr (Binop a b t) pos getCount)
lxinfix m t = InfixN (m $> \a@(Expr _ pos _) b -> Expr (Binop a b t) pos getCount)
lxprefix m t = Prefix (m $> \a@(Expr _ pos _) -> Expr (Unary t a) pos getCount)
lxpostfix m t = Prefix (m $> \a@(Expr _ pos _) -> Expr (Unary t a) pos getCount)
parsePrimary :: LoxParser Expr 
parsePrimary = choice 
  [ lxtrue *> makeExpr (Literal (LoxBool True))
  , lxfalse *> makeExpr (Literal (LoxBool False))
  , lxnil *> makeExpr (Literal LoxNil)
  , makeExpr . Literal . LoxNumber =<< lxnumber
  , makeExpr . Literal . LoxString =<< lxstring
  , makeExpr . Grouping =<< between lxlparen lxrparen parseLxExpr
  , makeExpr . Identifier =<< lxidentEither]
lexSpace :: LoxParser () 
lexSpace = L.space MC.space1 (L.skipLineComment "//") (L.skipBlockCommentNested "/*" "*/")

symbol :: T.Text -> LoxParser T.Text
symbol = try . L.symbol lexSpace
lexeme :: LoxParser a -> LoxParser a
lexeme = try . L.lexeme lexSpace

lxlparen    = symbol "("
lxrparen    = symbol ")"
lxlbrace    = symbol "{"
lxrbrace    = symbol "}"
lxcomma     = symbol ","
lxdot       = symbol "."
lxminus     = symbol "-" 
lxplus      = symbol "+" 
lxsemicolon = symbol ";"
lxstar      = symbol "*"
lxslash     = symbol "/"
lxnequal    = symbol "!="
lxequals    = symbol "=="
lxassign    = lexeme $ notFollowedBy lxequals *> single '=' 
lxbang      = symbol "!"
lxgreatereq = symbol ">=" 
lxgreater   = symbol ">"
lxlesseq    = symbol "<=" 
lxless      = symbol "<"
lxnumber    :: LoxParser Double
lxnumber    = try (lexeme L.float) <|> lexeme L.decimal
lxand       = symbol "and"
lxclass     = symbol "class" 
lxelse      = symbol "else"
lxfalse     = symbol "false"
lxfun       = symbol "fun"
lxfor       = symbol "for"
lxif        = symbol "if"
lxnil       = symbol "nil"
lxor        = symbol "or"
lxprint     = symbol "print"
lxreturn    = symbol "return"
lxsuper     = symbol "super" 
lxthis      = symbol "this"
lxtrue      = symbol "true"
lxvar       = symbol "var"
lxwhile     = symbol "while"
lxdumpheap = symbol "dumpheap!" *> lxsemicolon
lxdumpstack = symbol "dumpstack!" *> lxsemicolon
lxdumpclosures = symbol "dumpclosures!" *> lxsemicolon
lxidentGen       :: Bool -> LoxParser T.Text
lxidentGen lower  = try (notReservedKeyword *> lexeme (T.cons 
                      <$> satisfy (validIdentStarter lower)
                      <*> takeWhileP Nothing validIdentTail))
lxidentEither = lxident <|> lxpprident
lxident = lxidentGen True
lxpprident = lxidentGen False
validIdentStarter :: Bool -> Char -> Bool
validIdentStarter lower '_' = True
validIdentStarter lower c   = isAscii c && isLetter c
validIdentTail :: Char -> Bool
validIdentTail '_' = True
validIdentTail c = (isAscii c && isLetter c) || isDigit c
notReservedKeyword :: LoxParser ()
notReservedKeyword = notFollowedBy (choice [lxand, lxclass, lxelse, lxfalse, lxfun, lxfor, lxif, lxnil, lxor, lxprint, lxreturn, lxsuper, lxthis, lxtrue, lxvar, lxwhile])
lxstring    = lexeme (between "\"" "\"" (takeWhileP Nothing (`notElem` ("\n\"" :: [Char]))))

makeExpr :: ExprNode -> LoxParser Expr
makeExpr node = Expr node <$> getSourcePos <*> pure getCount

counter :: IORef Int
counter = unsafePerformIO $ newIORef 0
{-# NOINLINE counter #-}

getCount :: Int 
getCount = unsafePerformIO $ readIORef counter <* modifyIORef' counter (+1) 

{-# NOINLINE getCount #-}
