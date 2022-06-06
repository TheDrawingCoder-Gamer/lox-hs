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
import Data.HashMap.Lazy qualified as HM
import Data.Maybe (fromMaybe)
import Control.Monad (when, (>=>))
import Data.Unique
import System.IO.Unsafe (unsafePerformIO)
parseLox :: LoxParser (Either [LxStmt] LxExpr)
parseLox = choice 
  [try (many parseLxDecl <* eof)  <&> Left
  , parseLxExpr <&> Right]
parseLxDecl :: LoxParser LxStmt
parseLxDecl = choice 
  [parseClassDecl
  ,parseFunDecl FunNormal
  ,parseVarDecl
  ,parseLxStmt]
parseClassDecl :: LoxParser LxStmt
parseClassDecl = try $ do 
  lxclass 
  LxClassDecl 
    <$> lxpprident 
    <*> (HM.fromList . map (\(LxFunDecl (FunDecl name info)) -> (name, info)) <$> between lxlbrace lxrbrace (many (parseFunDecl FunMethod)))
parseVarDecl :: LoxParser LxStmt
parseVarDecl = try $ uncurry LxVar <$> do 
  lxvar
  name <- lxident
  option (name, Nothing) (do 
    lxassign
    (,) name . Just <$> parseLxExpr) <* lxsemicolon
parseFunDecl :: FunKind -> LoxParser LxStmt
parseFunDecl FunNormal = try $ do 
  lxfun 
  LxFunDecl <$> parseFun
parseFunDecl FunMethod = LxFunDecl <$> parseFun
parseFun :: LoxParser FunDecl
parseFun = try $ do 
  name <- lxident 
  params <- parseParams
  when (length params >= 255) $ registerCustomFailure "Can't have more than 255 params"
  LxBlock ss <- parseBlock
  pure $ FunDecl name $ FunInfo params (ss ++ [LxReturn Nothing])
parseParams :: LoxParser [T.Text] 
parseParams = try $ between lxlparen lxrparen $ sepBy lxident lxcomma
parseLxStmt :: LoxParser LxStmt
parseLxStmt = choice
  [ (lxprint *> (parseLxExpr <* lxsemicolon)) <&> LxPrint
  , parseReturnStmt
  , parseBlock
  , parseIfStmt
  , parseForStmt
  , parseWhileStmt
  , parseExprStmt
  ]
parseExprStmt :: LoxParser LxStmt
parseExprStmt = (parseLxExpr <* lxsemicolon) <&> LxExprStmt
parseBlock :: LoxParser LxStmt
parseBlock = between lxlbrace lxrbrace $ many parseLxDecl <&> LxBlock
parseIfStmt :: LoxParser LxStmt
parseIfStmt = try $ do 
  lxif 
  expr <- between lxlparen lxrparen parseLxExpr 
  thenBranch <- parseLxStmt
  option (LxIf expr thenBranch Nothing) $ do 
   lxelse 
   LxIf expr thenBranch . Just <$> parseLxStmt
parseWhileStmt :: LoxParser LxStmt 
parseWhileStmt = try $ do 
  lxwhile 
  LxWhile 
    <$> between lxlparen lxrparen parseLxExpr 
    <*> parseLxStmt
parseForStmt :: LoxParser LxStmt
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
  let whileBody = LxBlock [body, LxExprStmt $ fromMaybe (LxExpr (LxLit PLvNil) makeUnique incrPos) incrementer]
      whileLoop = LxWhile (fromMaybe (LxExpr (LxLit (PLvBool True)) makeUnique condPos) condition) whileBody
  pure $ LxBlock [fromMaybe (LxExprStmt (LxExpr (LxLit PLvNil) makeUnique condPos)) initializer, whileLoop] 
parseReturnStmt :: LoxParser LxStmt
parseReturnStmt = try $ do 
  lxreturn
  option (LxReturn Nothing) (LxReturn . Just <$> parseLxExpr) <* lxsemicolon


parseLxExpr :: LoxParser LxExpr
parseLxExpr = parseLxCall <|> parseOpExpr
parseLxCall :: LoxParser LxExpr
parseLxCall = try $ do 
  daFunCall <- LxCall <$> parsePrimary <*> parseArgs
  srcPos <- getSourcePos
  let expr = LxExpr daFunCall makeUnique srcPos
  manyLxArgs expr
    
manyLxArgs :: LxExpr -> LoxParser LxExpr
manyLxArgs e = do 
  option e $ do 
    srcPos <- getSourcePos
    manyLxArgs . (\args -> LxExpr (LxCall e args) makeUnique srcPos) =<< parseArgs
parseArgs :: LoxParser [LxExpr]
parseArgs = do 
  args <- between lxlparen lxrparen $ sepBy parseLxExpr lxcomma
  when (length args >= 255) $ registerCustomFailure "Can't have more than 255 arguments"
  pure args
parseOpExpr = try $ do 
  expr <- makeExprParser parsePrimary [
      [lxprefix lxminus LxNegate
      ,lxprefix lxbang  LxNot]
    , [lxinfixl lxstar LxTimes
      ,lxinfixl lxslash LxDiv]
    , [lxinfixl lxplus LxPlus
      ,lxinfixl lxminus LxMinus]
    , [lxinfixl lxgreatereq LxGreaterEq
      ,lxinfixl lxgreater   LxGreater
      ,lxinfixl lxless      LxLess
      ,lxinfixl lxlesseq    LxLessEq]
    , [lxinfixl lxequals    LxEquals
      ,lxinfixl lxnequal    LxUnequal]
    , [lxinfixl lxand       LxAnd]
    , [lxinfixl lxor        LxOr]
    , [lxinfixr lxassign    LxAssign]]
  case expr of 
    LxExpr {exprNode = LxBinop lexpr@(LxExpr (LxIdent{}) _ _) rexpr LxAssign, exprPos = pos} -> pure (LxExpr (LxEAssign lexpr rexpr) makeUnique pos)
    LxExpr {exprNode = LxBinop _ _ LxAssign } -> registerCustomFailure "Expected identifier on left side of assignment" $> expr
    _ -> pure expr
registerCustomFailure = registerFancyFailure . S.singleton . ErrorCustom
lxinfixl m t = InfixL (m $> \a@(LxExpr _ _ pos) b -> LxExpr (LxBinop a b t) makeUnique pos)
lxinfixr m t = InfixR (m $> \a@(LxExpr _ _ pos) b -> LxExpr (LxBinop a b t) makeUnique pos)
lxinfix m t = InfixN (m $> \a@(LxExpr _ _ pos) b -> LxExpr (LxBinop a b t) makeUnique pos)
lxprefix m t = Prefix (m $> \a@(LxExpr _ _ pos) -> LxExpr (LxUnary False t a) makeUnique pos)
lxpostfix m t = Prefix (m $> \a@(LxExpr _ _ pos) -> LxExpr (LxUnary False t a) makeUnique pos)
parsePrimary :: LoxParser LxExpr 
parsePrimary = choice 
  [ lxtrue *> makeExpr (LxLit (PLvBool True))
  , lxfalse *> makeExpr (LxLit (PLvBool False))
  , lxnil *> makeExpr (LxLit PLvNil)
  , makeExpr . LxLit . PLvNumber =<< lxnumber
  , makeExpr . LxLit . PLvString =<< lxstring
  , makeExpr . LxGroup =<< between lxlparen lxrparen parseLxExpr
  , makeExpr . LxIdent =<< lxidentEither]
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
lxnumber    :: LoxParser Float
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

makeExpr :: LxExprNode -> LoxParser LxExpr
makeExpr node = LxExpr node makeUnique <$> getSourcePos
makeUnique = unsafePerformIO newUnique
{-# NOINLINE makeUnique #-}
