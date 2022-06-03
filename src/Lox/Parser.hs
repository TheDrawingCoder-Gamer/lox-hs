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
import Data.Maybe (fromMaybe)
import Control.Monad (when)
parseLox :: LoxParser (Either [LxStmt] LxExpr)
parseLox = choice 
  [try (many parseLxDecl <* eof)  <&> Left
  , parseLxExpr <&> Right]
parseLxDecl :: LoxParser LxStmt
parseLxDecl = choice 
  [parseFunDecl FunNormal
  ,parseVarDecl
  ,parseLxStmt]
parseVarDecl :: LoxParser LxStmt
parseVarDecl = try $ uncurry LxVar <$> do 
  lxvar
  name <- lxident
  option (name, Nothing) (do 
    lxassign
    (,) name . Just <$> parseLxExpr) <* lxsemicolon
parseFunDecl :: FunKind -> LoxParser LxStmt
parseFunDecl _ = try $ do 
  lxfun 
  name <- lxident
  params <- parseParams
  when (length params >= 255) $ registerCustomFailure "Can't have more than 255 params"
  LxBlock ss <- parseBlock
  pure $ LxFunDecl name params (ss ++ [LxReturn (LxLit LvNil)])
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
  condition <- optional parseLxExpr 
  lxsemicolon
  incrementer <- optional parseLxExpr
  lxrparen
  body <- parseLxStmt
  let whileBody = LxBlock [body, LxExprStmt $ fromMaybe (LxLit LvNil) incrementer]
      whileLoop = LxWhile (fromMaybe (LxLit (LvBool True)) condition) whileBody
  pure $ LxBlock [fromMaybe (LxExprStmt (LxLit LvNil)) initializer, whileLoop] 
parseReturnStmt :: LoxParser LxStmt
parseReturnStmt = try $ do 
  lxreturn
  LxReturn <$> parseLxExpr <* lxsemicolon

parseLxExpr :: LoxParser LxExpr
parseLxExpr = parseLxCall <|> parseOpExpr
parseLxCall :: LoxParser LxExpr
parseLxCall = try $ do 
  daFunCall <- LxCall <$> parsePrimary <*> parseArgs
  manyLxArgs daFunCall 
    
manyLxArgs :: LxExpr -> LoxParser LxExpr
manyLxArgs e = do 
  option e $ do 
    manyLxArgs . LxCall e =<< parseArgs
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
  (case expr of 
    LxBinop (LxIdent _) _ LxAssign -> pure ()
    LxBinop _ _ LxAssign -> registerCustomFailure "Expected identifier on left side of assignment"
    _ -> pure ()) $>  expr
registerCustomFailure = registerFancyFailure . S.singleton . ErrorCustom
lxinfixl m t = InfixL (m $> \a b -> LxBinop a b t)
lxinfixr m t = InfixR (m $> \a b -> LxBinop a b t)
lxinfix m t = InfixN (m $> \a b -> LxBinop a b t)
lxprefix m t = Prefix (m $> LxUnary False t)
lxpostfix m t = Postfix (m $> LxUnary True t)
{-
parseEquality :: LoxParser LxExpr
parseEquality = do 
  lexpr <- parseComparison
  option lexpr $ do 
    daToken <- choice [lxequals $> LxEquals, lxnequal $> LxUnequal] 
    rexpr <- parseEquality
    pure $ LxBinop lexpr rexpr daToken

parseComparison :: LoxParser LxExpr 
parseComparison = do 
  lexpr <- parseTerm 
  option lexpr $ do 
    daToken <- choice 
      [ lxgreatereq $> LxGreaterEq
      , lxlesseq $> LxLessEq
      , lxless $> LxLess
      , lxgreater $> LxGreater] 
    rexpr <- parseComparison 
    pure $ LxBinop lexpr rexpr daToken

parseTerm :: LoxParser LxExpr
parseTerm = do 
  lexpr <- parseFactor 
  option lexpr $ do 
    daToken <- choice [lxplus $> LxPlus, lxminus $> LxMinus]
    rexpr <- parseTerm 
    pure $ LxBinop lexpr rexpr daToken

parseFactor :: LoxParser LxExpr
parseFactor = do 
  lexpr <- parseUnary
  option lexpr $ do 
    daToken <- choice [lxstar $> LxTimes, lxslash $> LxDiv]
    rexpr <- parseFactor 
    pure $ LxBinop lexpr rexpr daToken
-}
parsePrimary :: LoxParser LxExpr 
parsePrimary = choice 
  [ lxtrue $> LxLit (LvBool True)
  , lxfalse $> LxLit (LvBool False) 
  , lxnil $> LxLit LvNil
  , LxLit . LvNumber <$> lxnumber
  , LxLit . LvString <$> lxstring
  , LxGroup <$> between lxlparen lxrparen parseLxExpr
  , LxIdent <$> lxident ]

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
lxassign    = lexeme $ 
  do 
    single '='
    notFollowedBy (single '=')
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
lxident     :: LoxParser T.Text
lxident      = notFollowedBy (choice [lxand, lxclass, lxelse, lxfalse, lxfun, lxfor, lxif, lxnil, lxor, lxprint, lxreturn, lxsuper, lxthis, lxtrue, lxvar, lxwhile]) *> (lexeme $ T.cons 
              <$> satisfy (\c -> isAsciiUpper c || isAsciiLower c || c == '_')
              <*> takeWhileP Nothing (\c -> isAscii c && isLetter c) :: LoxParser T.Text)
lxstring    = lexeme (between "\"" "\"" (takeWhileP Nothing (`notElem` ("\n\"" :: [Char]))))

