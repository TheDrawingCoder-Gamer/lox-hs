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
import Control.Monad (when, (>=>), MonadPlus)
import Data.Unique
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
parseLox :: LoxParser (Either [Stmt] Expr)
parseLox = choice 
  [try (many parseLxDecl <* eof)  <&> Left
  , (parseLxExpr <* eof) <&> Right] -- todo: if both fail show stmt error as it's more likely what is wanted
parseLxDecl :: LoxParser Stmt
parseLxDecl = choice 
  [parseClassDecl
  ,parseFunDecl FunNormal
  ,parseVarDecl
  ,parseLxStmt]
parseClassDecl :: LoxParser Stmt
parseClassDecl = try $ do 
  lxclass
  ident <- lxident
  superclass <- optional (lxless *> (WithPos <$> lxident <*> getSourcePos))
  ClassDecl ident superclass <$> (M.fromList . map (\(LFunDecl (FunDecl name info)) -> (name, info)) <$> between lxlbrace lxrbrace (many (parseFunDecl FunMethod)))
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
  FunDecl name <$> parseMeatFun
parseMeatFun :: LoxParser FunInfo
parseMeatFun = do 
  params <- parseParams
  when (length params >= 255) $ registerCustomFailure "Can't have more than 255 params"
  Block ss <- parseBlock
  pure $ FunInfo params (ss ++ [LReturn Nothing])
parseAnonFun :: LoxParser Expr
parseAnonFun = do 
  lxfun
  pos <- getSourcePos
  FunInfo params ss <- parseMeatFun
  pure $ Expr (AnonFun params ss) pos 
parseParams :: LoxParser [T.Text] 
parseParams = try $ between lxlparen lxrparen $ sepBy lxident lxcomma
parseLxStmt :: LoxParser Stmt
parseLxStmt = choice
  [ (lxprint *> (parseLxExpr <* lxsemicolon)) <&> Print
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
  let whileBody = Block [body, Eval $ fromMaybe (Expr (Literal LoxNil) incrPos) incrementer]
      whileLoop = LWhile (fromMaybe (Expr (Literal (LoxBool True)) condPos) condition) whileBody
  pure $ Block [fromMaybe (Eval (Expr (Literal LoxNil) initPos)) initializer, whileLoop] 
parseReturnStmt :: LoxParser Stmt
parseReturnStmt = try $ do 
  lxreturn
  option (LReturn Nothing) (LReturn . Just <$> parseLxExpr) <* lxsemicolon


parseLxExpr :: LoxParser Expr
parseLxExpr = parseAssign

parseLxCall :: LoxParser Expr
parseLxCall = manyLxArgs =<< parsePrimary 
manyLxArgs :: Expr -> LoxParser Expr
manyLxArgs e = 
  option e (try $ manyLxArgs =<< choice [parseArgs e, parseGet e])
parseArgs :: Expr -> LoxParser Expr
parseArgs e = try $ do 
  pos <- getSourcePos
  args <- between lxlparen lxrparen (sepBy parseLxExpr lxcomma)
  when (length args >= 255) $ registerCustomFailure "Can't have more than 255 arguments"
  pure $ Expr (Call e args) pos
parseGet :: Expr -> LoxParser Expr
parseGet e = try $ do 
  lxdot
  pos <- getSourcePos
  field <- lxident
  pure $ Expr (Get e field) pos

{-
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
    Expr (LxParseFail f) _-> registerCustomFailure f
    _ -> pure ()) $> expr


lxassignop m = InfixR (m $> \a@(Expr _ pos) b -> 
  case a of
    Expr (Identifier name) _ -> Expr (Assign name b) pos
    Expr (Get e name) _ -> Expr (Set e name b) pos
    _ -> Expr (LxParseFail "Expected identifier on left side of assignment") pos)
lxinfixl m t = InfixL (m $> \a@(Expr _ pos) b -> Expr (Binop a b t) pos)
lxinfixr m t = InfixR (m $> \a@(Expr _ pos) b -> Expr (Binop a b t) pos)
lxinfix m t = InfixN (m $> \a@(Expr _ pos) b -> Expr (Binop a b t) pos)
lxprefix m t = Prefix (m $> \a@(Expr _ pos) -> Expr (Unary t a) pos)
lxpostfix m t = Prefix (m $> \a@(Expr _ pos) -> Expr (Unary t a) pos)
-}
lxbinop :: LoxParser a -> BinopKind -> LoxParser (Expr -> Expr -> Expr)
lxbinop m t = m $> \a@(Expr _ pos) b -> Expr (Binop a b t) pos

parseAssign :: LoxParser Expr 
parseAssign = try (do 
  lexpr <- parseLogicOr 
  pos <- getSourcePos
  lxassign
  rexpr <- parseAssign
  case lexpr of 
    Expr (Identifier name) _ -> 
      pure (Expr (Assign name rexpr) pos)
    Expr (Get e name) _ -> 
      pure (Expr (Set e name rexpr) pos)
    -- todo: soft fail
    _ -> fail "Left side of assign must be an identifier or a field access"
  )
  <|> parseLogicOr
parseLogicOr :: LoxParser Expr 
parseLogicOr = chainl1 parseLogicAnd (lxbinop lxor Or) 
parseLogicAnd :: LoxParser Expr
parseLogicAnd = chainl1 parseEquality (lxbinop lxequals Equals)
parseEquality :: LoxParser Expr
parseEquality = chainl1 parseComparison (choice [lxbinop lxequals Equals, lxbinop lxnequal Unequal])

parseComparison :: LoxParser Expr
parseComparison = chainl1 parseTerm 
  (choice 
  [ lxbinop lxgreatereq GreaterEq
  , lxbinop lxgreater   Greater
  , lxbinop lxless      Less
  , lxbinop lxlesseq    LessEq ])

parseTerm :: LoxParser Expr
parseTerm = chainl1 parseFactor (choice [lxbinop lxplus Plus, lxbinop lxminus Minus])

parseFactor :: LoxParser Expr
parseFactor = chainl1 parseUnary (choice [lxbinop lxstar Times, lxbinop lxslash Divide])

parseUnary :: LoxParser Expr
parseUnary = (do 
  op <- choice [lxbang $> Not, lxminus $> Negate]
  pos <- getSourcePos
  Expr <$> (Unary op <$> parseUnary) <*> pure pos) <|> parseLxCall 

parseSuper :: LoxParser Expr
parseSuper = do 
  lxsuper
  pos <- getSourcePos
  lxdot
  name <- lxident
  pure $ Expr (LSuper name) pos
parsePrimary :: LoxParser Expr 
parsePrimary = choice 
  [ parseAnonFun
  , lxtrue *> makeExpr (Literal (LoxBool True))
  , lxfalse *> makeExpr (Literal (LoxBool False))
  , lxnil *> makeExpr (Literal LoxNil)
  , makeExpr . Literal . LoxNumber =<< lxnumber
  , makeExpr . Literal . LoxString =<< lxstring
  , makeExpr . Grouping =<< between lxlparen lxrparen parseLxExpr
  , lxthis *> makeExpr LThis
  , parseSuper
  , makeExpr . Identifier =<< lxident]
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
lxident = lxidentLower <|> lxpprident
lxidentLower = lxidentGen True
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
makeExpr node = Expr node <$> getSourcePos 

chainl  :: MonadPlus m => m a -> m (a -> a -> a) -> a -> m a 
chainl p op x = option x (chainl1 p op)
chainl1 :: MonadPlus m => m a -> m (a -> a -> a) -> m a
chainl1 p op = p >>= rest
  where 
    rest x = do f <- op
                y <- p 
                rest (f x y)
            <|> pure x

registerCustomFailure = registerFancyFailure . S.singleton . ErrorCustom
