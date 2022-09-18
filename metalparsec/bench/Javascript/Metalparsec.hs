{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Javascript.Metalparsec (javascript) where

import Control.Applicative (liftA2, liftA3, (<**>))
import Control.Monad.Combinators (option, sepBy, sepBy1)
import Data.Char (digitToInt, isSpace, isUpper)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Javascript.Common
import Javascript.Metalparsec.Extended
import Text.Metalparsec
import Text.Read (readMaybe)

javascript :: Parser JSProgram
javascript = whitespace *> many element <* eof

element :: Parser JSElement
element =
  (keyword "function" *> (JSFunction <$> identifier <*> (parens (commaSep identifier)) <*> compound))
    <|> JSStm <$> stmt

compound :: Parser JSCompoundStm
compound = braces (many stmt)

stmt :: Parser JSStm
stmt =
  semi $> JSSemi
    <|> keyword "if" *> liftA3 JSIf parensExpr stmt (maybeP (keyword "else" *> stmt))
    <|> keyword "while" *> liftA2 JSWhile parensExpr stmt
    <|> ( keyword "for"
            *> parens
              ( (liftA2 JSForIn varsOrExprs (keyword "in" *> expr))
                  <|> liftA3 JSFor (maybeP varsOrExprs <* semi) (optExpr <* semi) optExpr
              )
            <*> stmt
        )
    <|> keyword "break" $> JSBreak
    <|> keyword "continue" $> JSContinue
    <|> keyword "with" *> liftA2 JSWith parensExpr stmt
    <|> keyword "return" *> (JSReturn <$> optExpr)
    <|> JSBlock <$> compound
    <|> JSNaked <$> varsOrExprs

varsOrExprs :: Parser (Either [JSVar] JSExpr)
varsOrExprs = (keyword "var" *> commaSep1 variable) <+> expr

variable :: Parser JSVar
variable = liftA2 JSVar identifier (maybeP (symbol '=' *> asgn))

parensExpr :: Parser JSExpr
parensExpr = parens expr

optExpr :: Parser (Maybe JSExpr)
optExpr = maybeP expr

expr :: Parser JSExpr
expr = commaSep1 asgn

asgn :: Parser JSExpr'
asgn = infixL condExpr $ symbol '=' $> JSAsgn

condExpr :: Parser JSExpr'
condExpr = liftA2 jsCondExprBuild expr' (maybeP ((symbol '?' *> asgn) <~> (symbol ':' *> asgn)))

expr' :: Parser JSExpr'
-- expr' = undefined
-- expr' =
--   precedence
--     [ Prefix
--         [ operator "--" $> jsDec,
--           operator "++" $> jsInc,
--           operator "-" $> jsNeg,
--           operator "+" $> jsPlus,
--           operator "~" $> jsBitNeg,
--           operator "!" $> jsNot
--         ],
--       Postfix [operator "--" $> jsDec, operator "++" $> jsInc],
--       InfixL
--         [ operator "*" $> JSMul,
--           operator "/" $> JSDiv,
--           operator "%" $> JSMod
--         ],
--       InfixL [operator "+" $> JSAdd, operator "-" $> JSSub],
--       InfixL [operator "<<" $> JSShl, operator ">>" $> JSShr],
--       InfixL
--         [
-- operator "<=" $> JSLe,
--           operator "<" $> JSLt,
--           operator ">=" $> JSGe,
--           operator ">" $> JSGt
--         ],
--       InfixL [operator "==" $> JSEq, operator "!=" $> JSNe],
--       InfixL [operator "&" $> JSBitAnd],
--       InfixL [operator "^" $> JSBitXor],
--       InfixL [operator "|" $> JSBitOr],
--       InfixL [operator "&&" $> JSAnd],
--       InfixL [operator "||" $> JSOr]
--     ]
--     memOrCon
expr' = infix10

infix10 = infixL infix9 $ operator "||" $> JSOr

infix9 = infixL infix8 $ operator "&&" $> JSAnd

infix8 = infixL infix7 $ operator "|" $> JSBitOr

infix7 = infixL infix6 $ operator "^" $> JSBitXor

infix6 = infixL infix5 $ operator "&" $> JSBitAnd

infix5 =
  infixL infix4 $
    operator "==" $> JSEq <|> operator "!=" $> JSNe

infix4 =
  infixL infix3 $
    operator "<=" $> JSLe
      <|> operator "<" $> JSLt
      <|> operator ">=" $> JSGe
      <|> operator ">" $> JSGt

infix3 =
  infixL infix2 $
    operator "<<" $> JSShl
      <|> operator ">>" $> JSShr

infix2 =
  infixL infix1 $
    operator "+" $> JSAdd
      <|> operator "-" $> JSSub

infix1 =
  infixL postFixExpr $
    operator "*" $> JSMul
      <|> operator "/" $> JSDiv
      <|> operator "%" $> JSMod

postFixExpr =
  postfix prefixExpr $
    operator "--" $> JSDec
      <|> operator "++" $> JSInc

prefixExpr =
  prefix memOrCon $
    operator "--" $> JSDec
      <|> operator "++" $> JSInc
      <|> operator "-" $> JSNeg
      <|> operator "+" $> JSPlus
      <|> operator "~" $> JSBitNeg
      <|> operator "!" $> JSNot

prefix :: Parser JSExpr' -> Parser JSUnaryOp -> Parser JSExpr'
prefix e op = chainPre (do o <- op; pure $ JSUnary o) e

postfix :: Parser JSExpr' -> Parser JSUnaryOp -> Parser JSExpr'
postfix e op = chainPost e $ do o <- op; pure $ JSUnary o

infixL :: Parser JSExpr' -> Parser JSBinOp -> Parser JSExpr'
infixL p op = chainl1 p $ do o <- op; pure $ \e e' -> JSBin e o e'
{-# INLINE infixL #-}

memOrCon :: Parser JSExpr'
memOrCon =
  keyword "delete" *> (JSDel <$> member)
    <|> keyword "new" *> (JSCons <$> con)
    <|> JSMember <$> member

con :: Parser JSCons
con = liftA2 JSQual (keyword "this" $> "this") (dot *> conCall) <|> conCall

conCall :: Parser JSCons
conCall =
  identifier
    <**> ( dot *> (flip JSQual <$> conCall)
             <|> flip JSConCall <$> parens (commaSep asgn)
             <|> pure (\name -> JSConCall name [])
         )

member :: Parser JSMember
member =
  primaryExpr
    <**> ( flip JSCall <$> parens (commaSep asgn)
             <|> flip JSIndex <$> brackets expr
             <|> dot *> ((flip JSAccess) <$> member)
             <|> pure JSPrimExp
         )

primaryExpr :: Parser JSAtom
primaryExpr =
  JSParens <$> parens expr
    <|> JSArray <$> brackets (commaSep asgn)
    <|> JSId <$> identifier
    <|> either JSInt JSFloat <$> naturalOrFloat
    <|> JSString <$> stringLiteral
    <|> JSTrue <$ keyword "true"
    <|> JSFalse <$ keyword "false"
    <|> JSNull <$ keyword "null"
    <|> JSThis <$ keyword "this"

space :: Parser ()
space = void (satisfyAscii isSpace)

whitespace :: Parser ()
whitespace = many_ (spaces <|> oneLineComment <|> multiLineComment)

keyword :: Text -> Parser ()
keyword s = text s `notFollowedBy` identLetter *> whitespace

operator :: Text -> Parser ()
operator op =
  ( notFollowedBy
      (text op)
      ( satisfyAscii $
          \c ->
            c == '+'
              || c == '-'
              || c == '*'
              || c == '/'
              || c == '='
              || c == '<'
              || c == '>'
              || c == '!'
              || c == '~'
              || c == '&'
              || c == '|'
              || c == '.'
              || c == '%'
              || c == '^'
      )
  )
    *> whitespace

identifier :: Parser Text
identifier = (slice ((identStart <:> many identLetter) >?> jsUnreservedName)) <* whitespace

naturalOrFloat :: Parser (Either Int Double)
naturalOrFloat = natFloat <* whitespace

-- Nonsense to deal with floats and ints
natFloat :: Parser (Either Int Double)
natFloat = asciiChar '0' *> zeroNumFloat <|> decimalFloat

zeroNumFloat :: Parser (Either Int Double)
zeroNumFloat =
  Left <$> (hexadecimal <|> octal)
    <|> decimalFloat
    <|> (fromMaybeP (fractFloat <*> pure 0) empty)
    <|> pure (Left 0)

decimalFloat :: Parser (Either Int Double)
decimalFloat = fromMaybeP (decimal <**> (option (Just . Left) fractFloat)) empty

fractFloat :: Parser (Int -> Maybe (Either Int Double))
fractFloat = f <$> fractExponent
  where
    f g x = fmap Right (g x)

fractExponent :: Parser (Int -> Maybe Double)
fractExponent =
  f <$> fraction <*> option "" exponent'
    <|> f <$> pure "" <*> exponent'
  where
    f fract exp n = readMaybe (show n ++ fract ++ exp)

fraction :: Parser [Char]
fraction = (asciiChar '.' $> '.') <:> some (satisfyAscii isAsciiDigit)

exponent' :: Parser [Char]
exponent' =
  ('e' :)
    <$> ( satisfyAscii (\c -> c == 'e' || c == 'E')
            *> ( (((:) <$> satisfyAscii (\c -> c == '+' || c == '-')) <|> pure id)
                   <*> (show <$> decimal)
               )
        )

decimal :: Parser Int
decimal = number 10 (satisfyAscii isAsciiDigit)

hexadecimal = satisfyAscii (\c -> c == 'x' || c == 'X') *> number 16 (satisfyAscii (\c -> (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F') || isAsciiDigit c))

octal = satisfyAscii (\c -> c == 'o' || c == 'O') *> number 8 (satisfyAscii (\c -> c >= '0' && c <= '7'))

number :: Int -> Parser Char -> Parser Int
number base = pfoldl1 (\x d -> base * x + digitToInt d) 0

stringLiteral :: Parser Text
stringLiteral = (T.pack . catMaybes) <$> (between (asciiChar '\"') (asciiChar '\"') (many stringChar)) <* whitespace

symbol :: Char -> Parser ()
symbol c = asciiChar c <* whitespace

parens :: Parser a -> Parser a
parens = between (symbol '(') (symbol ')')

brackets :: Parser a -> Parser a
brackets = between (symbol '[') (symbol ']')

braces :: Parser a -> Parser a
braces = between (symbol '{') (cut (symbol '}') "expected closing")

dot :: Parser ()
dot = symbol '.'

semi :: Parser ()
semi = symbol ';'

comma :: Parser ()
comma = symbol ','

commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p comma

commaSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p comma

-- Let bindings
spaces :: Parser ()
spaces = some_ space

oneLineComment :: Parser ()
oneLineComment = void (token "//" *> many_ (satisfyChar (/= '\n')))

multiLineComment :: Parser ()
multiLineComment =
  let inComment =
        void (token "*/")
          <|> some_ (satisfyChar (/= '*')) *> inComment
          <|> asciiChar '*' *> inComment
   in token "/*" *> inComment

identStart = satisfyChar jsIdentStart

identLetter = satisfyChar jsIdentLetter

stringChar :: Parser (Maybe Char)
stringChar = Just <$> satisfyChar jsStringLetter <|> stringEscape

stringEscape :: Parser (Maybe Char)
stringEscape =
  token "\\"
    *> ( token "&" $> Nothing
           <|> spaces *> token "\\" $> Nothing
           <|> Just <$> escapeCode
       )

escapeCode :: Parser Char
escapeCode =
  anyCharAscii >>= escCode
  where
    escCode 'a' = pure ('\a')
    escCode 'b' = pure ('\b')
    escCode 'f' = pure ('\f')
    escCode 'n' = pure ('\n')
    escCode 't' = pure ('\t')
    escCode 'v' = pure ('\v')
    escCode '\\' = pure ('\\')
    escCode '"' = pure ('"')
    escCode '\'' = pure ('\'')
    escCode '^' = (\c -> toEnum (fromEnum c - fromEnum 'A' + 1)) <$> satisfyChar isUpper
    escCode 'A' = token "CK" $> ('\ACK')
    escCode 'B' = token "S" $> ('\BS') <|> token "EL" $> ('\BEL')
    escCode 'C' = token "R" $> ('\CR') <|> token "AN" $> ('\CAN')
    escCode 'D' =
      token "C"
        *> ( token "1" $> ('\DC1')
               <|> token "2" $> ('\DC2')
               <|> token "3" $> ('\DC3')
               <|> token "4" $> ('\DC4')
           )
        <|> token "EL" $> ('\DEL')
        <|> token "LE" $> ('\DLE')
    escCode 'E' =
      token "M" $> ('\EM')
        <|> token "T"
          *> ( token "X" $> ('\ETX')
                 <|> token "B" $> ('\ETB')
             )
        <|> token "SC" $> ('\ESC')
        <|> token "OT" $> ('\EOT')
        <|> token "NQ" $> ('\ENQ')
    escCode 'F' = token "F" $> ('\FF') <|> token "S" $> ('\FS')
    escCode 'G' = token "S" $> ('\GS')
    escCode 'H' = token "T" $> ('\HT')
    escCode 'L' = token "F" $> ('\LF')
    escCode 'N' = token "UL" $> ('\NUL') <|> token "AK" $> ('\NAK')
    escCode 'R' = token "S" $> ('\RS')
    escCode 'S' =
      token "O" *> option (('\SO')) (token "H" $> ('\SOH'))
        <|> token "I" $> ('\SI')
        <|> token "P" $> ('\SP')
        <|> token "TX" $> ('\STX')
        <|> token "YN" $> ('\SYN')
        <|> token "UB" $> ('\SUB')
    escCode 'U' = token "S" $> ('\US')
    escCode 'V' = token "T" $> ('\VT')
    -- TODO numeric
    escCode _ = err "escape code not supported"
