{-# LANGUAGE DeriveAnyClass #-}

module Javascript.Common where

import Control.DeepSeq (NFData)
import Data.Char (isAlpha, isAlphaNum)
import Data.Set (fromList, member)
import Data.Text (Text)
import GHC.Generics (Generic)

type JSProgram = [JSElement]

type JSCompoundStm = [JSStm]

type JSExpr = [JSExpr']

data JSElement = JSFunction Text [Text] JSCompoundStm | JSStm JSStm deriving (Show)

data JSStm
  = JSSemi
  | JSIf JSExpr JSStm (Maybe JSStm)
  | JSWhile JSExpr JSStm
  | JSFor (Maybe (Either [JSVar] JSExpr)) (Maybe JSExpr) (Maybe JSExpr) JSStm
  | JSForIn (Either [JSVar] JSExpr) JSExpr JSStm
  | JSBreak
  | JSContinue
  | JSWith JSExpr JSStm
  | JSReturn (Maybe JSExpr)
  | JSBlock JSCompoundStm
  | JSNaked (Either [JSVar] JSExpr)
  deriving (Show)

data JSVar = JSVar Text (Maybe JSExpr') deriving (Show)

data JSExpr'
  = JSCond JSExpr' JSExpr' JSExpr'
  | JSBin JSExpr' JSBinOp JSExpr'
  | JSUnary JSUnaryOp JSExpr'
  | JSNew JSCons
  | JSDel JSMember
  | JSMember JSMember
  | JSCons JSCons
  deriving (Show)

data JSBinOp
  = JSOr
  | JSAnd
  | JSBitOr
  | JSBitXor
  | JSBitAnd
  | JSEq
  | JSNe
  | JSLt
  | JSGt
  | JSLe
  | JSGe
  | JSShl
  | JSShr
  | JSAdd
  | JSSub
  | JSMul
  | JSDiv
  | JSMod
  | JSAsgn
  deriving (Show)

-- data JSUnary
--   = JSPlus JSUnary
--   | JSNeg JSUnary
--   | JSBitNeg JSUnary
--   | JSNot JSUnary
--   | JSInc JSUnary
--   | JSDec JSUnary
--   | JSNew JSCons
--   | JSDel JSMember
--   | JSMember JSMember
--   | JSCons JSCons
--   deriving (Show)

data JSUnaryOp
  = JSPlus
  | JSNeg
  | JSBitNeg
  | JSNot
  | JSInc
  | JSDec
  deriving (Show)

deriving instance Generic JSElement

deriving instance Generic JSStm

deriving instance Generic JSVar

deriving instance Generic JSExpr'

deriving instance Generic JSUnaryOp

deriving instance Generic JSBinOp

deriving instance Generic JSMember

deriving instance Generic JSCons

deriving instance Generic JSAtom

deriving instance NFData JSElement

deriving instance NFData JSStm

deriving instance NFData JSVar

deriving instance NFData JSBinOp

deriving instance NFData JSUnaryOp

deriving instance NFData JSExpr'

deriving instance NFData JSMember

deriving instance NFData JSCons

deriving instance NFData JSAtom

-- jsPlus (JSUnary u) = JSUnary (JSPlus u)

-- jsNeg (JSUnary u) = JSUnary (JSNeg u)

-- jsBitNeg (JSUnary u) = JSUnary (JSBitNeg u)

-- jsNot (JSUnary u) = JSUnary (JSNot u)

-- jsInc (JSUnary u) = JSUnary (JSInc u)

-- jsDec (JSUnary u) = JSUnary (JSDec u)

data JSMember
  = JSPrimExp JSAtom
  | JSAccess JSAtom JSMember
  | JSIndex JSAtom JSExpr
  | JSCall JSAtom JSExpr
  deriving (Show)

data JSCons
  = JSQual Text JSCons
  | JSConCall Text JSExpr
  deriving (Show)

data JSAtom
  = JSParens JSExpr
  | JSArray JSExpr
  | JSId Text
  | JSInt Int
  | JSFloat Double
  | JSString Text
  | JSTrue
  | JSFalse
  | JSNull
  | JSThis
  deriving (Show)

jsCondExprBuild :: JSExpr' -> Maybe (JSExpr', JSExpr') -> JSExpr'
jsCondExprBuild c (Just (t, e)) = JSCond c t e
jsCondExprBuild c Nothing = c

jsIdentStart :: Char -> Bool
jsIdentStart c = isAlpha c || c == '_'

jsIdentLetter :: Char -> Bool
jsIdentLetter c = isAlphaNum c || c == '_'

jsUnreservedName :: String -> Bool
jsUnreservedName = \s -> not (member s keys)
  where
    keys =
      fromList
        [ "true",
          "false",
          "if",
          "else",
          "for",
          "while",
          "break",
          "continue",
          "function",
          "var",
          "new",
          "delete",
          "this",
          "null",
          "return",
          "with"
        ]

jsStringLetter :: Char -> Bool
jsStringLetter c = (c /= '"') && (c /= '\\') && (c > '\026')
