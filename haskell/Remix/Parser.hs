import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Token (GenTokenParser)
import Text.Parsec.IndentParsec(runGIPT)

import Control.Monad.Identity
import Text.Parsec.Token (makeTokenParser, GenLanguageDef(..))
import qualified Text.Parsec.IndentParsec.Token as IT
import Text.Parsec.IndentParsec.Prim

data Expr = Const Integer
          | Var String
          | WhereClause Expr [Binding] deriving Show

type Binding = (String, Expr)

type HaskellLikeIndent = IndentT HaskellLike Identity

langDef :: GenLanguageDef String () HaskellLikeIndent
langDef = LanguageDef { commentStart = "{-"
                      , commentEnd   = "-}"
                      , commentLine  = "--"
                      , identStart = letter   <|> char '_'
                      , identLetter = alphaNum <|> char '_'
                      , opStart = oneOf "-+/*=<>"
                      , opLetter = oneOf "-+/*=<>"
                      , reservedNames = [ "where" ]
                      , reservedOpNames = [ "=" , "+" , "-", "*", "/"]
                      , caseSensitive = False
                      , nestedComments = True
                      }

tokP :: IT.GenIndentTokenParser HaskellLike String () Identity
tokP = makeTokenParser langDef

identifier = IT.identifier tokP
integer = IT.integer tokP
semiSep = IT.semiSepOrFoldedLines tokP
kwWhere = IT.reserved tokP "where"
assigns = IT.reservedOp tokP "="


main = do inp <- getContents
          let x = runGIPT prog () "<stdin>" inp
              in case runIdentity x of
                      Right bs -> sequence_ $ map print bs
                      Left e -> do putStr "parse error: "
                                   print e

prog :: IndentParsecT String () Identity [Binding]
prog = do IT.whiteSpace tokP
          semiSep binding

expression = fmap Const integer
             <|> fmap Var identifier
             <|> do e <- expression
                    kwWhere
                    bs <- IT.bracesBlock tokP bindings
                    return $ WhereClause e bs

compoundExpression = do e <- expression
                        whereBlock e

whereBlock e = do try kwWhere
                  bs <- IT.bracesBlock tokP bindings
                  return $ WhereClause e bs
               <|> return e

bindings = semiSep binding
binding = do x <- identifier
             assigns
             e <- compoundExpression
             return (x,e)
