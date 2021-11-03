{
-- Let Var = Exp in Exp
-- Let x = 3+5 in 7*x
module Main where
import Data.Char
}

%name parser
%tokentype { Token }
%error { parseError }

%token
      Let             { TokenLet }
      num             { TokenNum $$ }
      var             { TokenVar $$}
      in              { TokenIn }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }
      '='             { TokenEq}
%%

ExpLet  : Let  var '=' ExpLet in ExpLet { Let $2 $4 $6}
        | Exp                     { Exp $1 }

Exp  : Exp '+' Term           { Plus $1 $3 }
     | Exp '-' Term           { Minus $1 $3 }
     | Term                   { Term $1 }

Term  : Term '*' Factor         { Times $1 $3 }
      | Term '/' Factor         { Div $1 $3 }
      | Factor                  { Factor $1 }

Factor
      : num                     { Num $1 }
      | var                     { Var $1}
      | '(' Exp ')'             { Brack $2 }


{

parseError :: [Token] -> a
parseError _ = error "Parse error"

data ExpLet
      =Let String ExpLet ExpLet
      | Exp Exp
      deriving Show

data Exp
      = Plus Exp Term
      | Minus Exp Term
      | Term Term
      deriving Show

data Term
      = Times Term Factor
      | Div Term Factor
      | Factor Factor
      deriving Show

data Factor
      = Num Int
      | Var String
      | Brack Exp
      deriving Show


data Token
      = TokenLet
      | TokenIn
      | TokenEq
      | TokenVar String
      | TokenNum Int
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenOB
      | TokenCB
      deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs

lexNum cs = TokenNum (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
  case span isAlpha cs of
      ("Let", rest) -> TokenLet : lexer rest
      ("in", rest) -> TokenIn : lexer rest
      (var, rest) -> TokenVar var : lexer rest

main = getContents >>= print . parser . lexer

}
