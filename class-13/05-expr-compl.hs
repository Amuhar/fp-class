import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative hiding (many, optional)
import Control.Monad


{-
   Добавьте поддержку вещественных и комплексных чисел из второго упражнения.
   Можете считать, что все числа в выражении являются комплексными (и можете
   не считать, если в состоянии красиво обойтись с типами и всё корректно
   проанализировать).
-}

data Expr = Con Int | Bin Op Expr Expr|Complex (Float,Float)| Conf Float
  deriving Show
data Op = Plus | Minus | Mul | Div
  deriving Show
  
--data Complex = Complex (Float, Float)

{-
expr   ::= term {addop term}*
term   ::= factor {mulop factor}*
factor ::= nat | '(' expr ')'
addop  ::= '+' | '-'
mulop  ::= '*' | '/'
-}
float :: Parser Float
float = fr <|> fn
    where 
       fr =  do 
                 n <- integer
                 char '.'
                 m <- natural
                 return $ read (show n ++ "." ++ show m) 
       fn = do
                n <- integer
                return (read (show n) :: Float)


complex :: Parser (Float, Float)
complex = do
            string "(" 
            r <- float
            char ','
            m <- float
            string ")"
            return (r,m)



expr = token (term >>= rest addop term)
  where
    rest op unit e1 = optional e1 $ do 
        p <- op
        e2 <- unit
        rest op unit $ Bin p e1 e2
    term = token (factor >>= rest mulop factor)
    factor = token (constant <|> bracket "(" ")" expr)
    addop = binop ("+", Plus) ("-", Minus)
    mulop = binop ("*", Mul) ("/", Div)
    binop (s1, cons1) (s2, cons2) =
          (symbol s1 >> return cons1) <|>
          (symbol s2 >> return cons2)
    constant = Complex `liftM` complex <|> Conf `liftM` float -- <|> Con `liftM` natural  
	
-- apply float "5" -> 5.0 

