import Data.Char

class Monad m => MonadZero m where
  zero :: m a

class MonadZero m => MonadPlus m where
  (+++) :: m a -> m a -> m a

newtype Parser a = Parser (String -> [(a,String)])

instance Monad Parser where
  return a = Parser (\cs -> [(a,cs)])
  p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                               (a,cs') <- parse p cs])

instance MonadZero Parser where
  zero = Parser (\cs -> [])

instance MonadPlus Parser where
  p +++ q = Parser (\cs -> parse p cs ++ parse q cs)

parse (Parser p) = p

(++++) :: Parser a -> Parser a -> Parser a
p ++++ q = Parser (\cs -> case parse (p +++ q) cs of
                            []     -> []
                            (x:xs) -> [x])

item :: Parser Char
item = Parser (\cs -> case cs of
                         ""     -> []
                         (c:cs) -> [(c,cs)])

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else zero}

char :: Char -> Parser Char
char c = sat (c ==)

string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do { a <- p; as <- many p; return (a:as)}

space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

apply :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})


-- =======================================

data Exp = Num Int | Add Exp Exp | Mul Exp Exp | Id Char

instance Show Exp where
  show (Num a) = "Num(" ++ (show a) ++ ")"
  show (Id c) = "Id(" ++ (show c) ++ ")"
  show (Add e1 e2) = "Add(" ++ (show e1) ++ (show e2) ++ ")"
  show (Mul e1 e2) = "Mul(" ++ (show e1) ++ (show e2) ++ ")"

num = do {x <- token (sat isDigit); return (Num (ord x - ord '0'))}

ident = do {x <- token (sat (isAlpha)); return (Id x)}

paren = do {symb "("; n <- expr; symb ")"; return n}

multiplicand = paren ++++ num ++++ ident

multiplication = do x1 <- multiplicand
                    symb "*"
                    x2 <- multiplicand
                    return (Mul x1 x2)

summand = paren ++++ multiplication ++++ num ++++ ident

addition = do {x1 <- summand; symb "+"; x2 <- summand; return (Add x1 x2)}

expr = paren ++++ addition ++++ multiplication ++++ num ++++ ident
