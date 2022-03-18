{-# LANGUAGE InstanceSigs #-}
-- реализуйте парсер, сделав его представителем Applicative и Alternative
import Control.Applicative

newtype Parser a = Parser { apply :: String -> [(a, String)] }

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \y -> [(f a, b) | (a, b) <- cs y]

instance Applicative Parser where
  pure :: a -> Parser a
  pure c =  Parser $ \y ->  [(c, y)]
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Alternative Parser where
  empty = Parser $ \_ -> []
  (Parser u) <|> (Parser v) = Parser f where
    f xs = case u xs of
      [] -> v xs
      z -> z <|> v xs