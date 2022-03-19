--Функция parseHex пытается разобрать 
--переданную ей строку как шестнадцатеричное число. 
--При удачном исходе она возвращает это число, 
--а при неудачном --- генерирует исключение. 
--Функция printError выводит информацию об этом исключении 
--в удобном текстовом виде. Для тестирования используйте
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except

data ParseError = ParseError { location :: Int, reason :: String }

type ParseMonad = Either ParseError
hexChar :: Char -> Integer
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | otherwise     = -1
checkDigit pos y = do
    if hexChar y >= 0
        then
            return (hexChar y)    
        else
            throwError (ParseError pos [y])    
parseHex :: String -> ParseMonad Integer
parseHex s = parserHex s 1 (length s) 0
parserHex [] n l d  = return d
parserHex (x:xs) n l d = do
    y <- checkDigit n x
    parserHex xs (n+1) l (y + 16*d)

printError :: ParseError -> ParseMonad String        
printError e =  Right $ "At pos "++ show (location e) ++": "++ reason e ++": invalid digit"

-- тестирование
test s = str where
  (Right str) = do 
      n <- parseHex s
      return $ show n  
    `catchError` printError