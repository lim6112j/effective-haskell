module Main(main) where
import Text.Read(readEither)
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr
eval :: Expr -> Int
eval expr =
  case expr of
    Lit n -> n
    Add e1 e2-> eval e1 + eval e2
    Sub e1 e2-> eval e1 - eval e2
    Mul e1 e2-> eval e1 * eval e2
    Div e1 e2-> eval e1 `div` eval e2
parse :: String -> Either String Expr
parse str =
  case parse' (words str) of
    Left err -> Left err
    Right (expr, []) -> Right expr
    Right (_, rest) -> Left $ "Unexpected tokens: " ++ unwords rest
  where parse' :: [String] -> Either String (Expr, [String])
        parse' [] = Left "Unexpected end of input"
        parse' (tok:rest) =
          case tok of
            "+" -> parseBinary Add rest
            "-" -> parseBinary Sub rest
            "*" -> parseBinary Mul rest
            "/" -> parseBinary Div rest
            lit ->
              case readEither lit of
                Left err -> Left err
                Right n -> Right (Lit n, rest)
        parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
        parseBinary op rest =
          case parse' rest of
            Left err -> Left err
            Right (e1, rest') ->
              case parse' rest' of
                Left err -> Left err
                Right (e2, rest'') -> Right (op e1 e2, rest'')
run :: String -> String
run input =
  case parse input of
    Left err -> "Error: " ++ err
    Right expr -> "Result: " ++ show (eval expr)
main :: IO ()
main = do
  print $ eval (Add (Lit 1) (Lit 2))
  print $ eval (Sub (Lit 1) (Lit 2))
  print $ eval (Mul (Lit 2) (Lit 3))
  print $ eval (Div (Lit 6) (Lit 5))
  print $ run "- 10 + 1 * 2 / 8 4"
