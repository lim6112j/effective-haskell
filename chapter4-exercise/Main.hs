module Main(main) where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a) deriving (Show)
showStringTree :: BinaryTree String -> String
showStringTree Leaf = "leaf"
showStringTree (Branch left a right) = "(" ++ showStringTree left ++ a ++ showStringTree right ++ ")"
aBinaryTree :: BinaryTree String
aBinaryTree = Branch Leaf "Haskell!" Leaf
addElementIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementIntTree Leaf b = Branch Leaf b Leaf
addElementIntTree (Branch left a right) b
  | b < a = Branch (addElementIntTree left b) a right
  | b > a = Branch left a (addElementIntTree right b)
  | otherwise = Branch left a right
aBinaryTreeInt :: BinaryTree Int
aBinaryTreeInt = addElementIntTree (Branch Leaf 2 Leaf) 4

doesExistInt :: BinaryTree Int -> Int -> Bool
doesExistInt Leaf _ = False
doesExistInt (Branch left a right) b
  | b < a = doesExistInt left b
  | b > a = doesExistInt right b
  | otherwise = True
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr deriving (Show)
eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2
safeEval :: Expr -> Either String Int
safeEval (Lit n) = Right n
safeEval (Add e1 e2) = Right (eval (Add e1 e2))
safeEval (Sub e1 e2) = Right (eval (Sub e1 e2))
safeEval (Mul e1 e2) = Right (eval (Mul e1 e2))
safeEval (Div e1 e2) = if eval e2 == 0 then Left "Division by zero" else Right (eval (Div e1 e2))
prettyPrint :: Expr -> String
prettyPrint expr =
  prettyPrint' expr <> " = " <> show (eval expr)
        where prettyPrint' expr =
                case expr of
                  Lit n -> show n
                  Add e1 e2 -> "(" <> prettyPrint' e1 <> " + " <> prettyPrint' e2 <> ")"
                  Sub e1 e2 -> "(" <> prettyPrint' e1 <> " - " <> prettyPrint' e2 <> ")"
                  Mul e1 e2 -> "(" <> prettyPrint' e1 <> " * " <> prettyPrint' e2 <> ")"
                  Div e1 e2 -> "(" <> prettyPrint' e1 <> " / " <> prettyPrint' e2 <> ")"
main :: IO ()
main = do
  print aBinaryTree
  print $ addElementIntTree aBinaryTreeInt 6
  print $ doesExistInt aBinaryTreeInt 6
  print $ safeEval (Div (Lit 4) (Lit 0))
  print $ safeEval (Div (Lit 4) (Lit 2))
  print $ prettyPrint ( (Lit 24) `Div` (Lit 2 `Mul` Lit 3))
  print $ prettyPrint $ Lit 14 `Mul` (Lit 5 `Add` (Lit 10 `Div` Lit 2))
