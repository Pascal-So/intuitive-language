import Text.ParserCombinators.ReadP

-- data Keyword = Is | Function | Of | Colon | Comma | Stop | Exclamation | Question | Do | CurlOpen | CurlClose | AngOpen | AngClose | What | And | Assign | To

data Function = Function [Ratio] Ratio -- coefficients, offset

apply :: Function -> Ratio -> Function
(Function coeffs offset) `apply` n = Function (tail coeffs) (offset + (head * n))

instance Show Function where
  show (Function coeffs offset) =
    intercalate ", " (map display coeffs ++ display offset)

data Op = Plus | Minus | Times | Divide
data Value = Value Ratio | FValue Function [Value] | Subexpr [(Op, Value)]

displayRatio :: Ratio -> String
display r =
  let
    num = numerator r
    den = denominator r
    sden = if den > 1 then "/" ++ (show den) else ""
  in
    show (numerator r) ++ sden

instance Num Value where
  a + b = (eval a) + (eval b)
  a * b = (eval a) * (eval b)
  abs a =
    let Value v = eval a
    in  Value (abs v)
  signum a =
    let Value v = eval a
    in signum v
  fromInteger n = Value (fromInteger n :: Ratio)
  negate a =
    let Value v = eval a
    in Value (negate v)

class Eval a where
  eval :: a -> Value

evaluateMultDiv :: [(Op, Value)] -> [(Op, Value)]
evaluateMultDiv v:vs =
  reverse $ (\(as, a) -> a:as) $ foldl (\(as, (op_t,v1)@a) (op,v2)@v ->
                     if op == Times then
                       (as, (op_t, v1*v2))
                     else if op == Divide then
                       (as, (op_t, v1/v2))
                     else
                       (a:as, v)
                     ) ([], v) vs

evaluatePlusMinus :: [(Op, Value)] -> [(Op, Value)]
evaluatePlusMinus v:vs =
  foldl (\(op_t,v1)@a (op,v2)@v -> if op == Plus then
                                     (op_t, v1+v2)
                                   else
                                     (op_t, v1-v2)
                                   ) v vs

instance Eval Value where
  eval Value v = Value v
  eval FValue (f xs) =
    let Function _ res = foldl apply f xs
    in Value res
  eval Subexpr vs =
    Value $ snd $ evaluatePlusMinus $ evaluateMultDiv vs




whitespaceChar :: ReadP Char
whitespaceChar =
  satisfy (`elem` " \t\n\r")

skipWhitespace :: ReadP ()
skipWhitespace = skipMany whitespaceChar

skipWhitespace1 :: ReadP ()
skipWhitespace1 = skipMany1 whitespaceChar

digit :: ReadP Char
digit =
  satisfy (`elem` ['0'..'9'])

fraction :: ReadP Ratio
fraction = do
  a <- many1 digit
  b <- option 1 $ skipWhitespace >> satisfy (=='/') >> skipWhitespace >> many1 digit
  return $ read a % read b

opChain :: ReadP Value
opChain = do
  v <- fraction
  skipWhitespace
  vs <- many $ do
    skipWhitespace
    op <- satisfy (`elem` "+-*/")
    skipWhitespace
    v <- fraction
    return (op, v)
  return $ (Plus, v):vs

expression :: ReadP Value
expression =
  
