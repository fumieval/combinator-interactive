{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, TemplateHaskell #-}
module Data.Combinator where

import Prelude hiding (elem, notElem, foldl, foldr, length)
import Data.Foldable
import Control.Lens
import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lift
import Data.Void
import Text.Trifecta
import Data.List (minimumBy)
import Data.Function (on)
import Data.Monoid

infixl 9 :$

data Expr e = Expr e :$ Expr e -- application
            | I | K | S -- primitive combinators
            | Var e -- external value
              deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

deriveLift ''Expr

instance Monad Expr where
    return = Var
    Var x >>= f = f x
    (a :$ b) >>= f = (a >>= f) :$ (b >>= f)
    I >>= _ = I
    K >>= _ = K
    S >>= _ = S

-- Basic Functions
-- | The 'length' function returns a length of an expression. 
length :: Expr e -> Int
length (f :$ g) = length f + length g + 1
length _ = 1

isPrim :: Expr e -> Bool
isPrim S = True
isPrim K = True
isPrim I = True
isPrim x = False

subst :: Eq e => e -- free variable to search for
    -> Expr e
    -> Expr e -> Expr e
subst v r (f :$ g) = subst v r f :$ subst v r g
subst v r (Var v') | v == v' = r
subst _ _ e = e

-- | The 'bindee' function transforms an expression to a combinator which binds specified variable when it is applied.
bindee :: Eq e => e -> Expr e -> Expr e
-- refered to John Tromp "Binary Lambda Calculus and Combinatory Logic", 2011 (http://homepages.cwi.nl/~tromp/cl/LC.pdf section 3.2)
bindee _ (S :$ K :$ _) = S :$ K
bindee x f              | x `notElem` toList f = K :$ f
bindee x (Var x')      | x == x' = I
bindee x (f :$ Var x') | x `notElem` toList f && x == x' = f
bindee x (Var y :$ f :$ Var z)
    | x == y && x == z = bindee x $ S :$ S :$ K :$ Var x :$ f
bindee x (f :$ (g :$ h))
    | isPrim f && isPrim g = bindee x $ S :$ bindee x f :$ g :$ h
bindee x ((f :$ g) :$ h)
    | isPrim f && isPrim h = bindee x $ S :$ f :$ bindee x h :$ g
bindee x ((f :$ g) :$ (h :$ g'))
    | isPrim f && isPrim h && g == g' = bindee x $ S :$ f :$ h :$ g
bindee x (f :$ g) = S :$ bindee x f :$ bindee x g

apply :: Expr e -> Expr e -> Expr e
apply I x = x
apply (K :$ x) y = x
apply (S :$ x :$ y) z = apply x z `apply` apply y z
apply f g = f :$ g

eval :: Expr e -> Expr e
eval (f :$ g) = eval f `apply` eval g
eval x = x

unlambdaParser :: Parser (Expr String)
unlambdaParser = char '`' *> ((:$) <$> unlambdaParser <*> unlambdaParser)
    <|> char 's' *> pure S
    <|> char 'k' *> pure K
    <|> char 'i' *> pure I
    <|> Var <$> (char '[' *> some (satisfy (/=']')) <* char ']')

ccParser :: Parser (Expr String)
ccParser = token $ foldl (:$) <$> term <*> many term where
    term = token $ parens ccParser
        <|> S <$ char 'S'
        <|> K <$ char 'K'
        <|> I <$ char 'I'
        <|> lambda
        <|> vacuous <$> stringLit
        <|> vacuous <$> intLit
        <|> Var <$> variable

variable = token $ liftA2 (:) lower (many alphaNum)

lambda :: Parser (Expr String)
lambda = do
    symbol "\\"
    v <- variable
    symbol "."
    bindee v <$> ccParser

stringLit :: Parser (Expr Void)
stringLit = fmap (error "stringLit is buggy") $ bindee "C" <$> bindee "N"
    <$> foldr (\x y -> Var "C" :$ x :$ y) (Var "N")
    <$> map (encodeInt.fromEnum)
    <$> (stringLiteral :: Parser String)

intLit :: Parser (Expr Void)
intLit = encodeInt <$> fromEnum <$> natural

cc :: QuasiQuoter
cc = QuasiQuoter { quoteExp = \s -> case parseString ccParser mempty s of
    Success a -> lift a
    Failure err -> fail $ show err
    , quoteType = const $ fail "Unsupported"
    , quoteDec = const $ fail "Unsupported"
    , quotePat = const $ fail "Unsupported"  }

showCC :: Expr String -> String
showCC = snd . go False where
    go _ I = (False, "I")
    go _ K = (False, "K")
    go _ S = (False, "S")
    go _ (Var x) = (True, x)
    go p (a :$ b)
        | p = fmap (\x -> "(" ++ x ++ ")") s
        | otherwise = s
        where
            s = case (go False a, go True b) of
                ((True, l), (True, r)) -> (True, l ++ " " ++ r)
                ((_, l), (_, r)) -> (False, l ++ r)

ccExpression :: Prism' String (Expr String)
ccExpression = prism' showCC (preview _Success . parseString (ccParser<*eof) mempty)

churchNumeral :: Prism' (Expr a) Int
churchNumeral = prism' encodeInt ((>>= decodeInt) . preview _Combinator)

encodeInt :: Int -> Expr a
encodeInt 0 = K :$ I
encodeInt 1 = I
encodeInt 2 = S :$ (S :$ (K :$ S) :$ K) :$ I
encodeInt n
    | mod n 2 == 0 = n'
    | otherwise = S :$ (S :$ (K :$ S) :$ K) :$ n'
    where
        n' = S :$ (K :$ encodeInt 2) :$ encodeInt (div n 2)

decodeInt :: Expr Void -> Maybe Int
decodeInt e = go $ vacuous e `apply` Var True `apply` Var False where
    go (Var True :$ x) = succ <$> go x
    go (Var False) = Just 0
    go _ = Nothing

_Combinator :: Prism' (Expr a) (Expr Void)
_Combinator = prism' vacuous (traverse (const Nothing)) where