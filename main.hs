{-# LANGUAGE TemplateHaskell, PackageImports #-}
import Prelude hiding (foldr)
import Text.Trifecta
import System.IO
import Control.Monad.State
import Control.Lens
import Data.Foldable
import Control.Applicative
import qualified Data.Map as Map
import qualified Control.Exception as E
import Data.Void
import System.IO.Unsafe
import System.IO.Error
import Data.Monoid

import "combinator-interactive" Data.Combinator

data Command = Eval (Expr String)
    | Run (Expr String)
    | Define String (Expr String)
    | Load FilePath String

parseCommand :: Parser Command
parseCommand = try define <|> run <|> load <|> eval where
    run = do
        string ":run"
        spaces
        Run <$> ccParser
    eval = do
        Eval <$> ccParser
    define = do
        var <- variable
        symbol "="
        Define var <$> ccParser
    load = do
        string ":load"
        path <- stringLiteral
        name <- option "main" $ variable
        return $ Load path name
    
consE x xs = S :$ (S :$ I :$ (K :$ x)) :$ (K :$ xs)

runLazy :: Expr Void -> IO ()
runLazy expr = input >>= output . vacuous . eval . (expr :$)

end = consE (churchNumeral # 256) end

input :: IO (Expr Void)
input = unsafeInterleaveIO $ E.catch g $ \e -> if isEOFError e then return end else fail (show e) where
    g = do
        ch <- getChar
        consE (_Combinator # churchNumeral # enum # ch) <$> input

output :: Expr () -> IO ()
output e = case e `apply` Var () of
    Var () :$ x :$ xs -> case x ^? churchNumeral of
        Just n
            | n >= 256 -> return ()
            | otherwise -> putChar (n ^. enum) >> output xs
        Nothing -> fail "The result is not a number"
    _ -> fail "The result is not a list"

data Env = Env
    { _definitions :: Map.Map String (Expr String)
    }
makeLenses ''Env

applyDefs :: Map.Map String (Expr String) -> Expr String -> Expr String
applyDefs = foldr (.) id . map (uncurry subst) . Map.toList

prompt :: StateT Env IO ()
prompt = do
    lift $ putStr "lazy> "
    lift $ hFlush stdout
    cmd <- lift getLine
    case parseString parseCommand mempty cmd of
        Success a -> case a of
            Eval expr -> do
                m <- use definitions
                lift $ putStrLn $ ccExpression # eval (applyDefs m expr)
            Run expr -> do
                m <- use definitions
                lift $ runLazy (fmap (\x -> error $ "Unexpected free variable: " ++ x) (applyDefs m expr))
                    `E.catch` \(E.ErrorCall msg) -> hPutStrLn stderr msg
                    `E.catch` \e -> hPutStrLn stderr (show (e :: E.IOException))
            Define name expr
                | name `Data.Foldable.elem` expr -> lift $ hPutStrLn stderr "Recursive definitions are not allowed"
                | otherwise -> do
                    m <- use definitions
                    definitions . at name ?= applyDefs m expr
            Load path name -> do
                r <- parseFromFile ccParser path
                case r of
                    Just expr -> do
                        definitions . at name ?= expr
                        lift $ putStrLn $ "Loaded: " ++ name ++ "."
                    Nothing -> return ()
        Failure doc -> lift $ hPutStrLn stderr $ show doc

main = evalStateT (forever prompt) (Env Map.empty)