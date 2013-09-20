{-# LANGUAGE TemplateHaskell, PackageImports, FlexibleInstances #-}
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
import System.IO.Error hiding (try)
import Data.Monoid
import qualified Data.ByteString as BS
import System.Directory
import qualified Data.Serialize as S
import System.Environment

import "combinator-interactive" Data.Combinator

data Command = Eval (Expr String)
    | Run (Expr String)
    | Define String (Expr String)
    | Load FilePath String
    | Save FilePath (Expr String)
    | Del String
    | Quit

parseCommand :: Parser Command
parseCommand = try define <|> run <|> load <|> save <|> del <|> quit <|> eval where
    run = do
        symbol ":run"
        Run <$> ccParser
    eval = do
        Eval <$> ccParser
    define = do
        var <- variable
        symbol "="
        Define var <$> ccParser
    load = do
        symbol ":load"
        path <- stringLiteral
        name <- option "main" $ variable
        return $ Load path name
    save = do
        symbol ":save"
        path <- stringLiteral
        expr <- ccParser
        return $ Save path expr
    del = do
        symbol ":del"
        Del <$> variable
    quit = Quit <$ symbol ":quit"
    
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
                prompt
            Run expr -> do
                m <- use definitions
                lift $ runLazy (fmap (\x -> error $ "Unexpected free variable: " ++ x) (applyDefs m expr))
                    `E.catch` \(E.ErrorCall msg) -> hPutStrLn stderr msg
                    `E.catch` \e -> hPutStrLn stderr (show (e :: E.IOException))
                prompt
            Define name expr
                | name `Data.Foldable.elem` expr -> do
                    lift $ hPutStrLn stderr "Recursive definitions are not allowed"
                    prompt
                | otherwise -> do
                    m <- use definitions
                    definitions . at name ?= applyDefs m expr
                    dump
                    prompt
            Load path name -> do
                r <- parseFromFile ccParser path
                case r of
                    Just expr -> do
                        definitions . at name ?= expr
                        dump
                        lift $ putStrLn $ "Loaded: " ++ name ++ "."
                        prompt
                    Nothing -> prompt
            Save path expr -> do
                m <- use definitions
                lift $ writeFile path $ ccExpression # eval (applyDefs m expr)
                prompt
            Del name -> do
                definitions . at name .= Nothing
                prompt
            Quit -> return ()
        Failure doc -> do
            lift $ hPutStrLn stderr $ show doc
            prompt

dump = do
    path <- lift $ (++"/.lazyi-env") <$> getHomeDirectory
    env <- get
    lift $ BS.writeFile path $ S.encode env

defaultEnv = Env Map.empty

instance S.Serialize (Expr String) where
    put expr = S.put (ccExpression # expr)
    get = S.get >>= maybe (fail "parse error") return . preview ccExpression

instance S.Serialize Env where
    put (Env m) = S.put m
    get = Env <$> S.get

main = getArgs >>= \r -> case r of
    [] -> do
        path <- (++"/.lazyi-env") <$> getHomeDirectory
        b <- doesFileExist path
        let w = hPutStrLn stderr "Warning: .lazyi-env is broken." >> return defaultEnv
        env <- if b
            then S.decode <$> BS.readFile path >>= either (const w) return
            else return defaultEnv
        evalStateT prompt env