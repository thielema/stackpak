{-# LANGUAGE OverloadedStrings #-}
module Stack.Stack where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.List.Split
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Stack.StackLsDependency (StackLsDependency)
import qualified Stack.StackLsDependency as StackLsDependency
import Util

-- |Build the stack project to ensure that it builds properly.
stackBuild :: Text -> ExceptT Text IO Text
stackBuild directory = do
    liftIO $ T.putStrLn "Building project..."
    execProcess (mkProcess directory "stack" ["build"])

-- |Get the dot representation of the stack dependencies using `stack dot --external`.
stackDot :: Text -> ExceptT Text IO Text
stackDot directory = execProcess (mkProcess directory "stack" ["dot", "--external"])

-- |Get the dependencies from `stack ls dependencies`.
stackLsDependencies :: Text -> ExceptT Text IO [StackLsDependency]
stackLsDependencies directory = execProcess (mkProcess directory "stack" ["ls", "dependencies"]) >>= liftExceptT . parseStackLsDependencies

-- |Parses the output of `stack ls dependencies`.
-- |TODO: rewrite as parsec Parser.
parseStackLsDependencies :: Text -> Either Text [StackLsDependency]
parseStackLsDependencies str = result
    where
        s a = splitOn (T.unpack " ") (T.unpack a)
        v (x:y:[]) = [(T.pack x, T.pack y)]
        v _ = []
        mr = map (v . s) (T.lines str)
        failedLines = length . filter (== [])
        result = if failedLines mr > 0
            then Left "Failed to parse the result of `stack ls dependencies`."
            else Right $ map StackLsDependency.fromTuple (concat mr)
