module Stack.StackLsDependency where

import Data.Text (Text)

-- |Type for the results of the command `stack ls dependencies`.
data StackLsDependency = StackLsDependency
    { name :: Text
    , version :: Text
    }

fromTuple :: (Text, Text) -> StackLsDependency
fromTuple (n, v) = StackLsDependency { name = n, version = v }
