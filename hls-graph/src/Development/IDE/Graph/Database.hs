
module Development.IDE.Graph.Database(
    ShakeDatabase,
    shakeOpenDatabase,
    shakeRunDatabase,
    shakeProfileDatabase,
    ) where

import Development.IDE.Graph.Internal.Action
import Development.IDE.Graph.Internal.Options
import Development.IDE.Graph.Internal.Rules
import Development.IDE.Graph.Internal.Types
import Data.Maybe
import Data.Dynamic
import Development.IDE.Graph.Internal.Database
import GHC.Conc
import Control.Concurrent.Extra


data ShakeDatabase = ShakeDatabase !Int !Int [Action ()] Database

-- Placeholder to be the 'extra' if the user doesn't set it
data NonExportedType = NonExportedType

shakeOpenDatabase :: ShakeOptions -> Rules () -> IO (IO ShakeDatabase, IO ())
shakeOpenDatabase opts rules = pure (shakeNewDatabase opts rules, pure ())

shakeNewDatabase :: ShakeOptions -> Rules () -> IO ShakeDatabase
shakeNewDatabase opts rules = do
    let extra = fromMaybe (toDyn NonExportedType) $ shakeExtra opts
    (theRules, actions) <- runRules extra rules
    db <- newDatabase extra theRules
    let threads = shakeThreads opts
    threads <- if threads /= 0 then pure threads else getNumProcessors
    pure $ ShakeDatabase threads (length actions) actions db

shakeRunDatabase :: ShakeDatabase -> [Action a] -> IO ([a], [IO ()])
shakeRunDatabase (ShakeDatabase threads lenAs1 as1 db) as2 = withNumCapabilities threads $ do
    incDatabase db
    as <- fmap (drop lenAs1) $ runActions db $ map unvoid as1 ++ as2
    return (as, [])

-- Only valid if we never pull on the results, which we don't
unvoid :: Functor m => m () -> m a
unvoid = fmap undefined

-- Noop
shakeProfileDatabase :: ShakeDatabase -> FilePath -> IO ()
shakeProfileDatabase _ file = writeFile file ""
