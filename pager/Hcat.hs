module HCAT(runHCat) where
import qualified System.Environment as Env
import Prelude hiding (FilePath)
getArgs :: IO [String]
getArgs = Env.getArgs
type FilePath = String
handleArgs :: IO (Either String FilePath)


runHCat :: IO ()
runHCat = handleArgs >>= print
