{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings #-}
import qualified Data.Conduit as C
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.Text.Lens
import Data.Conduit.Attoparsec
import System.Environment
import Data.Ord
import Data.Foldable (for_)
import Data.Monoid
import Text.Printf
import Data.Time.Clock
import Data.Time.Clock.POSIX

main = do
  getArgs >>= \case
    ["index"] -> do  
      val <- fetch "https://fest.ink/index.json"
      t <- getPOSIXTime
      forOf_ (key "fests" . _Array . traverse) val $ \f -> do
        let i = f ^?! key "id" . _Integer
        putStr $ if
          | f ^?! key "term" . key "in_session" . _Bool -> "[" ++ show i ++ "]"
          | t < fromIntegral (f ^?! key "term" . key "begin" . _Integer) -> "<" ++ show i ++ ">"
          | otherwise -> " " ++ show i ++ " "
        putStr "\t"
        putStrLn $ f ^. key "name" . _String . unpacked
    [i] -> do
      val <- fetch $ "https://fest.ink/" ++ i ++ ".json"
      let nameA = val ^?! key "teams" . key "alpha" . key "name" . _String . unpacked
          nameH = val ^?! key "teams" . key "bravo" . key "name" . _String . unpacked
          (Sum va, Sum vh) = foldMapOf (key "wins" . _Array . traverse)
              (\v -> (Sum $ v ^?! key "alpha" . _Integer, Sum $ v ^?! key "bravo" . _Integer)) val
      let vs = fromIntegral (va + vh)

      putStr nameA
      putStr $ replicate (16 - length nameA * 2) ' '
      printf "%.02f\n" (fromIntegral va / vs :: Float)

      putStr nameH
      putStr $ replicate (16 - length nameH * 2) ' '
      printf "%.02f\n" (fromIntegral vh / vs :: Float)
    _ -> putStrLn "Usage: fest-ink index | fest-ink <ordinal>"

  where
    fetch url = do
      man <- newManager tlsManagerSettings
      req <- parseUrl url
      runResourceT $ do
        response <- http req man
        responseBody response C.$$+- sinkParser json