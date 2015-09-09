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
      let getName cls = val ^?! key "teams" . key cls . key "name" . _String . unpacked
          nameA = getName "alpha"
          nameH = getName "bravo"
          victories cls v = Sum $ v ^?! key cls . _Integer
          (Sum va, Sum vh) = foldMapOf (key "wins" . _Array . traverse)
              ((,) <$> victories "alpha" <*> victories "bravo") val
          vs = fromIntegral (va + vh)

      putStrPadding nameA
      printf "%.02f\n" (fromIntegral va / vs :: Float)

      putStrPadding nameH
      printf "%.02f\n" (fromIntegral vh / vs :: Float)
    _ -> putStrLn "Usage: fest-ink index | fest-ink <ordinal>"

  where
    putStrPadding s = do
      putStr s
      putStr $ replicate (16 - length s * 2) ' '

    fetch url = do
      man <- newManager tlsManagerSettings
      req <- parseUrl url
      runResourceT $ do
        response <- http req man
        responseBody response C.$$+- sinkParser json