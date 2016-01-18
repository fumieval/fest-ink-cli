{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, DeriveTraversable, Rank2Types, ViewPatterns #-}
import Control.Applicative
import Control.Monad.Catch
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Data.Conduit.Attoparsec
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lens
import Network.HTTP.Conduit
import Network.HTTP.Types
import qualified Data.Conduit as C
import System.Environment
import Text.Printf

data SquidSisters a = SquidSisters !a !a deriving (Functor)

instance Monoid a => Monoid (SquidSisters a) where
  mempty = pure mempty
  mappend = liftA2 mappend

instance Applicative SquidSisters where
  pure a = SquidSisters a a
  SquidSisters f g <*> SquidSisters a b = SquidSisters (f a) (g b)

alphaBravo :: Applicative f => (Text -> f a) -> f (SquidSisters a)
alphaBravo f = SquidSisters <$> f "alpha" <*> f "bravo"

evalResult :: Value -> Maybe (SquidSisters Double)
evalResult val = do
  [votes, win] <- forM ["vote", "win"] $ \ty -> alphaBravo $ \k -> val ^? key ty . key k . _Double
  mult <- val ^? key "win" . key "multiply" . _Double
  return $ (+) <$> votes <*> fmap (mult*) win

strWidth :: String -> Int
strWidth = sumOf $ traverse . to (\ch -> if isLatin1 ch then 1 else 2)

teamName :: Text -> Traversal' Value String
teamName k = key "teams" . key k . key "name" . _String . unpacked

fetchResults :: Integer -> IO (Value, SquidSisters Double)
fetchResults i = do
  val <- fetch $ "https://fest.ink/" ++ show i ++ ".json"
  let victories v cls = [v ^. key cls . _Double . to Sum]
  return $ (,) val $ fmap getSum $ val ^. key "wins" . _Array . traverse . folding (alphaBravo . victories)

fetch :: String -> IO Value
fetch url = do
  man <- newManager tlsManagerSettings
  req <- parseUrl url
  runResourceT $ do
    resp <- http req man `catch` \case
      StatusCodeException (Status i msg) _ _ -> fail $ show i ++ " " ++ BC.unpack msg
      e -> throwM e
    responseBody resp C.$$+- sinkParser json

main :: IO ()
main = getArgs >>= \case
  [str] | Just i <- str ^? _Show -> do
    (val, SquidSisters a b) <- fetchResults i
    putStr $ val ^. key "name" . _String . unpacked
    case val ^?! key "term" . key "status". _String of
      "closed" -> putStrLn " (Concluded)"
      "in session" -> putStrLn " (Open)"
      "scheduled" -> putStrLn " (Upcoming)"
      _ -> error "Unknown status"
    forMOf_ (key "term") val $ \term -> do
      printf "From: %s\n" $ term ^. key "begin_s" . _String
      printf "To:   %s\n" $ term ^. key "end_s" . _String
    forM_ [("alpha", a), ("bravo", b)] $ \(k, n) -> do
      printf "%s %.0f (%.0f%%)\n" (val ^. teamName k) n (100 * n / (a + b))
  [] -> do
    val <- fetch "https://fest.ink/index.json"
    forOf_ (key "fests" . _Array . traverse) val $ \f -> do
      let i = f ^?! key "id" . _Integer

      putStr $ show i ++ replicate (4 - length (show i)) ' '

      let result = f ^? key "result" . nonNull . folding evalResult

      status <- case f ^?! key "term" . key "status". _String of
        "closed" -> case result of
            Just (SquidSisters a b) -> return $ case compare a b of
              LT -> "(Winner: " ++ f ^. teamName "bravo" ++ ")"
              EQ -> "(Even)"
              GT -> "(Winner: " ++ f ^. teamName "alpha" ++ ")"
            Nothing -> error "No winner?"
        "in session" -> do
            (_, SquidSisters a b) <- fetchResults i
            return $ printf "(%.2f | %.2f)" (a / (a + b)) (b / (a + b))
        "scheduled" -> return "(Upcoming)"
        _ -> error "Unknown status"

      putStr status

      putStr $ replicate (32 - strWidth status) ' '

      putStrLn $ f ^. key "name" . _String . unpacked
  _ -> putStrLn "Usage: fest-ink [ordinal]"