import Test.QuickCheck
import Control.Monad

absAverage1 :: [Double] -> Double
absAverage1 ds = sum ds / fromIntegral (length ds)

quickCheck1 :: [Double] -> Bool
quickCheck1 = \x -> absAverage1 x >= 0

absAverage2 :: [Double] -> Double
absAverage2 ds = sum (map abs ds) / fromIntegral (length ds)

quickCheck2 :: [Double] -> Property
quickCheck2 = \x -> length x > 1 ==> absAverage2 x >= 0

type Username = String
type MessageId = Integer
type Message = String

data UserAction
  = Login Username
  | ViewTweet MessageId
  | PostTweet Message
  deriving (Show)

randomAction :: Gen UserAction
randomAction = oneof [ liftM Login arbitrary
                     , liftM ViewTweet arbitrary
                     , liftM PostTweet arbitrary
                     ]

instance Arbitrary UserAction where
  arbitrary = randomAction

main :: IO ()
main = do
  putStrLn "Hello QuickCheck"
  quickCheck quickCheck1
  quickCheck quickCheck2

--
-- Big thanks to
-- https://ocharles.org.uk/blog/posts/2012-12-08-24-days-of-hackage.html
--
