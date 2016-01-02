# google-reader

```haskell
main :: IO ()
main = do
  p <- clientLogin readerOptions
  case p of
    Left  e -> print e
    Right r -> subscribe readerOptions r subscriptionOptions >>= print
  where
    readerOptions = ReaderOptions { root = bazQuxRoot, email = "email", password = "password" }
    subscriptionOptions = streamId .~ "feed/http://www.engadget.com/rss.xml" $ defaultEditSubscriptionOptions
```
