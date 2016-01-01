# google-reader

```haskell
main :: IO
main = do
  p <- clientLogin readerOptions
  case p of
    Left  e -> print e
    Right r -> subscriptionList readerOptions r >>= print
  where readerOptions = ReaderOptions { root = bazQuxRoot, email = "email", password = "password" }
```
