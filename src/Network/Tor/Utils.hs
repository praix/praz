module Network.Tor.Utils where

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left $ f x
mapLeft _ (Right v) = Right v

maybeToEither (Just v) _ = Right v
maybeToEither Nothing l = Left l