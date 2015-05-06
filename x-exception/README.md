x-exception
===========

Unsafe catch combinators for MonadCatch instances

`unsafeBracket :: MonadCatch m => m a -> (a -> m b) -> (a -> m c) -> m c`
`unsafeBracket_ :: MonadCatch m => m a -> m b -> m c -> m c`
`unsafeFinally :: MonadCatch m => m a -> m b -> m a`
