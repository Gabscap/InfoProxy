module GetIS where

import qualified Data.ByteString as BS
import           System.IO.Streams (InputStream)
import qualified System.IO.Streams.ByteString as Streams
import qualified System.IO.Streams.Cereal as Streams

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Fail as Fail

import Data.Int
import Data.Serialize

type IS = InputStream BS.ByteString
newtype GetIS a = GetIS { unGetIS :: IS -> IO a }

instance Functor GetIS where
    fmap f = GetIS . fmap (fmap f) . unGetIS

instance Applicative GetIS where
    pure    = GetIS . const . pure
    {-# INLINE pure #-}
    f <*> x = GetIS $ liftM2 (<*>) (unGetIS f) (unGetIS x)
    {-# INLINE (<*>) #-}
    f  *> x = GetIS $ liftM2 (*>)  (unGetIS f) (unGetIS x)
    {-# INLINE (*>) #-}

instance Alternative GetIS where
    empty   = GetIS $ const empty
    {-# INLINE empty #-}
    x <|> y = GetIS $ liftM2 (<|>) (unGetIS x) (unGetIS y)
    {-# INLINE (<|>) #-}

instance Monad GetIS where
    return  = GetIS . const . return
    {-# INLINE return #-}
    f >>= x = GetIS $ \is -> (unGetIS f $ is) >>= (($ is) . unGetIS . x)
    {-# INLINE (>>=) #-}
    (>>)    = (*>)
    {-# INLINE (>>) #-}
    fail    = GetIS . const . fail
    {-# INLINE fail #-}

instance Fail.MonadFail GetIS where
    fail = GetIS . const . Fail.fail
    {-# INLINE fail #-}

instance MonadPlus GetIS where
    mzero     = GetIS . const $ mzero
    {-# INLINE mzero #-}
    mplus a b = GetIS $ liftM2 mplus (unGetIS a) (unGetIS b)
    {-# INLINE mplus #-}

instance MonadIO GetIS where
    liftIO = GetIS . const

getIS :: Get r -> GetIS r
getIS g = GetIS $ Streams.getFromStream g

runGetIS :: GetIS r -> InputStream BS.ByteString -> IO r
runGetIS = unGetIS

limit :: Int64 -> GetIS r -> GetIS r
limit n (GetIS g) = GetIS $ g <=< Streams.throwIfProducesMoreThan (fromIntegral n)

count :: GetIS r -> GetIS (r, Int64)
count g = GetIS $ \is -> do
    (is', c) <- Streams.countInput is
    res <- runGetIS g is'
    c' <- c
    return (res, c')
