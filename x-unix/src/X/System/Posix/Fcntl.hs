{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module X.System.Posix.Fcntl where

import           Foreign.C
import           System.Posix.Types

-- | Performs @posix_fallocate(2)@ operation on file-descriptor.
--
-- Throws 'IOError' (\"unsupported operation\") if platform does not
-- provide @posix_fallocate(2)@.
--
-- (use @#if HAVE_POSIX_FALLOCATE@ CPP guard to detect availability).
--
-- /Since: 2.7.1.0/
fileAllocate :: Fd -> FileOffset -> FileOffset -> IO ()
#if HAVE_POSIX_FALLOCATE
fileAllocate fd off len = do
  throwErrnoIfMinus1_ "fileAllocate" (c_posix_fallocate (fromIntegral fd) (fromIntegral off) (fromIntegral len))

foreign import capi safe "fcntl.h posix_fallocate"
  c_posix_fallocate :: CInt -> COff -> COff -> IO CInt
#else

fileAllocate fd off len = do
  throwErrnoIfMinus1_ "fileAllocatezz" (c_xox (fromIntegral fd) (fromIntegral off) (fromIntegral len))

foreign import ccall safe "Test.h xox"
  c_xox :: CInt -> COff -> COff -> IO CInt

#endif
