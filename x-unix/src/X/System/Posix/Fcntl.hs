{-# LANGUAGE CPP #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module X.System.Posix.Fcntl where

import           Data.Word

import           Foreign.C
import           System.Posix.Types
-- ifdef
import           GHC.Generics (Generic)
import           Foreign.Ptr
import           Foreign.CStorable
import           Foreign.Storable
import           Foreign.Marshal.Utils

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
  f <- new (Fstore 4 3 (fromIntegral off) (fromIntegral len) 0)
  throwErrnoIfMinus1_ "fileAllocate" (c_fcntl (fromIntegral fd) (fromIntegral (42 :: Int)) f)

data Fstore = Fstore {
    fst_flags :: Word32
  , fst_posmode :: CInt
  , fst_offset :: COff
  , fst_lenght :: COff
  , fst_bytesalloc :: COff
  } deriving Generic

instance CStorable Fstore

instance Storable Fstore where
  sizeOf = cSizeOf
  alignment = cAlignment
  poke = cPoke
  peek = cPeek

foreign import capi safe "fcntl.h fcntl"
  c_fcntl :: CInt -> CInt -> Ptr Fstore -> IO CInt

#endif
