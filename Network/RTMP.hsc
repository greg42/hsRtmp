{-                                                                               
 - ----------------------------------------------------------------------------  
 - "THE BEER-WARE LICENSE" (Revision 42):                                        
 - <code@gregorkopf.de> wrote this file. As long as you retain this notice you   
 - can do whatever you want with this stuff. If we meet some day, and you        
 - think this stuff is worth it, you can buy me a beer in return Gregor Kopf     
 - ----------------------------------------------------------------------------  
 -}                                                                              

{-# LANGUAGE ForeignFunctionInterface #-}

{-|
Module      : Network.RTMP
Description : RTMP Wrapper
Copyright   : (c) Gregor Kopf, 2015
License     : BEER-WARE LICENSE (Revision 42)                                    
Maintainer  : code@gregorkopf.de
Stability   : experimental

A haskell-binding for librtmp.
-}
module Network.RTMP (RTMPErr(..), RTMPHandle, rtmpConnect, rtmpReconnectStream, 
                     rtmpClose, rtmpReadChunk, currentPosition, rtmpDuration, 
                     withChunks, mkRetryHandler, toggleErrorHandler) 
where

import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Control.Concurrent.MVar
import           Control.Monad
import qualified Data.ByteString as BS
import           Control.Applicative

#include <librtmp/rtmp.h>

type CPRTMP_Ctx = Ptr ()
type CPRTMP_Pkt = Ptr ()

-- | A handle to an RTMP connection
data RTMPHandle = RTMPHandle {   hCtx  :: CPRTMP_Ctx
                               , hFree :: MVar Bool
                             }

-- | An RTMP error
data RTMPErr = Timeout | Failed | OK
      deriving (Show)

foreign import ccall unsafe "librtmp/rtmp.h RTMP_Alloc"
   c_RTMP_Alloc :: IO CPRTMP_Ctx

foreign import ccall unsafe "librtmp/rtmp.h RTMP_Init"
   c_RTMP_Init :: CPRTMP_Ctx -> IO ()

foreign import ccall unsafe "librtmp/rtmp.h RTMP_SetupURL"
   c_RTMP_SetupURL :: CPRTMP_Ctx -> CString -> IO CInt

foreign import ccall unsafe "librtmp/rtmp.h RTMP_Connect"
   c_RTMP_Connect :: CPRTMP_Ctx -> CPRTMP_Pkt -> IO CInt

foreign import ccall unsafe "librtmp/rtmp.h RTMP_ConnectStream"
   c_RTMP_ConnectStream :: CPRTMP_Ctx -> CInt -> IO CInt

foreign import ccall unsafe "librtmp/rtmp.h RTMP_ReconnectStream"
   c_RTMP_ReconnectStream :: CPRTMP_Ctx -> CInt -> IO CInt

foreign import ccall unsafe "librtmp/rtmp.h RTMP_Read"
   c_RTMP_Read :: CPRTMP_Ctx -> (Ptr CChar) -> CInt -> IO CInt

foreign import ccall unsafe "librtmp/rtmp.h RTMP_Pause"
   c_RTMP_Pause :: CPRTMP_Ctx -> CInt -> IO CInt

foreign import ccall unsafe "librtmp/rtmp.h RTMP_Close"
   c_RTMP_Close :: CPRTMP_Ctx -> IO ()

foreign import ccall unsafe "librtmp/rtmp.h RTMP_Free"
   c_RTMP_Free :: CPRTMP_Ctx -> IO ()

foreign import ccall unsafe "librtmp/rtmp.h RTMP_GetDuration"
   c_RTMP_GetDuration :: CPRTMP_Ctx -> IO CDouble

foreign import ccall unsafe "librtmp/rtmp.h RTMP_IsTimedout"
   c_RTMP_IsTimedout :: CPRTMP_Ctx -> IO CInt

foreign import ccall unsafe "librtmp/rtmp.h RTMP_ToggleStream"
   c_RTMP_ToggleStream :: CPRTMP_Ctx -> IO CInt

-- | Creates a new RTMP connection and returns a handle.
rtmpConnect ::    String -- ^ RTMP URI to connect to
               -> Int -- ^ Desired seek position
               -> IO RTMPHandle
rtmpConnect uri pos = do
   ctx <- c_RTMP_Alloc
   c_RTMP_Init ctx
   withCString uri $ \cUri -> do
      c_RTMP_SetupURL ctx cUri
   c_RTMP_Connect ctx nullPtr
   c_RTMP_ConnectStream ctx (fromIntegral pos)
   mv <- newMVar False
   return $ RTMPHandle { hCtx = ctx, hFree = mv }

-- | Reconnects a handle to a stream and seek to a desired position. Unless
-- you want to seek in the stream, you don't have to call this function.
rtmpReconnectStream ::  RTMPHandle 
                     -> Int -- ^ Seek position
                     -> IO CInt
rtmpReconnectStream hdl pos = 
   c_RTMP_ReconnectStream (hCtx hdl) (fromIntegral pos)

-- | Closes an RTMP connection.
rtmpClose :: RTMPHandle -> IO ()
rtmpClose hdl = do
   f <- takeMVar (hFree hdl)
   when (not f) $ do
      c_RTMP_Close (hCtx hdl)
      c_RTMP_Free (hCtx hdl)
   putMVar (hFree hdl) True

-- | Attempts to read a given amount of bytes from an RTMP handle.
rtmpReadChunk ::    RTMPHandle -- ^ The handle
                 -> Int -- ^ Desired number of bytes
                 -> IO BS.ByteString -- ^ The result
rtmpReadChunk hdl numBytes = do
   allocaBytes numBytes $ \buf -> do
      rcvd <- c_RTMP_Read (hCtx hdl) buf (fromIntegral numBytes)
      if rcvd >= 0
         then BS.packCStringLen (buf, fromIntegral $ rcvd)
         else return BS.empty

-- | Returns the current position (in milliseconds) in the stream.
currentPosition :: RTMPHandle -> IO Int
currentPosition hdl = do
   x <- (#peek RTMP, m_read.timestamp) (hCtx hdl) :: IO CInt
   return $ fromIntegral x

-- | Returns the total duration (in seconds) of the stream.
rtmpDuration :: RTMPHandle -> IO Double
rtmpDuration hdl = realToFrac <$> c_RTMP_GetDuration (hCtx hdl)

-- | Consumes the stream using two functions: a success function and an
-- error function.
withChunks ::    RTMPHandle -- ^ The handle
              -> (BS.ByteString -> IO ())  -- ^ Success handler
              -> (RTMPErr -> IO RTMPErr) -- ^ Error handler
              -> IO RTMPErr
withChunks hdl suc err = do
   chunk    <- rtmpReadChunk hdl 1024
   curPos   <- currentPosition hdl
   duration <- rtmpDuration hdl
   if (BS.length chunk) /= 0
      then suc chunk >> withChunks hdl suc err 
      else if (fromIntegral curPos) >= 999.0 * duration
         then return OK
         else do
            to <- c_RTMP_IsTimedout (hCtx hdl)
            if (to == 1)
               then err Timeout
               else err Failed

-- | Creates an error handler that will perform an action up to N times
-- in order to un-stall the stream.
mkRetryHandler ::    RTMPHandle 
                  -> (BS.ByteString -> IO ()) 
                  -> Int
                  -> (RTMPHandle -> RTMPErr -> Int -> IO RTMPHandle)
                  -> RTMPErr 
                  -> IO RTMPErr
mkRetryHandler hdl suc maxNum act cause = defaultErrorHandler hdl 0 suc act 
                                                              maxNum cause
   where defaultErrorHandler hdl cnt suc act maxNum error =
            if (cnt < maxNum) 
               then do newH <- act hdl error cnt
                       withChunks newH suc
                                       (defaultErrorHandler hdl (cnt+1) suc act 
                                                            maxNum) 
               else return Failed

-- | An error handler that works by simply toggling the stream (up to three
-- times).
toggleErrorHandler ::    RTMPHandle 
                      -> (BS.ByteString -> IO ()) 
                      -> RTMPErr 
                      -> IO RTMPErr
toggleErrorHandler hdl suc = mkRetryHandler hdl suc 3 $ \h err cnt -> do
   putStrLn "error, will toggle."
   c_RTMP_ToggleStream (hCtx h)
   return h
