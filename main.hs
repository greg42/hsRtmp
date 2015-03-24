import           Network.RTMP
import qualified Data.ByteString as BS
import           System.Environment
import           System.IO

main = do
   (uri : outFile : _) <- getArgs
   hdl <- rtmpConnect uri 0
   fh  <- openFile outFile WriteMode
   let writeToFile = BS.hPut fh
   withChunks hdl writeToFile (toggleErrorHandler hdl writeToFile)
   putStrLn "Done."
