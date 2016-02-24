import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.Fix (fix)
import System.Random (randomRIO)
import Data.IORef
import System.IO.Unsafe

{-# NOINLINE danger #-}
danger :: IORef a
danger = unsafePerformIO $ newIORef undefined

{-coerce :: a -> IO b
coerce x = do
  writeIORef danger x
  readIORef danger
-} 
type Msg = (Int, String)
 
main :: IO ()
main = do
    chan <- newChan
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    let glob = 0::Int
    writeIORef danger glob
    listen sock 2
    forkIO $ fix $ \loop -> do
        (_, _) <- readChan chan
        loop
    mainLoop sock chan 0
 
mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr+1
 
runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
    let broadcast msg = writeChan chan (nr, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    
    let list = ["apple", "butterfly", "void"]
    
    name <- liftM init (hGetLine hdl)
    broadcast ("--> " ++ name ++ " entered.")
    hPutStrLn hdl ("Welcome, " ++ name ++ "!.")
    pict <- readIORef danger
    hPutStrLn hdl ("startgame " ++ (show (pict::Int)) ++ ".")
    
    chan' <- dupChan chan
    reader <- forkIO $ fix $ \loop -> do
        (nr', line) <- readChan chan'
        when (nr /= nr') $ hPutStrLn hdl line
        loop
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        putStrLn (name ++ ": " ++ line ++ "sd")
        case line of
         "quit" -> hPutStrLn hdl "Bye!"
         _      -> do
            p <- readIORef danger
            
            case  line == (list !! p) of 
                True -> do 
                    hPutStrLn hdl ("me: " ++ line ++ ".")
                    broadcast (name ++ ": " ++ line ++ ".")
                    r <- randomRIO (0::Int,2::Int)
                    writeIORef danger r
                    putStrLn (show r)
                    hPutStrLn hdl ("YOU WIN!!!!")
                    hPutStrLn hdl ("startgame " ++ (show r) ++ ".")
                    broadcast (name ++ " WIN!!!!")
                    broadcast ("startgame " ++ (show r) ++ ".")
                    loop
                False -> do
                    hPutStrLn hdl ("me: " ++ line ++ ".")
                    broadcast (name ++ ": " ++ line ++ ".")
                    loop
    killThread reader
    broadcast ("<-- " ++ name ++ " left.")
    hClose hdl
