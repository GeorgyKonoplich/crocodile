import Graphics.UI.Gtk
import System.Random (randomRIO)
import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import System.Environment   
import Data.List  

main :: IO ()
main = do
    [host, port] <- getArgs    
    initGUI
    --create main window
    window <- windowNew
    set window [windowTitle := "Crocodile",
            containerBorderWidth := 10,
            windowDefaultWidth := 350, 
            windowDefaultHeight := 400]

    --registration from
    registration <- vBoxNew False 0
    containerAdd window registration

    namefield <- hBoxNew False 0
    boxPackStart registration namefield PackNatural 0

    usernameLbl <- labelNew(Just ("Login"::String))
    boxPackStart namefield usernameLbl PackNatural 21
    name <- entryNew
    boxPackStart namefield name PackNatural 0
    register <- buttonNewWithLabel "Register"
    boxPackEnd namefield register PackNatural 0

    
    --mainBox
    mainBox <- hBoxNew False 0
    --containerAdd window mainBox

    --imageBox
    imageBox <- vBoxNew False 0
    boxPackStart mainBox imageBox PackGrow 0
    
    --chatBox
    chatBox <- vBoxNew False 0
    boxPackStart mainBox chatBox PackGrow 0
    
    --box for writing messages
    hb2 <- hBoxNew False 0
    boxPackEnd chatBox hb2 PackNatural 0

    --button for sending messages
    button <- buttonNewWithLabel "Send"
    boxPackEnd hb2 button PackNatural 0

    --single line text entry widget for writing messages
    txtfield <- entryNew
    boxPackEnd hb2 txtfield PackNatural 5
    
    let picfiles = ["./a.jpg", "./b.jpg", "./c.jpg"]

    slot <- imageNewFromFile (head picfiles)
    set imageBox[containerChild := slot, containerBorderWidth := 10 ]

    --conversation field
    leftBuff <- textBufferNew Nothing
    left <- textViewNewWithBuffer leftBuff
    textViewSetEditable left False  
    scrollL <- scrolledWindowNew Nothing Nothing
    containerAdd scrollL left
    boxPackStart chatBox scrollL PackGrow 0
    
    chan <- newChan
    
    --connecting to server
    hdl <- run host port

    --creating reader thread for sending messages to server
    reader <- forkIO $ fix $ \loop -> do
        line <- readChan chan
        let msg = line ++ "."
        hPutStrLn hdl msg
        loop

    --creating writer thread for receiving messages from server
    writer <- forkIO $ fix $ \loop -> do
        line <- liftM init (hGetLine hdl)
        count <- textBufferGetLineCount leftBuff
        iter <- textBufferGetIterAtLine leftBuff count
        let sg = init line
        --putStrLn sg
        case sg of
         "startgame " -> do
            let lasts = read(drop  ((length line) - 1) line)::Int
            --putStrLn (show lasts)
        
            --r <- randomRIO (0::Int,2::Int)
            imageSetFromFile slot (picfiles !! lasts)
         _       -> do
            textBufferInsert leftBuff iter (line ++ "\n")    
        loop

    --handling signals
    onPressed button (putMsg txtfield chan)
    onEntryActivate txtfield (putMsg txtfield chan)
    onPressed register (game window registration mainBox name hdl) 
    onEntryActivate name (game window registration mainBox name hdl)
    onDestroy window (killAllThreads reader writer hdl)
    
    widgetShowAll window         
    mainGUI

--sd
run :: HostName -> ServiceName -> IO Handle 
run host port = do
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)
    let serverAddr = head addrInfo
    sock <- socket (addrFamily serverAddr) Stream defaultProtocol
    connect sock (addrAddress serverAddr)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    return hdl
    

killAllThreads :: ThreadId -> ThreadId -> Handle -> IO ()
killAllThreads th1 th2 hdl  = do
    killThread th1
    killThread th2
    mainQuit
    hClose hdl

putMsg :: Entry -> Chan String -> IO ()
putMsg txtfield chan = do
    txt <- entryGetText txtfield
    entrySetText txtfield ""
    writeChan chan txt

game :: Window -> VBox -> HBox -> Entry -> Handle -> IO () 
game window reg startGame name hdl = do
    username <- entryGetText name
    containerRemove window reg
    containerAdd window startGame
    widgetShowAll window
    hPutStrLn hdl (username ++ ".")