import           Control.Monad.Trans (liftIO)
import           Data.Maybe
import           Graphics.UI.Gtk
import           System.IO

main :: IO()
main = do
  {-create window with attributes-}
  initGUI
  window <- windowNew
  set window [windowTitle := "hWrite", containerBorderWidth := 10,
              windowDefaultWidth := 800, windowDefaultHeight := 600]

  {-Setup layout stuff-}
  {-table <- tableNew 10 10 True-}
  {-containerAdd window table -}
  let darkgrey = Color 5435 5435 5435
      nearwhite = Color 45535 45535 45535
  wbox <- vBoxNew False 0
  containerAdd window wbox
  mbox <- hBoxNew False 0
  ubox <- hBoxNew False 0
  dbox <- hBoxNew False 0

  menu <- menuBarNew
  fileMenuButton <- menuItemNewWithLabel "File"
  menuShellAppend menu fileMenuButton
  
  fileMenu <- menuNew
  openMenuButton <- menuItemNewWithLabel "Open"
  saveMenuButton <- menuItemNewWithLabel "Save"
  saveAsMenuButton <- menuItemNewWithLabel "Save As"
  mapM_ (menuShellAppend menu) [openMenuButton,saveMenuButton,saveAsMenuButton]

  on fileMenuButton menuItemActivated $ do
    putStrLn "File clicked"
    menuPopup fileMenu Nothing
  boxPackStart mbox menu PackNatural 0
  
  widgetModifyBg ubox StateNormal nearwhite
  {-tableAttachDefaults table ubox 0 10 0 2-}
  {-tableAttachDefaults table dbox 0 10 3 10-}
  tv <- textViewNew
  tvalign <- alignmentNew 0.5 0.5 1 1
  alignmentSetPadding tvalign 10 0 0 0
  widgetModifyFg tvalign StateNormal nearwhite

  tvscrl <- scrolledWindowNew Nothing Nothing
  widgetModifyText tvalign StateNormal nearwhite
  widgetModifyText tvalign StateActive nearwhite	 
  widgetModifyText tvalign StatePrelight nearwhite	 
  widgetModifyText tvalign StateSelected nearwhite
  widgetModifyText tvalign StateInsensitive nearwhite	 
  widgetModifyText tvalign StateInconsistent nearwhite	 
  widgetModifyText tvalign StateFocused nearwhite
  widgetModifyText tvalign StateSelected nearwhite
  widgetModifyBase tvalign StateNormal nearwhite
  widgetModifyBase tvalign StateActive nearwhite	 
  widgetModifyBase tvalign StatePrelight nearwhite	 
  widgetModifyBase tvalign StateSelected nearwhite
  widgetModifyBase tvalign StateInsensitive nearwhite	 
  widgetModifyBase tvalign StateInconsistent nearwhite	 
  widgetModifyBase tvalign StateFocused nearwhite
  widgetModifyBase tvalign StateSelected nearwhite
  widgetModifyBg tvalign StateNormal nearwhite
  widgetModifyBg tvalign StateActive nearwhite	 
  widgetModifyBg tvalign StatePrelight nearwhite	 
  widgetModifyBg tvalign StateSelected nearwhite
  widgetModifyBg tvalign StateInsensitive nearwhite	 
  widgetModifyBg tvalign StateInconsistent nearwhite	 
  widgetModifyBg tvalign StateFocused nearwhite
  widgetModifyBg tvalign StateSelected nearwhite
  widgetModifyFg tvalign StateNormal nearwhite
  widgetModifyFg tvalign StateActive nearwhite	 
  widgetModifyFg tvalign StatePrelight nearwhite	 
  widgetModifyFg tvalign StateSelected nearwhite
  widgetModifyFg tvalign StateInsensitive nearwhite	 
  widgetModifyFg tvalign StateInconsistent nearwhite	 
  widgetModifyFg tvalign StateFocused nearwhite
  widgetModifyFg tvalign StateSelected nearwhite

  containerAdd tvalign tv
  containerAdd tvscrl tvalign
  boxPackStart dbox tvscrl PackGrow 10
  boxPackStart wbox mbox PackNatural 0
  boxPackStart wbox ubox PackNatural 5
  boxPackStart wbox dbox PackGrow 10
  textViewSetPixelsAboveLines tv 0
  textViewSetPixelsBelowLines tv 0
  textViewSetLeftMargin tv 10
  textViewSetRightMargin tv 10
  textViewSetWrapMode tv WrapWordChar
  currentfont <- fontDescriptionNew
  fontDescriptionSetFamily currentfont "Roboto"
  {-fontDescriptionSetWeight currentfont WeightThin-}
  -- fn <- fontDescriptionGetFamily currentfont
  widgetModifyFont tv $ Just currentfont
  widgetModifyFg tv StateNormal nearwhite
  widgetModifyBg tv StateNormal darkgrey
  widgetModifyBg tv StateSelected $ Color 500 500 500
  fontmap <- cairoFontMapGetDefault
  fonts <- pangoFontMapListFamilies fontmap
  cb <- comboBoxNewText
  mapM (comboBoxAppendText cb) (map show fonts)
  comboBoxSetActive cb 22

  set cb [widgetHeightRequest := 35,
	  widgetWidthRequest := 200]
  boxPackStart ubox cb PackNatural 5
  on cb changed $ do
		newfont <- comboBoxGetActiveText cb
		putStrLn $ fromMaybe "Verdana" newfont
		fontDescriptionSetFamily currentfont $ fromMaybe "Verdana" newfont
		widgetModifyFont tv $ Just currentfont
		return ()



  opendialog   <- fileChooserDialogNew Nothing (Just window) FileChooserActionOpen [("Open",ResponseOk),("Cancel",ResponseCancel)]
  saveasdialog <- fileChooserDialogNew Nothing (Just window) FileChooserActionSave [("Save",ResponseOk),("Cancel",ResponseCancel)]
  -- open   <- fileChooserButtonNewWithDialog opendialog

  -- save   <- buttonNewWithLabel "Save"
  
  
  -- saveas <- fileChooserButtonNewWithDialog saveasdialog

  -- boxPackStart ubox open   PackNatural 5
  -- boxPackStart ubox save   PackNatural 5
  -- boxPackStart ubox saveas PackNatural 5





  let openresponse response' = do
        case response' of ResponseOk -> do
                            putStrLn "ResponseOk(Open clicked)"
                            filename <- fileChooserGetFilename opendialog
                            openfile (fromJust filename) tv
                            putStrLn (fromJust filename)
                            widgetHide opendialog
                          _          -> do
                            putStrLn "ResponseCancel(Cancel clicked)"
                            widgetHide opendialog

  
  
  on opendialog response $ openresponse

  let saveaction = do
        file <- fileChooserGetFilename opendialog
        text <- getText tv
        case file of Just x  -> putStrLn x >> writeFile x text
                     Nothing -> do
                       putStrLn "No file open"
                       if (text == "") then do
                                         putStrLn "no text"
                                         messagedialog "No text to save."
                                       else do
                                         putStrLn "has text"
                                         widgetShow saveasdialog
                                         on saveasdialog hideSignal $ do
                                           
                                           newfile <- fileChooserGetFilename saveasdialog
                                           fileChooserSetFilename opendialog (fromJust newfile)
                                           return()
                                         return()
                                           --return (False)
        -- writeFile (fromJust file) text
        return()
  -- on save buttonActivated saveaction
  on saveasdialog response $ \response -> do 
    case response of ResponseOk -> do
                       putStrLn "Ok for saveas"
                       filename <- fileChooserGetFilename saveasdialog
                       text <- getText tv
                       writeFile (fromJust filename) text
                       widgetHide saveasdialog
                       return()
                     _          -> do
                       putStrLn "Other response received"
                       widgetHide saveasdialog


  on openMenuButton menuItemActivated $ widgetShow opendialog
  on saveMenuButton menuItemActivated saveaction
  on saveAsMenuButton menuItemActivated $ widgetShow saveasdialog

  on window deleteEvent $ liftIO $ niceQuit >> return False
  widgetShowAll window
  mainGUI
  return ()

--messagedialog :: String -> [(String,ResponseId)]
messagedialog message = do
  dialog <- messageDialogNew Nothing [] MessageInfo ButtonsOk message
  
  on dialog response $ \response -> do
    widgetHide dialog
    
  widgetShow dialog
  return()

openfile :: TextViewClass self => FilePath -> self -> IO ()
openfile filename tv = do
  handle <- openFile filename ReadWriteMode
  contents <- hGetContents handle
  putStrLn contents
  bf <- textViewGetBuffer tv
  textBufferSetText bf contents
  hClose handle
  return()

getText :: TextView -> IO String
getText tv = do
  bf <- textViewGetBuffer tv
  start <- textBufferGetStartIter bf
  end <- textBufferGetEndIter bf
  text <- textBufferGetText bf start end False
  return(text)

niceQuit :: IO ()
niceQuit = do
  mainQuit

