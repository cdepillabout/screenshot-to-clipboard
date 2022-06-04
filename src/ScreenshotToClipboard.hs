
module ScreenshotToClipboard where

import Control.Monad (when)
import Data.Functor (void)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import GI.Gdk (displayGetDefault)
import GI.GdkPixbuf (pixbufNewFromFile)
import GI.Gtk (clipboardGetDefault, clipboardStore, clipboardSetCanStore, clipboardSetImage, onClipboardOwnerChange)
import qualified GI.Gtk as Gtk
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

defaultMain :: IO ()
defaultMain =
  withSystemTempDirectory "screenshot-to-clipboard" $ \tempdir -> do
    void $ Gtk.init Nothing
    let imgPath = tempdir </> "image.png"

    -- Take screenshot with `import` program from imagemagick.
    (importExitCode, _, _) <- readProcessWithExitCode "import" [imgPath] ""
    when (importExitCode /= ExitSuccess) $
      err "error when calling `import` to take a screenshot"

    -- Load screenshot into GDK Pixbuf.
    imgPixbuf <-
      maybe (err "could not load screenshot image file with gdk pixbuf") pure =<< pixbufNewFromFile imgPath

    -- Get the current display, since it is needed to get the clipboard.
    display <-
      maybe (err "could not get default display from GDK") pure =<< displayGetDefault

    -- Get the clipboard and set hint that it should be able to store.
    clipboard <- clipboardGetDefault display
    clipboardSetCanStore clipboard Nothing

    -- Add callback for when the clipboard contents change.
    numberOfCallsIORef <- newIORef 0
    void $
      onClipboardOwnerChange clipboard $
        const (clipboardOwnerChangeCallback numberOfCallsIORef)

    -- Load the screenshot into the clipboard and store it.
    clipboardSetImage clipboard imgPixbuf
    clipboardStore clipboard

    -- Start GTK main loop.
    Gtk.main
  where
    err :: String -> a
    err e = error $ "screenshot-to-clipboard: " <> e

    -- This callback is called everytime the clipboard changes.  It counts the
    -- number of times it has been called, and it will end the program after
    -- having been called two times.
    --
    -- The first time this function is called will be when this program copies
    -- the screenshot to the clipboard.  This program then hangs around,
    -- waiting for something else to be copied to the clipboard.  It tells the
    -- GTK mainloop to exit only after having been called a second time.
    --
    -- The reason for this is because with XMonad (and other non-Gnome window
    -- managers?), it appears that there is nothing that will persist the
    -- screenshot after copying it to the clipboard if this program exits.  So
    -- this program needs to continue running at leaset until ANOTHER program
    -- actually pastes the screenshot that has been copied to the clipboard.
    --
    -- However, this program gets no notification when another program has
    -- pasted the screenshot.  In order to work around this lack of
    -- information, this program will just keep running until it sees that
    -- something else has been copied to the clipboard.
    clipboardOwnerChangeCallback :: IORef Int -> IO ()
    clipboardOwnerChangeCallback numberOfCallsIORef = do
      newNum <- atomicModifyIORef' numberOfCallsIORef (\num -> (num + 1, num + 1))
      when (newNum >= 2) $ Gtk.mainQuit
