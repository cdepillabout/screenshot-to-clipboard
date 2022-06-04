
module ScreenshotToClipboard where

import Control.Monad (when)
import Data.Functor (void)
import GI.Gdk (displayGetDefault)
import GI.GdkPixbuf (pixbufNewFromFile)
import GI.Gtk (clipboardGetDefault, clipboardStore, clipboardSetImage, onClipboardOwnerChange)
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
    (importExitCode, _, _) <- readProcessWithExitCode "import" [imgPath] ""
    when (importExitCode /= ExitSuccess) $
      err "error when calling `import` to take a screenshot"
    isEventsPending <- Gtk.eventsPending
    putStrLn $ "is events pending before get imgPixbuf? " <> show isEventsPending
    imgPixbuf <-
      maybe (err "could not load screenshot image file with gdk pixbuf") pure =<< pixbufNewFromFile imgPath
    isEventsPending <- Gtk.eventsPending
    putStrLn $ "is events pending before get display? " <> show isEventsPending
    display <-
      maybe (err "could not get default display from GDK") pure =<< displayGetDefault
    isEventsPending <- Gtk.eventsPending
    putStrLn $ "is events pending before get clipboard? " <> show isEventsPending
    clipboard <- clipboardGetDefault display
    isEventsPending <- Gtk.eventsPending
    putStrLn $ "is events pending before set on clipboard owner change callback? " <> show isEventsPending
    void $ onClipboardOwnerChange clipboard $ \_ -> do
      isEventsPending <- Gtk.eventsPending
      putStrLn $ "onclipboard change callback, is events pending before mainQuit? " <> show isEventsPending
      -- Gtk.mainQuit
    isEventsPending <- Gtk.eventsPending
    putStrLn $ "is events pending before get clipboard set image? " <> show isEventsPending
    clipboardSetImage clipboard imgPixbuf
    isEventsPending <- Gtk.eventsPending
    putStrLn $ "is events pending before get clipboard store? " <> show isEventsPending
    clipboardStore clipboard
    isEventsPending <- Gtk.eventsPending
    putStrLn $ "is events pending before run main? " <> show isEventsPending
    Gtk.main
  where
    err :: String -> a
    err e = error $ "screenshot-to-clipboard: " <> e
