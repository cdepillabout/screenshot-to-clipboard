
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
    imgPixbuf <-
      maybe (err "could not load screenshot image file with gdk pixbuf") pure =<< pixbufNewFromFile imgPath
    display <-
      maybe (err "could not get default display from GDK") pure =<< displayGetDefault
    clipboard <- clipboardGetDefault display
    void $ onClipboardOwnerChange clipboard $ const Gtk.mainQuit
    clipboardSetImage clipboard imgPixbuf
    clipboardStore clipboard
    Gtk.main
  where
    err :: String -> a
    err e = error $ "screenshot-to-clipboard: " <> e
