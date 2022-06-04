
module ScreenshotToClipboard where

import Control.Monad (when)
import GI.Gdk (displayGetDefault)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import GI.Gtk ( clipboardGetDefault, clipboardStore, clipboardSetImage )
import GI.GdkPixbuf (pixbufNewFromFile)

defaultMain :: IO ()
defaultMain =
  withSystemTempDirectory "screenshot-to-clipboard" $ \tempdir -> do
    let imgPath = tempdir </> "image.png"
    (importExitCode, _, _) <- readProcessWithExitCode "import" [imgPath] ""
    when (importExitCode /= ExitSuccess) $
      err "error when calling `import` to take a screenshot"
    imgPixbuf <-
      maybe (err "could not load screenshot image file with gdk pixbuf") pure =<< pixbufNewFromFile imgPath
    display <-
      maybe (err "could not get default display from GDK") pure =<< displayGetDefault
    clipboard <- clipboardGetDefault display
    clipboardSetImage clipboard imgPixbuf
    clipboardStore clipboard
  where
    err :: String -> a
    err e = error $ "screenshot-to-clipboard: " <> e
