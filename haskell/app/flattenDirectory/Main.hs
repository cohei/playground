-- find <directory> -type d -depth 1 -exec cabal run flattenDirectory -- {} \;
module Main (main) where

import           Control.Monad.IO.Class (MonadIO)
import           Data.Foldable          (for_)
import           Data.Maybe             (fromJust)
import           Path                   (Abs, Dir, File, Path, Rel, dirname,
                                         filename, parent, parseAbsDir,
                                         parseRelDir, toFilePath, (</>))
import           Path.IO                (copyFile, createDirIfMissing,
                                         listDirRecurRel)
import           System.Environment     (getArgs)
import           System.FilePath        (dropTrailingPathSeparator)

main :: IO ()
main = withTargetDir $ \targetDir -> flatten targetDir (outDir targetDir)

withTargetDir :: (Path Abs Dir -> IO ()) -> IO ()
withTargetDir action = do
  args <- getArgs

  case args of
    []      -> putStrLn "flattendirectory <directory>"
    dir : _ -> parseAbsDir dir >>= action

flatten :: Path Abs Dir -> Path Abs Dir -> IO ()
flatten targetDir out = do
  createDirIfMissing True out

  files <- findFiles targetDir
  for_ files $ \file -> copyFile (targetDir </> file) (out </> filename file)

findFiles :: MonadIO m => Path b Dir -> m [Path Rel File]
findFiles path = snd <$> listDirRecurRel path

-- /a/b -> /a/b.flattend
-- outDir :: Path Abs Dir -> Path Abs Dir
-- outDir path = parent path </> outDirName (dirname path)

-- /a/b -> /a.flattened/b
outDir :: Path Abs Dir -> Path Abs Dir
outDir path = parent (parent path) </> outDirName (dirname (parent path)) </> dirname path

outDirName :: Path Rel Dir -> Path Rel Dir
outDirName path =
  fromJust $ parseRelDir $ dropTrailingPathSeparator (toFilePath (dirname path)) <> ".flattened"
