import Control.Monad (when)
import Development.Shake
import Development.Shake.FilePath
import System.Directory (createDirectoryIfMissing)
import System.Posix


buildDir :: FilePath
buildDir = "_build"

srcDir :: FilePath
srcDir = "src"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir} $ do
    want [buildDir </> "all.js"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    phony "min" $ do
        need [buildDir </> "all.min.js"]

    buildDir </> "all.js" %> \out -> do
        need [buildDir </> "main.jsexe" </> ".built"]
        need ["js" </> "exports.js"]
        () <- cmd (FileStdout out)
                "cat"
                [buildDir </> "base.jsexe" </> "rts.js"]
                [buildDir </> "base.jsexe" </> "lib.base.js"]
                [buildDir </> "base.jsexe" </> "out.base.js"]
                [buildDir </> "main.jsexe" </> "lib.js"]
                [buildDir </> "main.jsexe" </> "out.js"]
                ["js" </> "exports.js"]
        return ()

    buildDir </> "all.min.js" %> \out -> do
        need [buildDir </> "all.js"]
        () <- cmd (FileStdout out)
                "ccjs"
                [buildDir </> "all.js"]
                "--language_in=ECMASCRIPT5"
                "--compilation_level=ADVANCED_OPTIMIZATIONS"
        -- ccjs doesn't properly return an exit status for failure, so to check
        -- if there was an error we check the size of the output file
        size <- liftIO $ getFileStatus out >>= return . fileSize
        when (size == 0) $ fail "ccjs error"
        return ()

    buildDir </> "base.jsexe" </> ".built" %> \out -> do
        let baseModule = "Base"
            baseModuleFile = "Base.hs"
            mainModuleFile = "Main.hs"
        need [srcDir </> baseModuleFile]
        liftIO $ createDirectoryIfMissing True buildDir
        () <- cmd "ghcjs"
                  "-DGHCJS_BROWSER"
                  "-O"
                  "-generate-base" [baseModule]
                  ["-i" ++ srcDir]
                  "-odir" [buildDir </> "hsbuild"]
                  "-hidir" [buildDir </> "hsbuild"]
                  "-o" [buildDir </> "base"]
                  [srcDir </> mainModuleFile]
        writeFile' out ""

    buildDir </> "main.jsexe" </> ".built" %> \out -> do
        let mainModuleFile = "Main.hs"
        need [buildDir </> "base.jsexe" </> ".built"]
        allSrcFiles <- getDirectoryFiles "" [srcDir <//> "*.hs"]
        need allSrcFiles
        () <- cmd "ghcjs"
                  "-DGHCJS_BROWSER"
                  "-O"
                  "-no-rts -no-stats"
                  "-use-base" [buildDir </> "base.jsexe" </> "out.base.symbs"]
                  ["-i" ++ srcDir]
                  "-odir" [buildDir </> "hsbuild"]
                  "-hidir" [buildDir </> "hsbuild"]
                  "-o" [buildDir </> "main"]
                  [srcDir </> mainModuleFile]
        writeFile' out ""
