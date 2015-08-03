import Development.Shake
import Development.Shake.FilePath
import System.Directory (createDirectoryIfMissing)


buildDir :: FilePath
buildDir = "_build"

srcDir :: FilePath
srcDir = "src"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir} $ do
    want [buildDir </> "main.jsexe" </> ".built"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

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
