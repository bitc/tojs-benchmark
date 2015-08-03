import Development.Shake
import Development.Shake.FilePath


buildDir :: FilePath
buildDir = "_build"

srcDir :: FilePath
srcDir = "src"

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles=buildDir} $ do
    want [buildDir </> "main.js"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    buildDir </> "bower_components" </> ".built" %> \out -> do
        need ["bower.json"]
        liftIO $ removeFiles (buildDir </> "bower_components") ["//*"]
        () <- cmd "bower"
                  "--allow-root"
                  "--config.analytics=false"
                  ["--config.directory=" ++ (buildDir </> "bower_components")]
                  "install"
        writeFile' out ""

    buildDir </> "output" </> ".built" %> \out -> do
        need [buildDir </> "bower_components" </> ".built"]
        allSrcFiles <- getDirectoryFiles "" [ srcDir <//> "*.purs",
                                              srcDir <//> "*.js" ]
        need allSrcFiles
        () <- cmd "psc"
                  [buildDir </> "bower_components" </> "*" </> "src" </> "**" </> "*.purs"]
                  "--ffi" [buildDir </> "bower_components" </> "*" </> "src" </> "**" </> "*.js"]
                  [srcDir </> "**" </> "*.purs"]
                  "--ffi" [srcDir </> "**" </> "*.js"]
                  "-o" [buildDir </> "output"]
        writeFile' out ""

    buildDir </> "main.js" %> \out -> do
        need [buildDir </> "output" </> ".built"]
        () <- cmd "psc-bundle"
                  [buildDir </> "output" </> "**" </> "*.js"]
                  "-m" "Main"
                  "-o" [out]
        return ()
