import Control.Exception (SomeException, catch)
import HaskellSay (haskellSay)
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

-- Figletを使用するための関数
figletConsole :: String -> IO ()
figletConsole text = do
  let message = text -- 本来はFigletの呼び出しを含むことになります
  haskellSay message

-- ユーザーからディレクトリ名を入力して取得
getDirectoryName :: IO String
getDirectoryName = do
  putStr "Please enter directory name: "
  hFlush stdout
  input <- getLine
  if null input
    then do
      figletConsole "Panic!"
      exitFailure
    else return input

-- メイン関数
main :: IO ()
main = do
  -- ディレクトリ名を取得
  directoryName <- catch getDirectoryName handleInvalidInput

  -- ディレクトリを作成
  createDirectory directoryName

  -- ファイルを作成
  let mainFileContent = "main :: IO ()\nmain = do\n  print \"template\"\n"
  writeFile (directoryName </> "Main.hs") mainFileContent

  figletConsole "Good!"

  -- cabalファイルに追記
  catch (appendCabalFile directoryName) handleCabalFileError

-- ディレクトリ名が不正な場合の例外ハンドラ
handleInvalidInput :: SomeException -> IO a
handleInvalidInput _ = do
  figletConsole "Panic!"
  exitFailure

-- cabalファイルにディレクトリ名を追記
appendCabalFile :: String -> IO ()
appendCabalFile directoryName = do
  let filePath = "kp-project.cabal"
  fileExists <- doesFileExist filePath

  if not fileExists
    then figletConsole "Oof!"
    else do
      currentDir <- getCurrentDirectory
      let configFileContent =
            "\n\nexecutable "
              ++ directoryName
              ++ "\n    import:           common-definitions\n    main-is:          "
              ++ directoryName </> "Main.hs\n    hs-source-dirs:   "
              ++ directoryName
              ++ "\n"

      -- ファイルに書き込み
      appendFile filePath configFileContent
      figletConsole "Success!"

-- cabalファイルの書き込みエラー時の例外ハンドラ
handleCabalFileError :: SomeException -> IO ()
handleCabalFileError _ = figletConsole "Oof!"
