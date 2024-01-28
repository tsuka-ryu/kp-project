import Control.Exception (SomeException, catch)
import HaskellSay (haskellSay)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)

-- haskellSayを使用するための関数
haskellSayConsole :: String -> IO ()
haskellSayConsole text = do
  let message = text
  haskellSay message

-- ユーザーからディレクトリ名を入力して取得
getDirectoryName :: IO String
getDirectoryName = do
  putStr "Please enter directory name: "
  hFlush stdout
  input <- getLine
  if null input
    then do
      haskellSayConsole "Panic!"
      exitFailure
    else return input

-- メイン関数
main :: IO ()
main = do
  -- ディレクトリ名を取得
  directoryName <- catch getDirectoryName handleInvalidInput

  -- ディレクトリを作成（親ディレクトリも含む）
  createDirectoryIfMissing True directoryName

  -- ファイルを作成
  let mainFileContent = "main :: IO ()\nmain = do\n  print \"template\"\n"
  writeFile (directoryName </> "Main.hs") mainFileContent

  -- cabalファイルに追記
  catch (appendCabalFile directoryName) handleCabalFileError

-- ディレクトリ名が不正な場合の例外ハンドラ
handleInvalidInput :: SomeException -> IO a
handleInvalidInput _ = do
  haskellSayConsole "Panic!"
  exitFailure

-- cabalファイルにディレクトリ名を追記
appendCabalFile :: String -> IO ()
appendCabalFile directoryName = do
  let filePath = "kp-project.cabal"
  fileExists <- doesFileExist filePath
  let executableName = map replaceSlash directoryName

  if not fileExists
    then haskellSayConsole "Oof!"
    else do
      let configFileContent =
            "\n\nexecutable "
              ++ executableName
              ++ "\n    import:           common-definitions\n    main-is:          "
              ++ "Main.hs\n    hs-source-dirs:   "
              ++ directoryName

      -- ファイルに書き込み
      appendFile filePath configFileContent
      haskellSayConsole "Success!"

-- cabalファイルの書き込みエラー時の例外ハンドラ
handleCabalFileError :: SomeException -> IO ()
handleCabalFileError _ = haskellSayConsole "Oof!"

-- スラッシュをハイフンに置き換え
replaceSlash :: Char -> Char
replaceSlash '/' = '-'
replaceSlash x = x