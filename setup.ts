import figlet from "https://x.nest.land/deno-figlet@0.0.5/mod.js";

// ログ用
const figletConsole = async (text: string) => {
  const message = await figlet(text);
  console.log(message);
};

// ディレクトリ名を読み取り
let directoryName = "";
try {
  const buf = new Uint8Array(1024); // バッファを作成

  // プロンプトを表示してユーザーからの入力を待つ
  console.log("Please enter directory name:");
  const input = await Deno.stdin.read(buf);

  // nullの場合は強制終了
  if (input === null) {
    await figletConsole("Panic!");
    Deno.exit(1);
  }

  const _directoryName = new TextDecoder().decode(buf.subarray(0, input));

  // 不要な改行を消す
  directoryName = _directoryName.replace("\n", "");

  // ディレクトリ名が空文字でも強制終了
  if (directoryName === "") {
    await figletConsole("Panic!");
    Deno.exit(1);
  }
} catch (_) {
  // ディレクトリ名がない時は強制終了
  await figletConsole("Panic!");
  Deno.exit(1);
}

// ファイルを作成
try {
  // ディレクトリ作成
  await Deno.mkdir(directoryName);
  // ファイルデータ作成
  const mainFileEncoder = new TextEncoder();
  const mainFileData = mainFileEncoder.encode(
    `main :: IO ()\nmain = do\n  print "template"\n`
  );
  // ファイルに書き込み
  await Deno.writeFile(`${directoryName}/Main.hs`, mainFileData);
  await figletConsole("Good!");
} catch (_) {
  // 同じディレクトリ名で実行されたら失敗
  await figletConsole("Ouch!");
}

// cabalファイルに追記
try {
  const filePath = "kp-project.cabal";
  const textContent = await Deno.readTextFile(filePath);
  // 同じディレクトリ名で作成済みだったらエラーにする
  if (textContent.includes(directoryName)) {
    throw Error();
  }

  // 追記する内容
  const configFileEncoder = new TextEncoder();
  const configFileData = configFileEncoder.encode(
    `\n\nexecutable ${directoryName}\n    import:           common-definitions\n    main-is:          Main.hs\n    hs-source-dirs:   ${directoryName}\n`
  );
  // ファイルに書き込み
  await Deno.writeFile(filePath, configFileData, { append: true });
  await figletConsole("Success!");
} catch (_) {
  await figletConsole("Oof!");
}
