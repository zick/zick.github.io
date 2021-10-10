# Clojure メモ

大昔（たぶん10年くらい前）Clojureを試したときは、
jarファイルをJavaに読み込ませてClojureを起動したような気がするのだが、
今はどうやらClojure command line toolsとやらをインストールするのが
普通のやり方らしい。以下「Clojureをインストール」と書く。

そんなわけでMacBookにClojureをインストールしようとしたら
（あんまりClojureと関係ないところで）
予想外に面倒だったのでメモを残しておく。

## 1. 「ご注文はうさぎですか？ BLOOM」を第1羽から再生する
https://gochiusa.com/bloom/

これをしないとインストールがうまくいかない

## 2. brewをインストールする
https://brew.sh/

MacにClojureインストールするにはbrewが必須らしい。
MacPortsしか使っていない私はまずbrewをインストールすることになった。
まず謎のスクリプトを実行する。

```shell
% /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

それからパスを通すために.zshrcの末尾に謎の呪文を追加。

```
# .zshrc
eval "$(/opt/homebrew/bin/brew shellenv)"
```

.zshrcを再読み込み。これで終わり。

```shell
% source .zshrc
```

## 3. CommandLineToolsをアップデート
これでclojureをインストールしようとしたら
MacのCommandLineToolsが古すぎると怒られたのでアップデート。

```shell
% sudo rm -rf /Library/Developer/CommandLineTools
% sudo xcode-select --install
```

## 4. Clojureをインストール
https://clojure.org/guides/getting_started

brewコマンド1回で終わり。かんたん。

```shell
% brew install clojure/tools/clojure
```

cljコマンドを起動するとREPLで遊べる。

```shell
% clj
Clojure 1.10.3
user=> (first '(a b c))
a
```

## 5. Emacs 26+をインストール
ClojureをEmacsで使うにはCIDERとやらをインストールするのがいいらしいが、
CIDERをインストールするにはEmacs 26以上が必要らしい。
今入っているEmacsが古かったので（せっかくなので）
brewを使って新しいEmacsをインストールした。

```shell
% brew install emacs
```

Emacs 27がインストールされた。

## 6. EmacsからMELPAを使えるようにする
https://melpa.org/#/getting-started

CIDERをインストールするにはMELPAとやらを使えるようにする必要があるらしい。
.emacsに以下の設定を書く。

```
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

すでに `(package-initialize)` が書いてある場合は、
それより上に`require`と`add-to-list`を書く。

あとはEmacsで謎の呪文を実行してパッケージを更新する。

```
M-x package-refresh-contents
```

## 7. CIDERをインストール
https://cider.mx/

Emacsで謎の呪文を実行。

```
M-x package-install RET cider RET
```

これでCIDERがインストールされる。
あとはcljファイルを開いて、 `C-c C-x j j` でREPLが起動する。
式の末尾にカーソルを置いて `C-c C-c` で式を評価。
多分 `C-c C-k` でファイル全体を評価。

## おわり
ご注文はうさぎですか？ BLOOM 第2羽のエンディングあたりでここまでできた。
あとはごちうさを見続けるだけ。おわり。

*2021-10-10*
