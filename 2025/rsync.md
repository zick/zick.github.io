# 非rootユーザでFreeBSDにrsyncを入れた

## 背景

root権限のないFreeBSDサーバのファイルを手元にコピーしたい。
scpは使えるけどエラーが出たら止まったり、
かゆいところに手が届かないのでrsyncを使いたい。
手元のマシンにrsyncは入ってるけど、
rsyncはローカルとリモートの両方にインストールする必要がある。

そんな感じの状況になったので
タイトルの通り、非rootユーザでFreeBSDにrsyncを入れた。

## 作業ログ

```
local% ssh $REMOTE -p $PORT  # リモートマシンにログイン。ポートは22ではない
remote% mkdir tmp && cd tmp  # 作業用ディレクトリに移動
# rsyncのソースをダウンロード。wgetなんてものはない。
remote% curl -O https://download.samba.org/pub/rsync/src/rsync-3.4.1.tar.gz
remote% tar xvzf rsync-3.4.1.tar.gz  # 圧縮ファイルを展開
remote% cd rsync-3.4.1  # 展開されたディレクトリに移動
# インストール先をホームにしてとりあえずconfigure
remote% ./configure --prefix=/usr/home/zick
(出力前略)
Configure found the following issues:

- Failed to find xxhash.h for xxhash checksum support.
- Failed to find zstd.h for zstd compression support.
- Failed to find lz4.h for lz4 compression support.
(出力中略)
To disable one or more features, the relevant configure options are:
    --disable-xxhash
    --disable-zstd
    --disable-lz4

configure.sh: error: Aborting configure run

# 足らんものがあるから失敗した。それでもやりたかったらフラグをつけろ
# と言われたのでとりあえずつけてみる
remote% ./configure --prefix=/usr/home/zick/ --disable-xxhash --disable-zstd --disable-lz4
(出力前略)
    rsync 3.4.1 configuration successful

remote% make  # うまくいったっぽいので、とりあえずmake
remote% make install うまくいったっぽいので、とりあえずmake install
remote% exit  # 無事インストールできたのでローカルに戻る
# 早速rsync。-aは属性を保持したまま再帰的にコピー、
# -vはファイル一覧を表示、-zは圧縮、-uは変更のあるものだけコピー。
local% rsync -avz -u -e "ssh -p $PORT" $REMOTE:/usr/home/zick .
```

こんな感じで無事動いた。
嘘。本当はprefixに `/bin` を付けてしまい、
`~/bin/bin` にインストールされてしまい悲しい思いをした。
この悲しみを繰り返さないためにここにメモを残す。

*2025-06-11*
