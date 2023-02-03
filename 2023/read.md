# WebAssemblyで入出力をする

[前回](/2022/wasm_comp.html)と[前々回](/2022/wasm_inter.html)のあらすじ:
WebAssemblyでLISP 1.5の処理系を作った。

[Ichigo Lisp](https://github.com/zick/IchigoLisp)
を公開してから約1年が経過したが、
今になってようやく（まともな）READが実装された。
たかがREADを実装するのにどうしてこんなに時間がかかったのか、
説明（言い訳）をしようと思う。
ちなみにタイトルに反しWebAssemblyの話は一切出てこない。

## すっごい雑なJavaScriptの説明

JavaScriptは基本的にシングルスレッドで、イベントループに基づいて動く。

```c++
// From https://developer.mozilla.org/ja/docs/Web/JavaScript/EventLoop
while(queue.waitForMessage()){
  queue.processNextMessage();
}
```

JavaScriptの処理系は、イベントキューからイベントを取り出し、
それに対応するJavaScriptの関数を呼び出す。
イベントは「ページがロードされた」とか「ボタンがクリックされた」とか
「タイマーが起動した」とかいったものだ。
スレッドは1つしかないので、
あるイベントを処理している間は他のイベントの処理はできない。
処理が途中で中断されることはない。
そのため、呼び出したJavaScriptの関数がいつまでたっても
イベントループまでreturnしなければ、
JavaScriptの処理系やブラウザは固まってしまう。
ネットワーク越しにデータを取ってくるなど時間のかかる処理を実行したいときは、
クロージャを継続として渡して一度イベントループまで戻った後、
処理が完了したら継続を起動するのが一般的だ
（最近はasyncとawaitというものもあるらしいが）。

## WebAssemblyからの出力

LISP 1.5にはPRINTという文字列を出力する関数がある。
PRINTが呼ばれたらできるだけ早く画面（ブラウザ）に文字列を表示したい。
しかし、WebAssembly（とJavaScript）で
単純なコードを書いているとそれさえもできない。
というのも、WebAssemblyのコードはJavaScriptから呼び出されるため、
WebAssemblyのコードの実行中はイベントループに戻らない。
イベントループに戻らないとブラウザは固まってしまうため、
文字列を表示したつもりになっても実際に画面にそれが反映されることはない
（これが実装の都合なのか仕様なのかはしらないが）。
評価の途中でPRINTが何度も呼ばれても文字列は表示されず、
式全体の評価が終わりイベントループに戻ってから
一斉に文字列が表示されることになる。

JavaScript流にこの問題を解決するなら、
PRINTが呼ばれた時点でイベントループに戻るべきなのだが、
それをするためには「PRINTが呼ばれたあとの継続」を保存し、
文字列の表示後にその継続から処理を再開できなければならない。
はじめから一級継続をサポートしていればそれも可能かもしれないが、
後付でそんなものを実装するのは困難だろう。

Ichigo Lispではこの問題を解決するためにWeb Workerというものを使った。
Web WorkerとはJavaScriptをメインスレッドとは異なる
別のスレッド（ワーカースレッドと呼ぶ）で実行するための仕組みだ。
メインスレッドとワーカースレッドは独立したイベントキューを持ち並行に動作する。
ワーカースレッドで動くJavaScriptがいつまでたってもreturnしなくても、
メインスレッドで動くJavaScriptが適宜returnしてイベントループに戻る限り、
ブラウザが固まることはない。

ただし、ワーカースレッドには色々制限があり、直接DOMを操作することはできない。
ワーカースレッドはボタンを押されたことを知ることもできなければ、
ワーカースレッドから直接文字列を表示することもできない。
そういったことをするためにはメッセージの送受信を使う。

Ichigo Lispでは、ユーザが式を入力し、evalボタンを押すと、
メインスレッドがワーカースレッドに
「この文字列を評価しろ」というメッセージを送信する。
ワーカースレッドはメッセージを受信すると、
WebAssemblyで書かれたLISP処理系により式を評価する。
評価中にPRINTが呼び出されるとワーカスレッドはメインスレッドに
「この文字列を表示しろ」というメッセージを送信する。
メインスレッドはメッセージを受信するとその文字列を画面に表示する。
式の評価が終わり、値が求まると、ワーカスレッドはそれを文字列に直し、
メインスレッドに「式の値は次の文字列となった」というメッセージを送信する。
メインスレッドはその文字列を画面に表示し、最初の状態に戻る。

ワーカスレッドは最初に「この文字列を評価しろ」というメッセージを受け取ってから
実際に評価を行い結果をメインスレッドに送信するまで、
一度もイベントループに戻らないのに対して、
メインスレッドは少し仕事をするたびにイベントループに戻る。
例えばevalボタンが押されて「この文字列を評価しろ」
というメッセージを送信したら、そこで仕事を終えイベントループにもどる。
PRINTが評価され「この文字列を表示しろ」というメッセージを受信したら、
文字列を画面に表示した後イベントループに戻る。
このようにメインスレッドの仕事を最小に保つことで
「こまめにイベントループに戻らなければならない」
というJavaScriptの流儀を守ることができるのだ。

## WebAssemblyへの入力

それではREADはどうだろうか。
PRINTと同様にメッセージを使えばよさそうに思える。
しかし残念ながらその戦略はうまく行かない。
メッセージの送信は任意の箇所から行うことができる。
しかし、メッセージの受信はイベントループからしかできない。
メッセージを受信したければイベントループまでreturnしなければならない。

繰り返しになるが、
WebAssemblyのコードが動いている間はイベントループに戻ることはない。
イベントループに戻りたければ処理を中断して再開する
一級継続のようなものが必要だが、そんなものを後付で実装するのは困難だ。
つまり、単なるメッセージ送受信とは別の解決策が必要になる。

そこで、SharedArrayBufferとAtomics.wait/notifyの出番だ。
SharedArrayBufferというのはスレッド間で共有可能なバッファで、
Atomics.waitはバッファの特定箇所の値が条件を満たしている
限りスレッドを停止させ、
Atomics.notifyは停止しているスレッドを起こす。
これはもう実例を見たほうが早いだろう。

```javascript
// Worker thread
function /* When READ is called */ {
  ...
  const lock_buf = new SharedArrayBuffer(4);
  const lock = new Int32Array(lock_buf);
  const str_buf = new SharedArrayBuffer(10240);
  const str_arr = new Uint8Array(str_buf);
  postMessage(['wasm', 'read', lock, str_arr]);
  Atomics.wait(lock, 0, 0);  // sleep while lock[0] == 0
  ...
}
...

// Main thread
var read_lock = null;
var read_str_arr = null;
function /* When receiving 'read' message from 'wasm' */ {
  ...
  if (e.data[1] == 'read') {
    read_lock = e.data[2];
    read_str_arr = e.data[3];
  }
  ...
}
function /* When "read" button is clicked */ {
  ...
  var str = document.getElementById('input').value;
  storeStringToArray(str, read_str_arr);
  Atomics.store(read_lock, 0, 1);  // read_lock[0] = 1
  Atomics.notify(read_lock, 0);
  ...
}
```

ワーカスレッドはメインスレッドに「このバッファに文字列を書き込め」
というメッセージを送りつけ眠りに入る。
メインスレッドはバッファを受け取ると後で使うために保存しておく。
readボタンが押されるとメインスレッドはバッファに文字列を書き込み、
寝ているワーカスレッドを起こす。
これで無事ワーカスレッドにユーザからの入力が届くという訳だ。

## 何が問題だったのか

ここまでの説明を読むと必要な道具がすべて揃っているように思える。
しかし、実際には重大な問題がある。
SharedArrayBufferがセキュリティ上の問題で基本的には使用できないのだ。

HTTPヘッダに[秘密の呪文](https://developer.mozilla.org/ja/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer#%E3%82%BB%E3%82%AD%E3%83%A5%E3%83%AA%E3%83%86%E3%82%A3%E3%81%AE%E8%A6%81%E4%BB%B6)
を書き込めばSharedArrayBufferを使えるようになるのだが、
Ichigo LispをサーブしているGitHub Pagesでは
HTTPヘッダを書き換えることはできいないらしい。

そんなわけでずっと諦めていたのだが、
なんとService Workerというものを使えば
JavaScriptによってHTTPヘッダを書き換えるというとんでもないことができる
[ということを書いているページ](https://stefnotch.github.io/web/COOP%20and%20COEP%20Service%20Worker/)
を見つけてしまった。
すごく雑に説明すると、
Service Workerがページの制御を奪い
「新たなHTTPリクエストを投げるときはヘッダを書き換えたものを返す」
という処理を行うようにして、さらにページをリロードすると、
ヘッダが書き換わった状態でJavaScriptが読み込まれるため
SharedArrayBufferが使えるようになるという寸法だ。

わけがわからない（小並感）

*2023-02-03*
