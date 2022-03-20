# WebAssemblyでLISPインタプリタを書いた

WebAssemblyで[LISP 1.5の処理系](https://github.com/zick/IchigoLisp)を書いた。

## 超高速WebAssembly入門

まずは
[このページ](https://developer.mozilla.org/ja/docs/WebAssembly/Understanding_the_text_format)
を読もう。

### WebAssemblyのデータ型

WebAssemblyには基本的に整数型と浮動小数点数型しかない。具体的には
`i32`, `i64`, `f32`, `f64` の4種類。

```
;; 32 bit の 1 + 2 の結果をスタックに置く
(i32.add (i32.const 1) (i32.const 2))
```

文字列はC言語のように整数の並びとして表現する。

### WebAssemblyのメモリ

WebAssemblyにはデータを置く場所として、
スタック、ローカル変数、グローバル変数、メモリがある。
メモリはアドレスを介したアクセスができる。
逆に言えばスタック、ローカル変数、グローバル変数はアドレスを取得できない。
アドレス経由でアクセスしたいものはメモリに置く必要がある。

```
;; 初期値0のグローバル変数
(global $gp (mut i32) (i32.const 0))
;; 1 + 2 の結果をアドレス0に格納
(i32.store (global.get $gp)
           (i32.add (i32.const 1) (i32.const 2)))
;; 32 bit の 3 + 4 の結果をスタックに置く
(i32.add (i32.load (global.get $gp))
         (i32.const 4))
```

### WebAssemblyの文字列

前述の通りWebAssemblyでは文字列はただの整数の並びだが、
メモリ上にあらかじめ文字列を格納することができる。

```
;; メモリの1000番地から "NIL" と 0x00 の4バイトを置く
(data (i32.const 1000) "NIL\00")
;; メモリの1004番地から "T" と 0x00 の2バイトを置く
(data (i32.const 1004) "T\00")
```

この例ではC言語にならって文字列の終端に 0x00 をおいているが、
文字列の開始アドレスと長さの組を使っても良い。
アドレスは人間が慎重に選ばなければならない。
文字列の開始アドレスを取得する方法はないので、
必要があれば別にグローバル変数（定数）を定義する必要がある。

```
;; メモリの1000番地から "NIL" と 0x00 の4バイトを置く
(data (i32.const 1000) "NIL\00")
;; 初期値1000のグローバル定数
(global $str_nil i32 (i32.const 1000))
```

### WebAssemblyの関数

WebAssemblyの処理はすべて関数の中に書く。
関数は任意個の引数を取り、高々1個の値を返す。

```
;; 引数を2つとり、その和を返す関数
(func $plus (param $p1 i32) (param $p2 i32) (result i32)
  (i32.add (local.get $p1) (local.get $p2)))
```

関数はローカル変数を使うこともできる。

```
(func $sqplus (param $p1 i32) (param $p2 i32) (result i32)
  ;; ローカル変数 $tmp
  (local $tmp i32)
  ;; $tmp に $p1 と $p2 の和を代入
  (local.set $tmp (call $plus (local.get $p1) (local.get $p2)))
  ;; $tmp * $tmp を返す
  (i32.mul (local.get $tmp) (local.get $tmp)))
```

### WebAssemblyの関数ポインタ

WebAssemblyでは関数のアドレスを取得する方法はない。
代わりに関数をテーブルに登録し、テーブルのインデックスを経由して呼び出せる。

```
;; 1 + 2 を返す関数
(func $f1 (result i32)
  (i32.add (i32.const 1) (i32.const 2)))

;; $f1 をテーブルのインデックス 0 に登録
(elem (i32.const 0) $f1)

;; void -> i32 の関数型
(type $v2i (func (result i32)))

(func $f2 (result i32)
  ;; テーブル経由で $f1 を呼び出す
  (call_indirect (type $v2i) (i32.const 0)))
```

### WebAssemblyの入出力

WebAssembly自体には入出力のための機能はない。
そういったことはJavaScriptの関数を呼び出すことで行う。
JavaScriptを呼び出せば何でもできるのだが、
値の受け渡しには整数と浮動小数点数しか使えないことに注意しなければいけない。
特に文字列をやり取りする際にはメモリとアドレスを使う必要がある。

```javascript
function getString(address) {
    var i8 = new Uint8Array(memory.buffer);
    var start = address, end = address;
    while (i8[end] != 0) { end++; }
    var bytes = new Uint8Array(memory.buffer, start, end - start);
    return new TextDecoder('utf8').decode(bytes);
}

function storeString(str) {
    var i8 = new Uint8Array(memory.buffer);
    var str_array = new TextEncoder('utf8').encode(str);
    for (var i = 0; i < str_array.length; i++) {
        i8[input_address + i] = str_array[i];
    }
    i8[input_address + str_array.length] = 0;
}
```

## 超高速LISP 1.5入門

まずは
[LISP 1.5 Programmer's Manual](http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf/view)
を読もう。

### LISP 1.5のデータ型

基本的にLISP 1.5にはコンスセルしかない。
なんとシンボルさえコンスセルを使って表す。
LISP 1.5 Programmer's Manualに従うと、シンボルは次のようなリストになる。

```lisp
;; シンボル PNAME のリスト表現
(-1 PNAME "PNAME")

;; シンボル APVAL のリスト表現
(-1 PNAME "APVAL")

;; シンボル NIL のリスト表現
#1=(-1 PNAME "NIL" APVAL (#1#))
```

ここで、 -1 はシステム上の-1であり、LISPにおける-1とは異なる。
ダブルクォートで囲まれた文字列は文字列のアドレスを表す。
つまり、シンボルとは、先頭に特殊な値（-1）が入った属性リストにすぎない。

### LISP 1.5の関数

LISP 1.5における（コンパイルされていない）関数は先頭にLAMBDAが入ったリストだ。

```lisp
((LAMBDA (X) (CONS X X)) 1)  ; => (1 . 1)
```

グローバル関数を定義するにはDEFINEを使う

```lisp
(DEFINE '((F1 (LAMBDA (X) (CONS X X)))))  ; => (F1)
(F1 2)  ; => (2 . 2)
```

DEFINEはシンボルの属性リストにEXPRを追加する。
DEFINEが評価された後のF1は次のようになる。

```lisp
(-1 PNAME "F1" EXPR (LAMBDA (X) (CONS X X)))
```

関数を呼び出す際には属性リストからEXPRが取り出され評価される。
この通り関数もコンスセルで表現される。

## WebAssemblyでLISPインタプリタを作る

WebAssemblyを使ったLISPインタプリタの作り方を書こうかと思ったが、
ここまでに書いた内容を使ってLISPインタプリタを書けることは自明なので、
作り方は特に説明せず、設計上の悩みどころを書くことにする。

### ローカル変数 vs メモリ

WebAssemblyを手書きする場合、ローカル変数（引数を含む）を使うのが楽だ。
楽なのだが、これには大きな問題がある。ごみ集めだ。
前述の通り、ローカル変数のアドレスを取得する方法はないし、
もちろんすべてのローカル変数をたどるようなことはできない。

まあ、なんとかなろうだろうとローカル変数を多用するプログラムを書いてしまったが
正直なところあとで少し後悔した。
ローカル変数の存在しないNScripterやScratchでごみ集めを書くのと比べ、
考えることが多くなる割に、C言語のような書きやすさ
（C言語を「書きやすい」と表現するほどWebAssemblyは書きづらい）
もないため、ただただ面倒だった。
もし、今度WebAssemblyでLISPインタプリタを書くことがあれば、
次はWebAssemblyのローカル変数を使わない方針をとってみてもいいかもしれない。
まあ、次なんてないだろうが。

### コンスセル以外のデータ

先程「基本的にLISP 1.5にはコンスセルしかない」と書いたが、
これは『基本的』な話であり、実際にはコンスセル以外のものはある。例えば配列だ。
LISP 1.5 Programmer's Manualに従うと、コンスセル以外のデータは
フルワード領域というところに置かれる。
シンボルが参照する文字列の実体もフルワード領域に置かれることになる。

しかし、今回の実装では面倒だったのでフルワード領域を作らなかった。
シンボルの参照する文字列は30 bitのfixnumに3文字ずつ詰め込んだ。

```lisp
(-1 PNAME ("CAR") ...)
(-1 PNAME ("CDR") ...)
(-1 PNAME ("CON" "S\00\00") ...)
```

3文字までの文字列を表現するためにはコンスセル1つ、つまり8バイトを使う。
6文字までの文字列を表現するためにはコンスセル2つ、つまり16バイトを使う。
素晴らしきメモリ効率だ。

しかし、シンボルというのは一番最初に実装する必要がある。
最初はWebAssemblyの知識も経験もなかったので、
極力楽をしたいという気持ちもありこんなことになってしまった。
フルワード領域を後から足せる設計にはしていたが、
いざ動くものができてから頑張る気にはなれなかった。

### 非同期処理

WebAssemblyの関数はまずJavaScriptから呼ばれる。この時点で気づくべきだった。
WebAssemblyが処理をしている間、JavaScriptのスレッドは止まってしまうことに。
そのままではWebAssemblyが処理をしている間は何かを出力することもできなければ、
入力を受け付けることもできない。

出力に関しては新しいスレッドを作ることで対処できる。

```javascript
// 新しいスレッドで ichigo.js を実行
var ichigo = new Worker('ichigo.js');
```

新しく作られたスレッドはDOMを直接触ることはできないので、
メインスレッドにメッセージを渡し、メインスレッドで描画を行えば良い。

```javascript
postMessage(['print', '表示してほしいテキスト']);
```

これでPRINTなどは正しく動作するようになる。
問題は入力だ。どうやら、JavaScriptで入力待ちを実現する方法は
少なくても広く使われているブラウザでは存在しないらしい。
スレッドを複数使ってもスレッド間の通信はメッセージしかないため、
メッセージを受け取るために一度returnしないといけない。
そのためREADのような関数を素直に実装することはできない。

一つの解決策として、継続を使うことが考えられる。
入出力関数を呼ぶときには継続をJavaScriptに渡して、
JavaScriptが処理を終えたら、JavaScriptから継続を起動すればいい。

ただ、最初から継続を実装するつもりならそれでいいが、
あとから入出力のためだけに継続を実装する気にはなれない。
もし、今度WebAssemblyでLISPインタプリタを書くことがあれば、
はじめから継続を実装すべきだろう。まあ、次なんてないだろうが。

*2022-03-20*
