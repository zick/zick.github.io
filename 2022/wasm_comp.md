# WebAssemblyでLISPコンパイラを書いた

WebAssemblyで[LISP 1.5の処理系](https://github.com/zick/IchigoLisp)を書いた。
インタプリタの話は[こっち](/2022/wasm_inter.html)。
なお、このタイトルは9割方詐欺であり、
実際にはコンパイラのほとんどのコードは
（WebAssemblyで書かれたLISP処理系で動く）LISP自身で書かれている。

## 続・超高速WebAssembly入門

詳しくは
[このページ](https://developer.mozilla.org/ja/docs/WebAssembly/Understanding_the_text_format)
を読もう。

### WebAssemblyのモジュール。
WebAssemblyのコードはモジュールという単位にまとめられる。
モジュールはテーブル、メモリ、グローバル変数、そして関数などを含む。
テーブルとメモリは複数のモジュールで共有できる。

```javascript
// JavaScriptで共有のメモリとテーブルを作る
var memory = new WebAssembly.Memory({initial: 8});  // 8 * 64KB
var table = new WebAssembly.Table({initial: 1024, element: 'anyfunc'})
var importObject = {
    js: {
        memory: memory,
        table : table,
    },
    ...
};
// tableとmemoryを使ってモジュールをinstantiateする。
// 同様に他のモジュールもinstantiateできる。
WebAssembly.instantiateStreaming(fetch('ichigo.wasm'), importObject)
    .then(obj => {
        ...
    });
```

モジュールを超えて関数を呼び出すことはできない。
別のモジュールの関数を呼ぶためにはテーブルを使う。

```
;;; モジュールA

;; 1 + 2 を返す関数
(func $f1 (result i32)
  (i32.add (i32.const 1) (i32.const 2)))

;; $f1 をテーブルのインデックス 0 に登録
(elem (i32.const 0) $f1)
```

```
;;; モジュールB

;; void -> i32 の関数型
(type $v2i (func (result i32)))

(func $f2 (result i32)
  ;; テーブル経由で $f1 を呼び出す
  (call_indirect (type $v2i) (i32.const 0)))
```

この例ではモジュールBが一方的にモジュールAに依存しているが、
相互に依存関係があってもテーブルのインデックスに重なりがなければ問題ない。

## 続・超高速LISP 1.5入門

詳しくは
[LISP 1.5 Programmer's Manual](http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf/view)
を読もう。

### APVAL, SUBR, FSUBR, EXPR, FEXPR

LISP 1.5ではグローバル変数、グローバル関数の内容は
シンボルの属性リストに格納される。

グローバル変数は属性APVALに格納される。

```lisp
> (csetq gv 3)  ; グローバル変数GVを定義
(3)
> gv  ; GVを評価
3
> (printprop 'gv)  ; シンボルGVの属性リストを表示
(APVAL (3) PNAME "GV")
GV
```

コンスを1つかませている理由の考察は読者の練習問題とする。

機械語（この場合はWebAssembly）で書かれた関数は属性SUBRに格納される。

```
;; WebAssemblyでかかれたCAR
(func $subr_car (result i32)
      (local $arg1 i32)
      (local.set $arg1 (call $getArg1))
      (call $car (local.get $arg1)))

;; テーブルの100番に$subr_carを登録
(elem (i32.const 100) $subr_car)

;; 初期化時にシンボルCARの内容を以下のようにする（1は引数の数）
;; (-1 PNAME "CAR" SUBR (100 1))
```

EVALはシンボルがSUBRを持っていると、
`call_indirect`を使ってテーブル経由で関数を呼び出す。

機械語（この場合はWebAssembly）で書かれた
**引数を評価しない**関数は属性FSUBRに格納される。

```
;; WebAssemblyで書かれたQUOTE
(func $fsubr_quote (result i32)
      (local $args i32)
      (local.set $args (call $cdr (call $getEArg)))
      (call $car (local.get $args)))

;; テーブルの108番に$fsubr_quoteを登録
(elem (i32.const 108) $fsubr_quote)

;; 初期化時にシンボルQUOTEの内容を以下のようにする
;; (-1 PNAME "QUOTE" FSUBR (108))
```

EVALはシンボルがFSUBRを持っていると、引数を評価せず
`call_indirect`を使ってテーブル経由で関数を呼び出す。

LISPで書かれた関数は属性EXPRに格納される。

```lisp
> (define '((kaar (lambda (x) (car (car x))))))  ; KAARを定義
(KAAR)
> (kaar '((a b) (c d)))  ; KAARの呼び出し
A
> (printprop 'kaar)  ; シンボルKAARの属性リストを表示
(EXPR (LAMBDA (X) (CAR (CAR X))) PNAME "KAAR")
KAAR
```

見ての通りそのままLAMBDA式が格納される。
これをEVALで評価するのは簡単だろう。
ちなみにEXPRを定義するDEFINE自身がEXPRとして定義されている。
DEFINEを定義するためにDEFINEが欲しくなるところだが、
これをどうやって実現するかは読者の練習問題とする。

LISPで書かれた引数を評価しない関数はFEXPRに格納される。
EXPRとほとんど同じなので省略する。
ちなみにLISP 1.5にFEXPRを定義するための専用の構文（関数）は存在しない。
どうやって定義するかは

### COMPILE

LISPにおけるコンパイルは関数単位で行う。
コンパイルした関数からコンパイルしてない関数を呼んだり、
コンパイルしてない関数からコンパイルした関数を呼ぶこともできる。

```lisp
> (define '(
   (f (lambda (x) (add1 x)))
   (g (lambda (x) (f x)))
   (h (lambda (x) (g x)))))  ; 関数 F, G, H を定義
(F G H)
> (compile '(g))  ; Gだけコンパイル
NIL
> (h 1)
2
```

関数Aが関数Bを呼び出す場合を考える。
関数Aをコンパイルする時点では関数Bは定義されてなくてもよい。
関数Aを呼び出すまでに関数Bが定義されれば問題なく動作する。
また、関数Aを呼び出すまでに関数Bを定義し、さらにコンパイルした場合、
関数A、関数Bともにコンパイルされたコードが実行される。
関数Aを再度コンパイルし直す必要はない。
このように、関数の定義順、コンパイル順は自由である。
（ただし最適化をする際には順序が影響する可能性がある）

LISP 1.5はダイナミックスコープを採用しており、
これがコンパイラと非常に相性が悪い。
そこで、SPECIALとかCOMMONといった特殊な関数を使って、
あれこれ問題を回避できるのだが無理矢理感がある。
今どきのLISPには存在しないロストテクノロジーなので深入りしないことにする。

関数COMPILEはシンボルのリストを受け取り、
そのシンボルがEXPRをもつ場合はそれをコンパイルして、SUBRを設定する。
そのシンボルがFEXPRをもつ場合はそれをコンパイルして、FSUBRを設定する。
もともとのEXPRやFEXPRは削除される。

```lisp
> (define '((f1 (lambda (x) (cons x x)))))  ; 関数F1を定義
(F1)
> (printprop 'f1)  ; シンボルF1の属性リストを表示
(EXPR (LAMBDA (X) (CONS X X)) PNAME "F1")
F1
> (compile '(f1))  ; F1をコンパイル
NIL
> (printprop 'f1)  ; シンボルF1の属性リストを表示
(SUBR (315 1) PNAME "F1")
F1
```

## WebAssemblyでLISPコンパイラを作る

ここまでに書いた内容を使ってLISPコンパイラを書けることは
あまり自明ではないので説明を書く。
説明と苦労話が入り混じっている気がするが気にしないことにする。

### 基本方針

コンパイラの大部分はLISP自身で書く。
コンパイラは基本的に
「記号の列を受け取り別の記号の列を返す」
処理を繰り返すので、まさにLISP向けのプログラムだろう。
今回作ったコンパイラはLISPで書かれた関数を受け取り、
それをより単純なプリミティブを使ったLISPプログラムに変換し、
それをWebAssemblyのテキスト表現に近いアセンブリ言語に変換し、
最後にそれをWebAssemblyのバイナリ表現に変換する。
ここまでのLISPによる処理で作られるのは数のリストなので、
ここでWebAssemblyで書かれた関数の力を借り、
メモリの連続領域にバイナリを配置する。
そして、JavaScriptがメモリの内容から
WebAssemblyのモジュールをinstantiateすることでコンパイルが完了する。


```javascript
function getBinary(address, size) {
    var i8 = new Uint8Array(memory.buffer);
    return new Uint8Array(memory.buffer, address, size);
}
function loadWasm(pos, size) {
    var bytes = getBinary(pos, size);
    WebAssembly.instantiate(bytes, importObject)
        .then(obj => {
            modules.push(obj);
        });
}
```

1つのWebAssemblyモジュールは複数の関数を持つことができるが、
簡単のためLISPの関数1つにつき1つのモジュールを作るものとする。
また、LISPの関数がLAMBDAを使って入れ子関数を持つ場合は、
これを別の関数とみなし、別のモジュールを個別に作る。
関数ごとにWebAssemblyのテーブルに登録するためのインデックスが必要になるが、
これはコンパイラが使った番号を覚えておき、
関数をコンパイルするたびにインクリメントすればよい。
登録した番号は属性リストに入れることで、組み込みのSUBR同様に呼び出せる。

```lisp
> (compile '(f1))
NIL
> (printprop 'f1)
(SUBR (300 1) PNAME "F1")  ; テーブルの300番
> (compile '(f2))
NIL
> (printprop 'f2)
(SUBR (301 1) PNAME "F2")  ; テーブルの301番
```

### 定数を返す関数をコンパイルする

一番簡単な例として常に決まった数（例えば0）を返す関数を
コンパイルすることを考える。

```lisp
(lambda () 0)
```

もし、WebAssemblyのテキスト表現へのコンパイル結果を手で書くなら
次のようなものになるだろう。

```
(func zero_fun (result i32)
  (i32.const 2))
```

0ではなく2を返しているのは、fixnumに変換しているためだ。
LISPでコンパイラを書くときはシステムの数とfixnumを区別する必要があり、
絶妙に面倒だが、説明も面倒なので深入りしない。

さて、LISPの関数をもとにアセンブリを作り出すわけだが、
上記のような立派なテキスト表現を出力する必要はない。
関数の（WebAssemblyレベルの）型はすべて同じだし、
関数の名前は必要ない。
値の（WebAssemblyレベルの）型もすべてi32で固定なので、
次のようなもので十分だ。

```lisp
> (compile-code 0)
(CONST 0)
```

`(CONST 0)` が `(i32.const 2)` に対応する。
なぜ2が0になったかはfixnumの面倒なあれこれだ。

アセンブリが作れれば、これをバイナリに変換する。

```lisp
> (assemble-code '(CONST 0))
(65 2)
```

65 (0x41) というのは `i32.const` のバイナリ表現、2というのはfixnumの0だ。
つまりこのコード片はfixnumの0をスタックに置く。
WebAssemblyはスタックに置かれた値を戻り値とするので、
これだけから構成される関数は「fixnumの0を返す関数」となる。
つまり、欲しい関数がもう得られた。
後は[仕様書](https://www.w3.org/TR/wasm-core-1/)を読みながら
WebAssemblyのヘッダとかいろんなセクションとか面倒な情報の
バイナリ表現を作ってつなぎ合わせると完全な
WebAssemblyのモジュールのバイナリ表現が得られる。
つまりコンパイラはもう完成だ。

定数ではなくグローバル変数を返す関数をコンパイルするには、
シンボルからAPVALをGETして、
値の入っているセルからロードをするようなコードを生成すれば良い。
SUBRを呼び出すような関数は、各引数をコンパイルして、
（WebAssemblyではなく自前で管理する）スタックにそれをpushした後、
call_indirectを使って呼び出すようなコードを生成すれば良い。
EXPRの場合は同様に引数を処理しEVALを呼び出すコードを生成すれば良い。
話が面倒になるのはFSUBRとFEXPRだろう。

### FSUBRの解釈

LISP 1.5 Programmer's Manualによれば、
FSUBRは機械語で書かれた引数を評価しない関数だ。
これを字面通り解釈すると、
FSUBRの呼び出しが現れたときコンパイラはなにもできなくなる。
すべての処理をインタプリタに任せることになる。

これでは困るのでコンパイラは組み込みのFSUBRの意味を
「勝手に」解釈することにする。
SUBRの呼び出しは本当にSUBRを呼ぶコードを生成するが、
FSUBRの呼び出しは実際にはFSUBRを呼び出さず、
意味を解釈した上でコンパイラがコードを生成する。
例えばCONDはWebAssemblyの分岐命令を生成する。

FSUBRが使われるのはCONDのような制御構文だけではない。
LISP 1.5では可変長引数の関数はFSUBRとして定義されているため、
足し算を行うPLUS (今どきのLISPだと+) もFSUBRだ。
こういった引数をすべて評価する可変長引数関数は、
2引数バージョンのSUBRを用意して、それを使うように変換する。

```lisp
(transform '(plus 1 2 3))
> (PLUS2 1 (PLUS2 2 3))
```

あとは、普通のSUBR呼び出しとしてコンパイルすれば良い。

複雑なFSUBRとして、PROGというものがある。
これはラベルジャンプなどをサポートするためのもので、
柔軟なジャンプ命令のないWebAsssemblyとは非常に相性が悪いが、
[素晴らしき妥協](2022/wasm_goto2.html)をすれば
比較的簡単にコード生成ができる。あくまでも比較的だが。

さて、組み込みのFSUBRはその意味がマニュアルに載っているので、
コンパイラが勝手に意味を解釈することができた。
しかし、ユーザが定義したFEXPRはコンパイラには意味が分からないだろうし、
ましてやそれをコンパイルしたFSUBRに至っては
コンパイラが意味を解釈するのは不可能だろう。
つまり、ユーザが定義したFSUBR/FEXPRの呼び出しは
インタプリタを呼び出すようなコードを生成せざるを得ない。
それもただインタプリタを呼び出せばいいというものではない。
コンパイラは関数の引数を通常であればスタックに置き、
引数の名前などは忘れてしまうのだが、
FSUBR/FEXPRで使われる変数は名前を覚えておく必要がある。
また、FSUBR/FEXPRが変数の値を書き換える可能性があるため、
名前を覚えておくだけでは不十分で、変数をヒープに置く必要がある。
こんな感じで未知のFSUBR/FEXPRを呼び出すというのは大仕事になる。
コンパイルしても速くならないどころか、
下手をすると余分な処理をする分だけ遅くなる可能性すらある。

FSUBR/FEXPRというアイディアを初めて聞いたとき
「ああ、なんて便利なんだろう。
どうしてこんな便利なものが今どきのLISPにはないんだ。
コンパイラと相性が悪くても別にいいじゃないか。」
などと思っていたが、FSUBR/FEXPRをサポートするコンパイラを書いてみたら、
そんな気持は完全に消え去った。
FSUBR/FEXPRは悪い文明！！粉砕する！！

### スコープ

LISP 1.5はダイナミックスコープを採用している。

```lisp
> (define '(
   (f (lambda (x) (plus x y)))
   (g (lambda (y) (f 1)))))
(F G)
> (g 2)
3
```

ダイナミックスコープを実現するためには変数の名前を覚えておく必要がある。
しかし、コンパイラは前述の通り（普通は）変数の名前を忘れてしまう。
これに関してはユーザ側がSPECIALやCOMMONといった特殊な関数を使って
対処するというルールなので、コンパイラはSPECIAL/COMMONを実装する以外は
特に頑張らなくて良い。

問題はLAMBDAを使った入れ子関数だ。

```lisp
> (define '((f (lambda (x) ((lambda (y) (+ x y)) 1)))))
(F)
> (f 2)
3
```

前述の通り、内側のLAMBDAは別の関数として個別にコンパイルする。
ここで内側のLAMBDAには自由変数xが現れる。
これはもちろん外側のxなのだが、どうやってそれを伝えればいいのだろうか。
クールに
[3imp](https://legacy.cs.indiana.edu/~dyb/papers/3imp.pdf)
を読みこなすSchemerは
「これはですね、中でSETQを使ってないから
xの値をベクタに入れて覚えておけばいんですよ」
などとスマートな解決策を提示してくれるかもしれない。
たしかにそうかも知れない。
ただ「SETQが呼ばれていないか」の判定はどれほど正確にできるだろうか。
未知のFSUBR/FEXPRが現れるとそのなかでSETQが使われるか判定することはできない。
つまり、未知のFSUBR/FEXPRが現れると保守的に
SETQが使われる可能性があると判定することになる。
それから、クロージャが覚える自由変数の値だが、
配列/ベクタに入れることはできない。なぜなら配列を実装していないから。
そうなると、自由変数の値はリストに入れることになり、
変数を参照する際にはリストをたどることになる。
こうなれば（少なくても気持ちの上では）
連想リストを探索するのと大差ないのではないだろうか。
そんなお気持ちを考慮して、クロージャが捕まえる変数は（常に）
インタプリタ同様に連想リストとして記憶することにした。
クロージャに捕まる側の変数はスタックではなくヒープに置く。
より正確にはスタックに連想リストの要素となるコンスへのポインタを入れる。
こうすることでSETQが使われていても正しく動作する。
この手抜きの利点はこれだけではない。
クロージャが（変数の名前を必要とする）FSUBR/FEXPRを呼んだとしても、
正しく動作する。FSUBR/FEXPRのためには結局名前が必要なのだ。
それからクロージャを返す関数なんかも工夫せずにそのまま動く。
とりあえずヒープを使えば全ては解決する。

ちょっとスコープのことを書こうと思ったら
やたらと話がごちゃごちゃしてしまったのも全部FSUBR/FEXPRが悪い。

### ごみ集め

コンパイルされたコードがシンボルやリストを参照することがある。
ここで何も工夫しないと参照しているシンボルやリストが
ごみ集めによって回収されてしまう。
これを防ぐ簡単な方法は属性リストのなかに
参照しているオブジェクトを突っ込むことだろう。

```lisp
(-1 PNAME "F1" SUBR (300 1 . 参照しているオブジェクトのリスト))
```

クロージャなど名前のない関数での対策法は読者の練習問題とする。

### 非情な非同期処理

コンパイル結果をinstantiateするJavaScriptのコードを再掲する。

```javascript
function loadWasm(pos, size) {
    var bytes = getBinary(pos, size);
    WebAssembly.instantiate(bytes, importObject)
        .then(obj => {
            modules.push(obj);
        });
}
```

thenが現れていることから分かるようにこれは非同期処理だ。
つまり、loadWasmを呼んだ時点ではこのコンパイルした関数を呼べるとは限らない。
そこで、COMPILEはSUBRを追加するもののEXPRは消さず、
次にトップレベルからEVALが呼ばれたときにEXPRを消すようにした。
SUBRよりEXPRが優先されるため、
EXPRが消されるまではコンパイル前の関数が呼ばれる。
厳密さを求めるならthen以下が実行されたのを確認してから
EXPRを消すのが良いのだが、LISP側でそれを判断するのはあまりにも面倒だ。
やはり継続を実装すべきだった。

## まとめ

LISP 1.5の処理系を真面目に作ったら、
どうして今どきのLISPがFSUBR/FEXPRやダイナミックスコープを捨てたのか
大変よく分かった。みんなもLISP 1.5を実装してこの気持を体験しよう。

*2022-03-21*
