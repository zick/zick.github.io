# Ichigo Lispコンパイラ完全解説

## はじめに

この記事では私がWebAssemblyで書いた
[Ichigo Lisp](https://github.com/zick/IchigoLisp)という
**LISP 1.5実装のコンパイラのソースコードをすべて解説**する。
というのも、最近とあるコンパイラの本を読んで
「説明が特定の実装に基づいてる割にそのソースが一部しか載ってない」
という不満を感じたのだが、[自分で書いたコンパイラの記事](/2022/wasm_comp.html)
を読み返してみると、コンパイラのソースコードが一切出てこないことに気づいた。
こんな記事を書きながらよく人様の本に不満を持てたものだと
瀬戸内海より深く反省したので、
ここにIchigo Lispのコンパイラのすべてのソースコードを解説する。

## 想定読者とか

Ichigo Lispのコンパイラは（ほとんど）すべてLISP 1.5で書かれている。
LISP 1.5特有のややこしい機能などは使っていないので、
Common Lisp、Scheme、Clojureあたりの入門書を読んだことのある人なら
だいたい読めると思う。
一部WebAssembly（のテキスト表現）で書かれた処理も出てくるが、
これは別に面白くないので、雰囲気が分かれば十分かと思う。

## コンパイラの入り口

LISP 1.5の関数`COMPILE`はコンパイルする関数の名前のリストを受け取る。
例えば `(COMPILE '(FOO BAR BAZ))` といった感じだ。
Ichigo Lispでは複数の関数にまたがる最適化といったカッコいいことはしないので、
単に一つずつ処理する。

```lisp
  (DE COMPILE (LST)
   (MAP LST (FUNCTION (LAMBDA (X) (C::COMPILE1 (CAR X))))))
```

見ての通り、`C::COMPILE1`という関数が単一の関数をコンパイルする。
`DE`というのはCommon Lispの`DEFUN`に相当する。
正確に言えばLISP 1.5には`DE`は存在しないのだが、
当時のLISP実装では`DE`はそれなりにメジャーだったっぽいので
Ichigo Lispでは`DE`を使えるようにしている。

```lisp
  (DE C::COMPILE1 (SYM) (PROG (FE FN IDX-OBJ)
   (SETQ FE (GET SYM 'FEXPR))
   (IF FE
    (SETQ FN FE)
    (SETQ FN (GET SYM 'EXPR)))
   (IF (NULL FN) (ERROR (SYMCAT SYM '$$| does not have EXPR or FEXPR|)))
   (SETQ IDX-OBJ (C::COMPILE-LAMBDA (IF FE (LIST 'FEXPR SYM) SYM) FN))
   (IF FE
    (PUTPROP SYM (LIST (CAR IDX-OBJ) (CDR IDX-OBJ)) 'FSUBR)
    (PUTPROP SYM (LIST (CAR IDX-OBJ) (LENGTH (CADR FN)) (CDR IDX-OBJ)) 'SUBR))
   ;; Remove EXPR later because WebAssembly modules are actually loaded
   ;; after all functions returned.
   (IF FE
    (CSETQ *COMPILED-FEXPRS* (CONS SYM *COMPILED-FEXPRS*))
    (CSETQ *COMPILED-EXPRS* (CONS SYM *COMPILED-EXPRS*)))
   ))
```

`PROG`に慣れていない人のために一応説明しておくと、
最初の`(FE FN IDX-OBJ)`はローカル変数のリストだ。
`C::COMPILE1`はまずシンボルの属性リストから関数の定義を取り出して、
変数`FN`に代入する。
`GET`はシンボルに結びついた値を取り出す関数だ。
`(DE FOO (ARG) BODY)`を評価すると、
シンボル`FOO`と`(LAMBDA (ARG) BODY)`が`EXPR`というキーで結びつき、
以後`(GET 'FOO 'EXPR)`を評価すると`(LAMBDA (ARG) BODY)`が得られる。
LISP 1.5はEXPRとFEXPRという2種類のユーザ定義関数があり、
EXPRは`DE`で定義される普通の関数、
FEXPRは引数が可変長で評価されないまるでマクロのような関数となっている。
`C::COMPILE1`はEXPRとFEXPRの両方をサポートするために
やたらとIFが出てくるが、基本的にはEXPRの方だけを読めば十分だ。
関数の定義を取り出すと、それを`C::COMPILE-LAMBDA`に渡す。
これこそが実際に関数をコンパイルする関数だ。
`C::COMPILE1`は成功すると整数を返す。
これはWebAssemblyのテーブルのインデックスなのだが、
まあコンパイルされた関数のアドレスとでも思っておけば良い。
その整数が得られたら、`PUTPROP`でシンボルと整数を結びつける。
以後コンパイルされた関数を呼び出すとそのアドレスにジャンプするといった感じだ。
そのあとは本当はシンボルからEXPRの定義を削除したいのだが、
色々あってその場で削除する代わりに「あとで消す」リストに登録する。
というのも、WebAssemblyの都合でコンパイル済みの関数が呼べるようになるのが、
一度トップレベルに戻ったあとなので、
それまではコンパイルした関数を呼べないためだ。
こういったしょーもない話は虚しい。

## 関数のコンパイルの全体像

関数`C::COMPILE-LAMBDA`は関数の名前と定義を受け取る。

```lisp
  (DE C::COMPILE-LAMBDA (SYM FN) (PROG (OBJS ASM)
   (C::VERIFY0 SYM FN)
   (SETQ FN (LIST (CAR FN) (CADR FN) (C::TRANSFORM (CAR (CDDR FN)))))
   (IF *PRINT-LAP* (PRINT FN))
   (SETQ ASM (C::COMPILE-FUNC SYM (CADR FN) (CAR (CDDR FN))))
   (IF *PRINT-LAP* (PRINT ASM))
   (BSTART)
   (C::ASSEMBLE ASM)
   (SETQ OBJS (C::GET-CONSTS ASM))
   (RETURN (CONS (LOAD-WASM) OBJS))))
```

まず、`C::VERIFY0`は関数の定義が妥当かどうか簡単に調べる。
ちなみに、もっと詳細な検査をする`C::VERIFY1`は私の頭の中にしか存在しない。
定義が妥当であれば、`C::TRANSFORM`を呼び出し、
関数をより単純な定義に書き換える。
例えばCONDをIFに書き換えたり、
可変長引数の`+`を2引数の`PLUS2`の呼び出しに書き換えたりする。
`C::TRANSFORM`はLISPの式を別のLISPの式に書き換える関数だ。
そのあとは`C::COMPILE-FUNC`を呼び出し、
関数を独自形式のアセンブリ言語にコンパイルする。
詳細は後で説明するが、アセンブリはもちろんリストで表現される。
狭義のコンパイルはここまでといってもいいかもしれない。
この次はまず、バイナリを書き出すバッファをクリアするために
`BSTART`を呼び出す。これはWebAssemblyで書かれた関数で、
単にポインタをバッファの先頭に移動するだけだ。

```
   (func $subr_bstart (result i32)
         (global.set $bwritep (global.get $bwrite_start))
         (i32.const 0))
```

バッファの準備ができたら`C::ASSEMBLE`がアセンブルを行い、
バッファにバイナリを書き込む。
これでリストで表現されていたLISP関数が
バイナリで表現されるWebAssemblyの関数になるわけだが、
バイナリから参照されるLISPオブジェクトが
ごみ集めで回収されないように一工夫いる。
`C::GET-CONSTS`はアセンブリに含まれる定数のリストを返す。

```lisp
  (DE C::GET-CONSTS (ASM L)
   (REMOVE-DUPLICATES ((LABEL REC (LAMBDA (AS)
     (COND
      ((ATOM AS) NIL)
      ((AND (EQ (CAR AS) 'CONST) (CADR AS) (NOT (NUMBERP (CADR AS))))
       (LIST (CADR AS)))
      (T (MAPCON (CDR AS) (FUNCTION (LAMBDA (Y) (REC (CAR Y)))))))))
    ASM)))
```

定数はアセンブリ言語では`(CONST C)`という形式で表され、
この関数は数以外の定数を取り出し、重複を取り除いてから返す。
定数のリストが得られたら`LOAD-WASM`を呼んでバッファの内容をもとに
実際に関数を作り出し、そのインデックス（関数のアドレス）を得る。

```js
        loadWasm: function(pos, size) {
            var bytes = getBinary(pos, size);
            WebAssembly.instantiate(bytes, importObject)
                .then(obj => {
                    modules.push(obj);
                });
        }
```

その辺を詳しく知りたい人はMDNでも読んでもらいたい。
なにはともあれ、`C::COMPILE-LAMBDA`は関数のインデックスと定数のリストを返す。
これを`C::COMPILE1`がシンボルの属性リストに入れるので、
WebAssemblyはコンパイルされた関数を呼び出せるし、
また定数はごみ集めされることもないという訳だ。

### ここまでのまとめ

関数は`C::COMPILE-LAMBDA`によってコンパイルされる。
`C::COMPILE-LAMBDA`は

1. `C::VERIFY0`で関数の内容が妥当か検査し
1. `C::TRANSFORM`で式を単純なものに書き換え
1. `C::COMPILE-FUNC`で関数をアセンブリに変換し
1. `C::ASSEMBLE`がアセンブルを行いバイナリをつくる

といった処理を行う。
以後この詳細を説明する。

## C::VERIFY0

関数が妥当か検査する`C::VERIFY0`は以下のように定義される。

```lisp
  (DE C::VERIFY0 (SYM FN) (PROG ()
   (IF (ATOM FN) (ERROR (SYMCAT SYM '$$| is not a function|)))
   (IF (NOT (EQ (CAR FN) 'LAMBDA))
    (ERROR (SYMCAT SYM '$$| is not a lambda|)))
   ))
```

先頭がLAMBDAかどうか確かめるだけで何一つ面白いことをやっていない。
特に解説することがないが、せっかくなのでLISP 1.5のシンボルについて説明する。
`$$|FOO BAR BAZ|`
というのは空白を含む任意の文字から成るシンボルを作るための記法だ。
Common Lispの `| FOO BAR BAZ|` とだいたい同じだ。
`SYMCAT`は次のように定義されている。

```lisp
  (SYMCAT (LAMBDA (X Y) (PROG2 (MAP (NCONC (UNPACK X) (UNPACK Y))
   (FUNCTION (LAMBDA (X) (PACK (CAR X)))))
   (INTERN (MKNAM)))))
```

`DE`ではなく`DEFINE`を使って定義しているため
定義の仕方が違うがそれはあまり重要ではない。
全体としては`UNPACK`して`PACK`して`INTERN`している。
`UNPACK`はシンボルを1文字ずつ分解したシンボルのリストを得る関数。
`PACK`はシンボルをバッファに詰め込む関数。
`MKNAM`と`INTERN`はバッファからシンボルを作り出す。
これによって2つのシンボルを連結したシンボルを作り出せる。
LISP 1.5には文字列がないので、気の利いた表示をしたければ
こうやってシンボルをこねくり回す必要がある。

## C::TRANSFORM

`C::TRANSFORM`の定義を見る前に実例を見てみよう。
可変長引数の関数は（可能であれば）固定長引数の関数に書き換える。
例えば `(list x y '(z))` は
`(CONS X (CONS Y (CONS (QUOTE (Z)) NIL)))` に変換される。
`(+ x y 1)` は `(PLUS2 X (PLUS2 Y 1))` に変換される。
複雑な構文は簡単なものに変換される。
例えば `(and x y z)` は
`(IF (NOT X) NIL (IF (NOT Y) NIL Z))` に変換される。
こういった変換をするのは、そのほうが楽だからというのももちろんあるが、
それ以上にこれをやらないとコンパイラが無意味になりかねないからだ。
LISP 1.5の仕様上、可変長引数の関数を呼び出すときは引数を評価しない。
`+`や`LIST`を呼び出すときは引数を評価しているように見えるが、
これは`+`や`LIST`が内部で引数を評価しているのだ。
このルールをそのままに `(DE F1 (X Y) (LIST (+ X Y)))`
という関数をコンパイルすると、
「関数`LIST`にリスト`(+ X Y)`を渡す関数」が出来上がる。
これは実質的にインタプリタみたいなものでなにも嬉しくない。
一方、事前に`C::TRANSFORM`を呼び出してやると、
`(DE F1 (X Y) (LIST (+ X Y)))` は
`(LAMBDA (X Y) (CONS (PLUS2 X Y) NIL))` に変換され、
これをコンパイルすると
「`X`と`Y`の値を`PLUS2`に渡し、その結果と`NIL`を`CONS`に渡す関数」
となる。`X`と`Y`の値はスタックに置かれるため、
全体としてスタック操作と関数呼び出しを行う関数となり、
そこに余分なリスト操作は一切出てこない。
もう少し頑張れば`PLUS2`や`CONS`の処理をその場でやることもできる。
まさに「コンパイルされた関数」となるわけだ。

それでは`C::TRANSFORM`の定義を見てみよう。

```lisp
  (DE C::TRANSFORM (EXP)
   (COND
    ((ATOM EXP) EXP)
    ((EQ (CAR EXP) 'QUOTE) EXP)
    ((EQ (CAR EXP) 'LAMBDA) EXP)
    ((EQ (CAR EXP) 'COND) (C::TRANSFORM-COND (CDR EXP)))
    ((EQ (CAR EXP) 'AND) (C::TRANSFORM-AND (CDR EXP)))
    ((EQ (CAR EXP) 'OR) (C::TRANSFORM-OR (CDR EXP)))
    ((EQ (CAR EXP) 'LIST) (C::TRANSFORM-LIST (CDR EXP)))
    ((EQ (CAR EXP) 'LOGAND) (C::TRANSFORM-LSUBR 'LOGAND2 (CDR EXP) -1))
    ((EQ (CAR EXP) 'LOGOR) (C::TRANSFORM-LSUBR 'LOGOR2 (CDR EXP) 0))
    ((EQ (CAR EXP) 'LOGXOR) (C::TRANSFORM-LSUBR 'LOGXOR2 (CDR EXP) 0))
    ((EQ (CAR EXP) 'MAX) (C::TRANSFORM-LSUBR 'MAX2 (CDR EXP) 0))
    ((EQ (CAR EXP) 'MIN) (C::TRANSFORM-LSUBR 'MIN2 (CDR EXP) 0))
    ((EQ (CAR EXP) 'PLUS) (C::TRANSFORM-LSUBR 'PLUS2 (CDR EXP) 0))
    ((EQ (CAR EXP) '+) (C::TRANSFORM-LSUBR 'PLUS2 (CDR EXP) 0))
    ((EQ (CAR EXP) 'TIMES) (C::TRANSFORM-LSUBR 'TIMES2 (CDR EXP) 1))
    ((EQ (CAR EXP) '*) (C::TRANSFORM-LSUBR 'TIMES2 (CDR EXP) 1))
    ((EQ (CAR EXP) 'CONC) (C::TRANSFORM-LSUBR 'NCONC (CDR EXP) NIL))
    ((EQ (CAR EXP) 'SETQ)
     (LIST 'SETQ (CADR EXP) (C::TRANSFORM (CAR (CDDR EXP)))))
    ((EQ (CAR EXP) 'IF)
     (CONS 'IF (MAPLIST (CDR EXP) (FUNCTION (LAMBDA (Y)
      (C::TRANSFORM (CAR Y)))))))
 ;; Especially for FUNCTION, GO, LABEL, PROG, TIME
    ((OR (GET (CAR EXP) 'FSUBR) (GET (CAR EXP) 'FEXPR)) EXP)
    (T (MAPLIST EXP (FUNCTION (LAMBDA (Y) (C::TRANSFORM (CAR Y))))))))
```

関数の数だけルールがあるのでやたらと長いが、

* `COND`, `AND`, `OR`, `LIST`は専用の関数で変換
* `+`のような引数を（内部で）評価する関数は共通の方法で変換
* `SETQ`と`IF`は簡単なのでその場で変換
* それ以外の引数を評価しないフォームには何もしない
* 引数を事前に評価する普通のフォームは全要素を変換

といった具合で、そこまで場合分けは多くない。
`SETQ`は第一引数は変換せず、第二引数だけを変換する。
`IF`は全引数を変換してしまう。
次は個別のルールをもつ関数を見てみよう。

```lisp
  (DE C::TRANSFORM-COND (X)  ;; X of (COND . X)
   (IF (NULL X)
    NIL
    (LIST 'IF (C::TRANSFORM (SCAR (CAR X)))
     (C::TRANSFORM (SCAR (SCDR (CAR X))))
     (C::TRANSFORM-COND (CDR X)))))
  (DE C::TRANSFORM-AND (X)  ;; X of (AND . X)
   (COND ((NULL X) T)
    ((NULL (CDR X)) (C::TRANSFORM (CAR X)))
    (T (LIST 'IF (LIST 'NOT (C::TRANSFORM (CAR X)))
     NIL
     (C::TRANSFORM-AND (CDR X))))))
  (DE C::TRANSFORM-OR (X)  ;; X of (OR . X)
   (COND ((NULL X) NIL)
    ((NULL (CDR X)) (C::TRANSFORM (CAR X)))
    (T (LIST 'IF
     (LIST 'CAR (LIST 'CSETQ '*OR-RESULT* (C::TRANSFORM (CAR X))))
     '*OR-RESULT*
     (C::TRANSFORM-OR (CDR X))))))
  (DE C::TRANSFORM-LIST (X)  ;; X of (LIST . X)
   (COND ((NULL X) NIL)
    (T (LIST 'CONS (C::TRANSFORM (CAR X)) (C::TRANSFORM-LIST (CDR X))))))
```

基本的にLISPの入門書に載っているリストの操作くらいのことしかやっていないので、
見どころはあまりないと思う。強いて言うなら`OR`が面倒なことくらいだろう。
ちなみに`SCAR`と`SCDR`は`NIL`に対して`NIL`を返す`CAR`と`CDR`だ。

```lisp
  (SCAR (LAMBDA (X) (IF (NULL X) X (CAR X))))
  (SCDR (LAMBDA (X) (IF (NULL X) X (CDR X))))
```

LISP 1.5はシンボルがリストで表現されている関係で
このような関数を用意する必要がある。

```lisp
  (DE C::TRANSFORM-LSUBR (FN X D)
   (COND ((NULL X) D)
    ((NULL (CDR X)) (C::TRANSFORM (CAR X)))
    (T (LIST FN (C::TRANSFORM (CAR X)) (C::TRANSFORM-LSUBR FN (CDR X) D)))))
```

引数を（内部で）評価する可変長引数の関数を変換する
`C::TRANSFORM-LSUBR`は`REDUCE`みたいなものだ。これも簡単だろう。

そんなわけであっという間に`C::TRANSFORM`が終わってしまった。
一種のプログラム変換がこんなに簡単に書けてしまうのは
LISPならではかもしれない。
次はいよいよコンパイラの中核部分の説明に入る。

## C::COMPILE-FUNC

### スタックフレーム

Ichigo Lispでは組み込み関数及びコンパイル済みの関数（まとめてSUBRという）を
呼ぶときにはスタックが次のようになっている。

```
|    | <-- $sp
|argR| --> (arg4, arg5, arg6, ...)
|arg3|
|arg2|
|arg1|
|env |
|....|
```

スタックポインタ（の一つ下）は第四引数を指し、
その下には第三引数、第二引数、第一引数、
そして環境を表す連想リストが続く。
より正確には、引数の数が4つより少ないときは適宜NILが入り、
引数の数が4つ以上のときは、`argR`の位置に第四引数以降のリストが入る。
引数が4つ以上の場合が少々奇妙ではあるが、
これにより常にスタックフレームのサイズが固定化されるという利点はある
（しかし、こうしないほうが良かったとあとで後悔した）。

引数を評価しない関数（FSUBR）の場合は少し形が違う。

```
|    | <-- $sp
|expr|
|env |
|....|
```
スタックポインタ（の一つ下）が式全体を表し、
その下に環境を表す連想リストが続く。

### スタックに置けない変数

スタックが出てきたところで、次は「引数（変数）の置き場所」について考える。
通常、引数はスタックに置くのだが入れ子関数が出てくるとこれはうまく行かない。
例えば次のような例を考えてほしい。

```lisp
> (define '((gen (lambda (n) (function (lambda (m) (setq n (plus n m))))))))
(GEN)
> (prog (x) (setq x (gen 100)) (print (x 10)) (print (x 90)) (print (x 300)))
110
200
500
NIL
```

関数`gen`はクロージャを返す。このクロージャは`gen`の引数`n`を使っている。
そのため、`n`は`gen`を呼び終えたあとも使えなければならない。
要するに`n`をスタックに置く訳にはいかない。
他にもLISP 1.5にはスペシャル変数やコモン変数といったものもあり、
これらもスタックに置くことができない。
ここから先はそういった「引数の置き場所」を意識しなければならない。

### コード

さて、いよいよコードの解説に移る。

```lisp
  (DE C::COMPILE-FUNC (SYM ARGS EXP) (PROG (CV COV SV)
   (SETQ CV (C::CAPTURED-VARS ARGS EXP))
   (SETQ COV (REMOVE-IF-NOT (FUNCTION (LAMBDA (X) (GET X 'COMMON))) ARGS))
   (SETQ SV (REMOVE-IF-NOT (FUNCTION (LAMBDA (X) (GET X 'SPECIAL))) ARGS))
   (RETURN
    (LIST 'PROGN
     (CONC (LIST 'BLOCK)
      ;; Initialization
      (C::INIT-FSUBR-STACK SYM)
      (C::INIT-CV-STACK ARGS CV)
      (C::INIT-COMMON-VARS ARGS COV)
      (C::INIT-SPECIAL-VARS ARGS SV)
      ;; Body
      (LIST (C::COMPILE-CODE SYM ARGS (C::REPLACE-CV-REF ARGS EXP CV))))
     ;; Cleanup
     (CONC (LIST 'PROGN)
      (C::CLEANUP-SPECIAL-VARS ARGS SV)
      (C::CLEANUP-FSUBR-STACK SYM)
      (C::RESTORE-SP SYM))))))
```

`C::COMPILE-FUNC`はまず入れ子関数に捕まった変数を`CV`にセットし、
コモン変数、スペシャル変数を`COV`、`SV`にセットする。
これが終わったらいよいよアセンブリのコードを作る。
これは下準備、本体、後始末の3つのコードから成る。
まずは入れ子関数に捕まった変数を探す関数を見てみよう。

```lisp
  (DE C::CAPTURED-VARS (ARGS EXP)
   (REMOVE-DUPLICATES ((LABEL REC (LAMBDA (ARGS E INL)
    (COND
     ((ATOM E) (IF (AND INL (MEMBER E ARGS) (NOT (GET E 'COMMON))
       (NOT (GET E 'SPECIAL))) (LIST E) NIL))
     ((EQ (CAR E) 'QUOTE) NIL)
     ((EQ (CAR E) 'LAMBDA)
      (REC
       (SET-DIFFERENCE ARGS (CADR E)) (CAR (CDDR E)) T))
     ((EQ (CAR E) 'PROG)
      (REC
       (SET-DIFFERENCE ARGS (CADR E)) (CDDR E) T))
     (T (MAPCON E (FUNCTION (LAMBDA (Y)
      (REC ARGS (CAR Y) INL)))))))) ARGS EXP NIL)))
```

これもリスト操作の例題のような関数なので難しくないと思う。
`QUOTE`の内部ではなく、`LAMBDA`もしくは`PROG`の内部に現れ、
かつコモンでもスペシャルでもない変数が入れ子関数に捕まった変数だ。
Ichigo Lispでは`PROG`は関数を生み出すことにしているが、
おそらくこれは一般的ではないと思うので、
人前で話して恥をかかないように注意してほしい。

#### 下準備

次は下準備の最初の処理`C::INIT-FSUBR-STACK`を見る。
これは引数を評価しない関数であるFEXPRをコンパイルするときにだけ使われる。

```lisp
  (DE C::INIT-FSUBR-STACK (FI)
   (IF (OR (ATOM FI) (NOT (EQ (CAR FI) 'FEXPR)))
    NIL
    ;; 29: createSubrStackFromFsubrStack
    (LIST (LIST 'CALL 'I2V (LIST 'GET-LOCAL 0) 29)
     (LIST 'SET-LOCAL 0 (LIST 'CALL 'V2I 0)))))  ;; 0: getSp
```

コンパイル対象が普通の関数（つまりEXPR）であればNILを返す。
言い換えればコードを生成しない。
一方、コンパイル対象がFEXPRの場合は、
WebAssemblyで書かれた関数`createSubrStackFromFsubrStack`
を呼ぶコードを生成する。
その場で関数を呼ぶのではなく、関数を呼ぶコードを生成するのだ。
具体的には次のようなコードが生成される。

```lisp
(CALL I2V (GET-LOCAL 0) 29)
(SET-LOCAL 0 (CALL V2I 0))
```

`I2V`や`V2I`は型を示しており、`I2V`は「intを受け取りvoidを返す型」、
`V2I`は「voidを受け取りintを返す型」を表している。
`GET-LOCAL`と`SET-LOCAL`はWebAssemblyのローカル変数の読み書きを行う。
これはLISPのローカル変数とは無関係なので注意してほしい。
`CALL`は関数呼び出しを行うのだが`29`や`0`といった最後の引数が
どの関数かを示す。この番号は関数のインデックス、
つまり関数のアドレスのようなのものだ。
この詳細を見るためには残念ながらWebAssemblyのコードを読まなければならない。

```
 (elem (i32.const 0) $getsp)  ;; v2i
...
 (elem (i32.const 29) $createSubrStackFromFsubrStack)  ;; i2v
...
 (func $getsp (result i32)
       (global.get $sp))
...
 (func $createSubrStackFromFsubrStack (param $fmp i32)
       (call $push (call $getAArgF (local.get $fmp)))  ;; arg a
       (call $push (call $cdr (call $getEArgF (local.get $fmp))))  ;; arg 1
       (call $push (call $getAArgF (local.get $fmp)))  ;; arg 2
       (call $push (i32.const 0))  ;; arg 3
       (call $push (i32.const 0)))  ;; arg 4
```

ローカル変数0番には関数が呼ばれた時点でのスタックポインタが入っている。
この関数が何をやっているのか詳細は割愛するが、雑に説明すると、
FSUBR用スタックフレームをSUBR用に作り直して、
第一引数の箇所に環境を表す連想リスト、
第二引数に引数のリストが入るようにしている。

なんだか必要以上に複雑な気がするが気を取り直して下準備第二弾、
`C::INIT-CV-STACK`を見る。
これは入れ子関数に捕まった引数を処理するために使われる。

```lisp
  (DE C::COMPILE-ARG (N L)
   (IF (< N 4)
    (LIST 'CALL 'I2I (LIST 'GET-LOCAL 0) (+ 11 N)) ;; 11: getArgF1
    (LIST 'CALL 'II2I (LIST 'GET-LOCAL 0) N 15))) ;; 15: getArgFN
  (DE C::INIT-CV-STACK1 (V N)
   (IF (< N 4)
    (LIST 'CALL 'II2V
     (LIST 'GET-LOCAL 0)
     (LIST 'CALL 'II2I (LIST 'CONST 'C::VCTAG)
      (LIST 'CALL 'II2I (LIST 'CONST V) (C::COMPILE-ARG N) 6) 6)  ;; 6: cons
     (+ 31 N))  ;; 31: setArgF1
    (LIST 'CALL 'III2V
     (LIST 'GET-LOCAL 0)
     N
     (LIST 'CALL 'II2I (LIST 'CONST 'C::VCTAG)
      (LIST 'CALL 'II2I (LIST 'CONST V) (C::COMPILE-ARG N) 6) 6)  ;; 6: cons
     35)))  ;; 35: setArgFN
  (DE C::INIT-CV-STACK (ARGS CV)
   (MAPLIST CV (FUNCTION (LAMBDA (Y)
    (C::INIT-CV-STACK1 (CAR Y) (POSITION (CAR Y) ARGS))))))
```

`C::INIT-CV-STACK`は入れ子関数に捕まった引数を1つずつ
`C::INIT-CV-STACK1`に渡す。その際に引数の位置、
つまり何番目の引数なのかという情報も渡す。
`C::INIT-CV-STACK1`は引数をスタックから取り出し、
`C::VCTAG`というシンボルとのコンスを作り、スタックに戻す。
例えば第一引数`N`が100であり、この引数が入れ子関数に捕まっているとする。

```
|    | <-- local 0
|NIL |
|NIL |
|NIL |
|100 |
|env |
|....|
```

この場合、スタックは次のようになる。

```
|    | <-- local 0
|NIL |
|NIL |
|NIL |
|    | --> (C::VCTAG N . 100)
|env |
|....|
```

コンスセルはもちろんヒープに置かれるため、スタックが縮んでも消えることはない。
これによりクロージャを正しく実現できる。
各所に現れる`(< N 4)`を見るとこのスタックフレームの設計により
余分なコードが各所に生まれて後悔したことがうかがえるだろう。

```
 (elem (i32.const 11) $getArgF1)  ;; i2i
 (elem (i32.const 31) $setArgF1)  ;; ii2v
 (func $getArgF1 (param $fmp i32) (result i32)
       (i32.load (i32.sub (local.get $fmp) (i32.const 16))))
 (func $setArgF1 (param $fmp i32) (param $val i32)
       (i32.store (i32.sub (local.get $fmp) (i32.const 16)) (local.get $val)))
```

スタックフレームを触るWebAssemblyの関数はこのようになっているが、
別に読んでも楽しいものではないだろう。

次は下準備第三段、`C::INIT-COMMON-VARS`だ。
コモン変数とはコンパイルされた関数と
インタプリタで動く関数で共通の変数を使うための仕組みだ。

```lisp
  (DE C::INIT-COMMON-VARS (ARGS COV)
   (IF (NULL COV)
    NIL
    (CONS
     (LIST 'CALL 'II2V
      (LIST 'GET-LOCAL 0)
      (LIST 'CALL 'II2I
       (LIST 'CALL 'II2I (LIST 'CONST (CAR COV))
        (C::COMPILE-ARG (POSITION (CAR COV) ARGS)) 6)  ;; 6: cons
       (LIST 'CALL 'I2I (LIST 'GET-LOCAL 0) 10)  ;; 10: getAArgFInSubr
       6)  ;; 6: cons
      30)  ;; 30: setAArgFInSubr
     (C::INIT-COMMON-VARS ARGS (CDR COV)))))
```

スタックから引数の値を取り出し、引数の名前とのペアを作り、
さらにそれを環境に追加する。
例えば第一引数の名前が`X`、値が100だとすると、

```
|    | <-- local 0
|NIL |
|NIL |
|NIL |
|100 |
|env | --> alist
|....|
```

このようなスタックが次のようになる。

```
|    | <-- local 0
|NIL |
|NIL |
|NIL |
|100 |
|env | --> ( (X . 100) . alist)
|....|
```

なんて面倒なんだ。次は下準備の最後。`C::INIT-SPECIAL-VARS`だ。

```lisp
  (DE C::COMPILE-SPECIAL-VAR (CELL)
   (LIST 'CALL 'I2I
    (LIST 'LOAD (LIST 'CONST CELL))
    4)) ;; 4: car
  (DE C::INIT-SPECIAL-VARS (ARGS SV)
   (IF (NULL SV)
    NIL
    (CONS
     (LIST 'PROGN
      (LIST 'CALL 'I2V (C::COMPILE-SPECIAL-VAR (PROP (CAR SV) 'SPECIAL)) 1)
      (LIST 'CALL 'I2V (C::COMPILE-ARG (POSITION (CAR SV) ARGS)) 1)
      (LIST 'STORE
       (LIST 'CALL 'I2I (LIST 'CONST (PROP (CAR SV) 'SPECIAL)) 4)  ;; 4: car
       (LIST 'CALL 'V2I 2))  ;; 2: pop (args)
      ;; TODO: make a utility function for set argument
      (LIST 'CALL 'III2V
       (LIST 'GET-LOCAL 0) (POSITION (CAR SV) ARGS)
       (LIST 'CALL 'V2I 2)  ;; 2: pop (special)
       35))  ;; 35: setArgFN
     (C::INIT-SPECIAL-VARS ARGS (CDR SV)))))
```

`C::INIT-SPECIAL-VARS`は
スタックの内容とシンボルのスペシャルセルの内容を入れ替える。
例えば変数`X`の現在の値が99、新たな値が100の場合、

```
|    | <-- local 0
|NIL |
|NIL |
|NIL |
|100 |
|env |
|....|

X: (special 99)
```

スタックとシンボルは次のようになる。

```
|    | <-- local 0
|NIL |
|NIL |
|NIL |
|99  |
|env |
|....|

X: (special 100)
```

交換に際してWebAssemblyのpushとpopを使っている。

```
 (elem (i32.const 1) $push)  ;; i2v
 (elem (i32.const 2) $pop)  ;; v2i
 (func $push (param $val i32)
       (i32.store (global.get $sp) (local.get $val))
       (global.set $sp (i32.add (global.get $sp) (i32.const 4))))
 (func $pop (result i32)
      (global.set $sp (i32.sub (global.get $sp) (i32.const 4)))
      (i32.load (global.get $sp)))
```

#### 後始末

さて、下準備が終わったので、次は本体に移りたいところだが、
先に後始末の説明をやってしまう。
後始末第一弾は`C::CLEANUP-SPECIAL-VARS`だ。

```lisp
  (DE C::CLEANUP-SPECIAL-VARS (ARGS SV)
   (IF (NULL SV)
    NIL
    (CONS
     (LIST 'PROGN
      (LIST 'CALL 'I2V (C::COMPILE-ARG (POSITION (CAR SV) ARGS)) 1)
      (LIST 'STORE
       (LIST 'CALL 'I2I (LIST 'CONST (PROP (CAR SV) 'SPECIAL)) 4)  ;; 4: car
       (LIST 'CALL 'V2I 2)))  ;; 2: pop (args)
     (C::CLEANUP-SPECIAL-VARS ARGS (CDR SV)))))
```

`C::CLEANUP-SPECIAL-VARS`はスタックの値でシンボルのスペシャルセルを置き換える。
要するに、スペシャルセルの値が関数呼び出しの前の状態に戻る。
まさに`C::INIT-SPECIAL-VARS`の後始末をするわけだ。

次は後始末第二弾、`C::CLEANUP-FSUBR-STACK`だ。

```lisp
  (DE C::CLEANUP-FSUBR-STACK (FI)
   (IF (OR (ATOM FI) (NOT (EQ (CAR FI) 'FEXPR)))
    NIL
    ;; 40: cleanupSubrStackFromFsubrStack
    (LIST (LIST 'CALL 'I2V (LIST 'GET-LOCAL 0) 40)
     ;; Don't need to eval return value
     (LIST 'CALL 'I2V 0 1))))
```

細かい説明は省くが、
`C::INIT-FSUBR-STACK`でいじったスタックフレームをもとに戻す。
最後にスタックに0をプッシュするがこれは
細かすぎる実装上の都合なので見なかったことにする。

後始末の最後は`C::RESTORE-SP`だ。

```lisp
  (DE C::RESTORE-SP (FI)
   (IF (AND (CONSP FI) (EQ (CAR FI) 'FEXPR))
    NIL
    (LIST (LIST 'CALL 'I2V (LIST 'GET-LOCAL 0) 41))))  ;; 41: setsp
```

これはスタックポインタを適切な位置に戻す。
ただそれだけで何も面白くない。

#### 本体

いよいよ本当にコードコンパイルするところに移りたいのだが、
残念ながらまだ下準備の続きがある。
入れ子関数に捕まった変数を参照する箇所を書き換える必要があるのだ。

```lisp
  (DE C::REPLACE-CV-REF (ARGS EXP CV)
   (COND
    ((NULL CV) EXP)
    ((ATOM EXP) (IF (MEMBER EXP CV) (LIST 'CDDR EXP) EXP))
    ((EQ (CAR EXP) 'SETQ)
     (IF (MEMBER (CADR EXP) CV)
      (LIST 'RPLACD (LIST 'CDR (CADR EXP))
       (C::REPLACE-CV-REF ARGS (CAR (CDDR EXP)) CV))
      (LIST 'SETQ (CADR EXP)
       (C::REPLACE-CV-REF ARGS (CAR (CDDR EXP)) CV))))
    ((EQ (CAR EXP) 'QUOTE) EXP)
    ((EQ (CAR EXP) 'LAMBDA) EXP)
    ((EQ (CAR EXP) 'PROG) EXP)
    ((EQ (CAR EXP) 'IF) (CONS 'IF (C::REPLACE-CV-REF ARGS (CDR EXP) CV)))
    ;; Especially for FUNCTION, GO, LABEL, TIME
    ((GET (CAR EXP) 'FSUBR) EXP)
    ((GET (CAR EXP) 'FEXPR) EXP)
    (T (CONS (C::REPLACE-CV-REF ARGS (CAR EXP) CV)
             (C::REPLACE-CV-REF ARGS (CDR EXP) CV)))))
```

`C::INIT-CV-STACK`により入れ子関数に捕まった変数は
`(C::VCTAG VAR . VAL)`のような形に変換された。
そのため、この変数を参照する箇所は`(CDDR VAR)`に変換し、
この変数を書き換える箇所は`(RPLACD (CDR VAR))`に変換する。
この変換はLISPレベルで行われるのである意味`TRANSFORM`の仲間とも言える。

さてさて、下準備が本当に完了したのでいよいよコードをコンパイルする。

```lisp
  (DE C::COMPILE-CODE (SYM ARGS X)
   (COND
    ((ATOM X) (C::COMPILE-ATOM X ARGS))
    (T (C::COMPILE-COMP SYM ARGS X))))
```

関数の名前からしてコードをコンパイルするぞという意欲が見えてくる。
`SYM`は関数名、`ARGS`は引数、そして`X`がコンパイルしたいコードだ。
コードがアトムのときと複合式のときで処理が分かれる。

```lisp
  (DE C::COMPILE-ATOM (X ARGS)
   (COND
    ((NULL X) (LIST 'CONST NIL))
    ((FIXP X) (LIST 'CONST X))
    ((GET X 'APVAL) (C::COMPILE-APVAL (PROP X 'APVAL)))
    ((GET X 'COMMON) (C::COMPILE-GET-ALIST-VAR X))
    ((GET X 'SPECIAL) (C::COMPILE-SPECIAL-VAR (PROP X 'SPECIAL)))
    ((POSITION X ARGS) (C::COMPILE-ARG (POSITION X ARGS)))
    ((SYMBOLP X) (C::COMPILE-GET-ALIST-VAR X))
    (T (ERROR (SYMCAT X '$$| is not supported atom|)))))
```

アトムはNIL、数（fixnum）、グローバル変数、コモン変数、スペシャル変数、
引数、そして謎の変数の場合で処理を分ける。
NILと数はスタックに`X`をプッシュする`(CONST X)` というコードを生成する。
グローバル変数はAPVALセル、
スペシャル変数はスペシャルセルから値を取り出すコードを生成する。

```lisp
  (DE C::COMPILE-APVAL (CELL)
   (LIST 'CALL 'I2I
    (LIST 'LOAD (LIST 'CONST CELL))
    4)) ;; 4: car
  (DE C::COMPILE-SPECIAL-VAR (CELL)
   (LIST 'CALL 'I2I
    (LIST 'LOAD (LIST 'CONST CELL))
    4)) ;; 4: car
```

`(LOAD (CONST CELL))`といったコードが肝だ。
セルはコンパイルした時点で決まるので定数にできる。

コモン変数と謎の変数は環境から値を取り出す。
いわば`ASSOC`を呼ぶようなものだ。

```lisp
  (DE C::COMPILE-GET-ALIST-VAR (X)
   (LIST 'CALL 'II2I
    (LIST 'CONST X)
    (LIST 'CALL 'I2I (LIST 'GET-LOCAL 0) 10)  ;; 10: getAArgFInSubr
    25))  ;; 25: getVarInAlist
```

アトムはコンパイルできたので次は複合式だ。

```lisp
  (DE C::COMPILE-COMP (SYM ARGS X)
   (COND
    ((C::SPECIALFNP (CAR X)) (C::COMPILE-SPECIAL-CALL SYM ARGS X))
    ((ATOM (CAR X)) (C::COMPILE-SYM-CALL SYM ARGS X))
    (T (C::COMPILE-LIST-CALL SYM ARGS (CAR X) (CDR X)))))
```

複合式のコンパイルは式がスペシャルフォーム（っぽいもの）の場合と、
第一要素がシンボルの場合と、
第一要素がリストの場合で処理を分ける。

```lisp
  (DE C::COMPILE-SPECIAL-CALL (SYM ARG X)
   (COND
    ((EQ (CAR X) 'IF) (C::COMPILE-IF-CALL SYM ARG (CDR X)))
    ((EQ (CAR X) 'QUOTE) (C::COMPILE-QUOTE-CALL SYM ARG (CDR X)))
    ((EQ (CAR X) 'LOGAND2) (C::COMPILE-LSUBR-CALL SYM ARG 204 (CDR X)))
    ((EQ (CAR X) 'LOGOR2) (C::COMPILE-LSUBR-CALL SYM ARG 205 (CDR X)))
    ((EQ (CAR X) 'LOGXOR2) (C::COMPILE-LSUBR-CALL SYM ARG 206 (CDR X)))
    ((EQ (CAR X) 'MAX2) (C::COMPILE-LSUBR-CALL SYM ARG 207 (CDR X)))
    ((EQ (CAR X) 'MIN2) (C::COMPILE-LSUBR-CALL SYM ARG 208 (CDR X)))
    ((EQ (CAR X) 'PLUS2) (C::COMPILE-LSUBR-CALL SYM ARG 209 (CDR X)))
    ((EQ (CAR X) 'TIMES2) (C::COMPILE-LSUBR-CALL SYM ARG 210 (CDR X)))
    ((EQ (CAR X) 'FUNCTION) (C::COMPILE-FUNCTION-CALL SYM ARG (CDR X)))
    ((EQ (CAR X) 'LABEL) (C::COMPILE-LABEL-CALL SYM ARG (CDR X)))
    ((EQ (CAR X) 'CSETQ) (C::COMPILE-CSETQ-CALL SYM ARG (CDR X)))
    ((EQ (CAR X) 'SETQ) (C::COMPILE-SETQ-CALL SYM ARG (CDR X)))
    ((EQ (CAR X) 'PROG) (C::COMPILE-PROG-CALL SYM ARG X))  ;; Use whole X
    ((EQ (CAR X) 'RETURN) (C::COMPILE-RETURN-CALL SYM ARG (CDR X)))
    ((EQ (CAR X) 'GO) (C::COMPILE-GO-CALL SYM ARG (CDR X)))
    (T (ERROR (SYMCAT (CAR X) '$$| is not supported special fn|)))))
  (DE C::SPECIALFNP (X)
   (MEMBER X '(IF QUOTE LOGAND2 LOGOR2 LOGXOR2 MAX2 MIN2 PLUS2 TIMES2
    FUNCTION LABEL CSETQ SETQ PROG GO RETURN)))
```

やたらと長いが大きく分けて、
組み込みの可変長引数の関数（を`C::TRANSFORM`が2引数に限定したもの）の処理と、
特定の構文専用の処理に分かれる。
まずは簡単な可変長引数の関数の処理を見てみよう。

```lisp
  (DE C::COMPILE-LSUBR-CALL (SYM ARGS IDX X)
   (C::COMPILE-SUBR-CALL SYM ARGS (LIST IDX 2) X))
```

これは「私は2引数関数です」と主張しながら`C::COMPILE-SUBR-CALL`を呼ぶだけだ。

```lisp
  (DE C::COMPILE-SUBR-CALL (SYM ARGS SB AA)
   (CONC
    (LIST 'PROGN)
    ;; Push alist (10: getAArgFInSubr)
    (LIST (LIST 'CALL 'I2V (LIST 'CALL 'I2I (LIST 'GET-LOCAL 0) 10) 1))
    ;; Push arguments
    (MAPLIST AA
     (FUNCTION (LAMBDA (Y)
      (LIST 'CALL 'I2V (C::ERROR-CHECK
       (C::COMPILE-CODE SYM ARGS (CAR Y))) 1))))  ;; 1: push
    (LIST (LIST 'CALL 'II2I (CAR SB) (LENGTH AA) 20))))  ;; 20: subrCall
```

よく見たら`SB`はCARしか見てないので、
「私は2引数関数です」と主張する意味はまったくなかったことを無視しつつ、
`C::COMPILE-SUBR-CALL`は次のスタックフレームを作るために
まず環境をプッシュして、
引数を順次`C::COMPILE-CODE`してプッシュする。
それができたら関数のインデックスをつかって実際に関数呼び出しを行う。
`C::ERROR-CHECK`は引数の評価に失敗した場合、
途中で関数（正確にはブロック）を抜けてエラーを返す。

```lisp
  (DE C::ERROR-CHECK (ASM)
   (IF (< *OPTIMIZE-SAFETY* 2)
    ASM
    (LIST 'PROGN
     (LIST 'CALL 'I2V ASM 1)
     (LIST 'IF
      (LIST 'CALL 'I2I (LIST 'CALL 'V2I 7) 42)  ;; 7: peek, 42: errorp
      (LIST 'PROGN (LIST 'CALL 'V2I 2) (LIST 'BR-BLOCK))  ;; 2: pop
      (LIST 'CALL 'V2I 2)))))  ;; 2: pop
```

エラー型であればbreakするといったコードを生成する。
これで組み込みの可変長引数の関数呼び出しはコンパイルできた。

`IF`と`QUOTE`の処理は簡単だ。

```lisp
  (DE C::COMPILE-IF-CALL (SYM ARG X)  ;; X of (IF . X=(c th el))
   (LIST 'IF
    (C::ERROR-CHECK (C::COMPILE-CODE SYM ARG (SCAR X)))
    (C::COMPILE-CODE SYM ARG (SCAR (SCDR X)))
    (C::COMPILE-CODE SYM ARG (SCAR (SCDR (SCDR X))))))
  (DE C::COMPILE-QUOTE-CALL (SYM ARG X)  ;; X of (QUOTE . X=(exp))
   (LIST 'CONST (SCAR X)))
```

`IF`は3つの引数を`C::COMPILE-CODE`して`IF`でつなぐだけ。
`QUOTE`に至っては定数を作るだけだ。

`CSETQ`は簡単だが、`SETQ`は思いの外難しい。

```lisp
  (DE C::COMPILE-CSETQ-CALL (SYM ARG X)   ;; X of (CSETQ . X)
   (LIST 'CALL 'II2I
    (LIST 'CONST (CAR X))
    (C::ERROR-CHECK (C::COMPILE-CODE SYM ARG (CADR X)))
    27))  ;; 27: apvalSet
  (DE C::COMPILE-SETQ-CALL (SYM ARG X) (PROG (N)    ;; X of (SETQ . X)
   (SETQ N (POSITION (CAR X) ARGS))
   (RETURN (COND
    ;; Set var in special cell
    ((GET (CAR X) 'SPECIAL)
     (LIST 'PROGN
      (LIST 'CALL 'I2V (C::ERROR-CHECK
       (C::COMPILE-CODE SYM ARG (CADR X))) 1)  ;; 1: push
     (LIST 'STORE
      (LIST 'CALL 'I2I (LIST 'CONST (PROP (CAR X) 'SPECIAL)) 4)  ;; 4: car
      (LIST 'CALL 'V2I 7))  ;; 7: peek
      (LIST 'CALL 'V2I 2)))  ;; 2: pop (val)
    ;; Set var in alist
    ((OR (NOT N) (GET (CAR X) 'COMMON))
     (LIST 'CALL 'III2I
      (LIST 'CONST (CAR X))
      (C::ERROR-CHECK (C::COMPILE-CODE SYM ARG (CADR X)))
      (LIST 'CALL 'I2I (LIST 'GET-LOCAL 0) 10)  ;; 10: getAArgFInSubr
      28))  ;; 28: setVarInAlist
    ;; Set var in stack
    (T
     (LIST 'PROGN
      (LIST 'CALL 'I2V (C::ERROR-CHECK
       (C::COMPILE-CODE SYM ARG (CADR X))) 1)  ;; 1: push
      (IF (< N 4)
       (LIST 'CALL 'II2V
        (LIST 'GET-LOCAL 0)
        (LIST 'CALL 'V2I 7)  ;; 7: peek (val)
        (+ 31 N))  ;; ;; 31: setArgF1
       (LIST 'CALL 'III2V
        (LIST 'GET-LOCAL 0)
        N
        (LIST 'CALL 'V2I 7)  ;; 7: peek (val)
        35))  ;; ;; 35: setArgFN
      (LIST 'CALL 'V2I 2)))))))  ;; 2: pop (val)
```

`CSETQ`は第二引数を`C::COMPILE-CODE`したら、
あとはセルに値を入れるだけなのでWebAssemblyで書かれた関数に任せる。
`SETQ`はスペシャル変数、コモン変数、ローカル変数で処理を分ける。
とはいえ、第二引数を`C::COMPILE-CODE`してから格納するという流れは変わらない。
ただ必要以上に複雑で長いだけだ。

次は入れ子関数を作り出す`FUNCTION`と`LABEL`だ。

```lisp
  (DE C::COMPILE-FUNCTION-CALL (SYM ARG X)  ;; X of (FUNCTION . X=(fn))
   (LIST 'CALL 'II2I
    ;; Create alist (22: createAlistFromStack)
    (LIST 'CALL 'II2I (LIST 'GET-LOCAL 0) (LIST 'CONST ARGS) 22)
    (LIST 'CONST (C::CREATE-SUBR-FROM-LAMBDA (CAR X)))
    24))  ;; 24: createFunarg
  (DE C::COMPILE-LABEL-CALL (SYM ARG X) (PROG (SB)   ;; X of (LABEL . X)
   (SETQ SB (C::CREATE-SUBR-FROM-LAMBDA (CADR X)))
   (RETURN (LIST 'CALL 'III2I
    ;; Create alist (22: createAlistFromStack)
    (LIST 'CALL 'II2I (LIST 'GET-LOCAL 0) (LIST 'CONST ARGS) 22)
    (LIST 'CONST SB)
    (LIST 'CONST (CAR X))
    26))))  ;; 26: createLabelFunarg
  (DE C::CREATE-SUBR-FROM-LAMBDA (FN) (PROG (IDX-OBJ)
   (C::VERIFY0 'LAMBDA FN)
   (SETQ IDX-OBJ (C::COMPILE-LAMBDA 'LAMBDA FN))
   (RETURN (LIST 'SUBR (CAR IDX-OBJ) (LENGTH (CADR FN)) (CDR IDX-OBJ)))))
```

どちらも`C::CREATE-SUBR-FROM-LAMBDA`で入れ子関数をコンパイルしてから、
クロージャを作る。
この`C::CREATE-SUBR-FROM-LAMBDA`は`C::COMPILE-LAMBDA`を呼ぶことにより、
WebAssemblyバイナリコードを生成し、その関数のインデックスを取得する。
そして最後に`(SUBR FN-INDEX NUM-ARGS . CONSTANTS)`というリストを作る。
関数のインデックスさえ分かれば呼び出すことができるのだが、
定数をごみ集めから守るためにリストに含んでいる。
`C::COMPILE-FUNCTION-CALL`と`C::COMPILE-LABEL-CALL`はこのあとに
スタックから環境を作り出す。
入れ子関数に捕まった変数の話を思い出してほしいのだが、
スタックからタグと名前と値の3つ組を参照するようにした。

```
|    | <-- local 0
|NIL |
|NIL |
|NIL |
|    | --> (C::VCTAG N . 100)
|env |
|....|
```

つまり名前があるのだ。名前があれば環境、つまり連想リストが作れる。
`C::CREATE-SUBR-FROM-LAMBDA`をよく見てほしいのだが、
これは純粋に入れ子の関数だけをコンパイルしている。
つまり、そこに外側にどんな変数が存在するかの情報はないのだ。
入れ子関数をコンパイルするときには外側の世界の変数は謎の変数となる。
ここで`C::COMPILE-ATOM`を思い出してほしいのだが、
謎の変数は環境から取り出すコードを生成するようにしていた。
結果として、すべてが上手く動く。
とんでもなく複雑で効率が悪いことを忘れれば。

さて、スペシャルフォームの残りは`PROG`関連のみとなったが、
これは非常に複雑なので後回しにしよう。
話を複合式のコンパイルまで戻すと、
残りは第一要素がアトムの場合とリストの場合だ。
アトムの場合は`C::COMPILE-SYM-CALL`が呼ばれる。

```lisp
  (DE C::COMPILE-SYM-CALL (SYM ARGS X) (PROG (SB FS EX FE)
   (SETQ SB (GET (CAR X) 'SUBR))
   (SETQ FS (GET (CAR X) 'FSUBR))
   (SETQ EX (GET (CAR X) 'EXPR))
   (SETQ FE (GET (CAR X) 'FEXPR))
   (RETURN (COND
    (FE
     (C::COMPILE-FEXPR-CALL SYM ARGS FE X))
    (FS
     (C::COMPILE-FSUBR-CALL SYM ARGS FS X))
    ;; Primitive SUBRs
    ((AND SB (< (CAR SB) 300))  ;; <300 means primitive SUBRs
     (C::COMPILE-SUBR-CALL SYM ARGS SB (CDR X)))
    ;; Prefer global function
    ((OR SB EX)
     (C::COMPILE-FUNC-CALL SYM ARGS (LIST 'CONST (CAR X)) (CDR X)))
    ;; Call local function if exists
    ((AND (MEMBER (CAR X) ARGS)
      (NOT (GET (CAR X) 'COMMON)) (NOT (GET (CAR X) 'SPECIAL)))
     (C::COMPILE-FUNC-CALL SYM ARGS
      (C::COMPILE-ARG (POSITION (CAR X) ARGS)) (CDR X)))
    ;; Special variable
    ((GET (CAR X) 'SPECIAL)
     (C::COMPILE-FUNC-CALL SYM ARGS
      (C::COMPILE-SPECIAL-VAR (PROP (CAR X) 'SPECIAL)) (CDR X)))
    ;; Assume the function will be defined later
    (T (C::COMPILE-FUNC-CALL SYM ARGS (LIST 'CONST (CAR X)) (CDR X)))))))
```

関数の種類、優先順位のせいでコードが複雑になっているが、
基本的には下請け関数が適切にスタックフレームを作って
関数を呼び出すだけだ。
未定義の関数を呼び出そうとした場合、名前経由で呼び出すコードを生成する。
下請け関数は次のようになっている。

```lisp
  ;;; FN and ALST must be an instruction
  (DE C::COMPILE-FUNC-CALL-WITH-ALIST (SYM ARGS FN AA ALST)
   (CONC
    (LIST 'PROGN ALST)
    ;; Push arguments
    (MAPLIST AA
     (FUNCTION (LAMBDA (Y)
      (LIST 'CALL 'I2V (C::ERROR-CHECK
       (C::COMPILE-CODE SYM ARGS (CAR Y))) 1))))  ;; 1: push
    ;; Call FN
    (LIST (LIST 'CALL 'II2I FN (LENGTH AA) 21))))  ;; 21: funcCall
  ;; FN must be an instruction
  (DE C::COMPILE-FUNC-CALL (SYM ARGS FN AA)
   (C::COMPILE-FUNC-CALL-WITH-ALIST SYM ARGS FN AA
    ;; Push the alist (10: getAArgFInSubr)
    (LIST 'CALL 'I2V (LIST 'CALL 'I2I (LIST 'GET-LOCAL 0) 10) 1)))
  (DE C::COMPILE-FSUBR-CALL (SYM ARGS FS E)
   (CONC
    (LIST 'PROGN)
    ;; Push expression
    (LIST (LIST 'CALL 'I2V (LIST 'CONST E) 1))
    ;; Push alist (22: createAlistFromStack)
    (LIST (LIST 'CALL 'I2V
     (LIST 'CALL 'II2I (LIST 'GET-LOCAL 0) (LIST 'CONST ARGS) 22) 1))
    (LIST (LIST 'CALL 'I2I (CAR FS) 23))))  ;; 23: fsubrCall
  (DE C::COMPILE-FEXPR-CALL (SYM ARGS FE E)
   (CONC
    (LIST 'PROGN)
    ;; Push dummy unused alist
    (LIST (LIST 'CALL 'I2V (LIST 'CONST NIL) 1))
    ;; Push arguments
    (LIST (LIST 'CALL 'I2V (LIST 'CONST (CDR E)) 1))
    ;; Push actual alist (22: createAlistFromStack)
    (LIST (LIST 'CALL 'I2V
     (LIST 'CALL 'II2I (LIST 'GET-LOCAL 0) (LIST 'CONST ARGS) 22) 1))
    (LIST (LIST 'CALL 'II2I (LIST 'CONST FE) 2 21))))  ;; 21: funcCall
```

（必要なら）数を評価して、スタックフレームを作り、
あとはWebAssemblyで書かれた関数`funcCall`に任せる。
この`funcCall`のソースは載せないが結構面倒だ。
というのも関数の種類が多いせいだ。
とはいえ面倒なだけで難しいことはしていない。
コンパイル済みの関数からコンパイルされてない関数を呼んだり、
その逆をしたり、LISPの自由度故に場合分けが多くなってしまう。

複合式の最後のケースは第一要素がリストの場合だ。

```lisp
  (DE C::COMPILE-LIST-CALL (SYM ARGS FN AA)
   (COND
    ((EQ (CAR FN) 'LAMBDA)
     (C::COMPILE-FUNC-CALL-WITH-ALIST SYM ARGS
      (LIST 'CONST (C::CREATE-SUBR-FROM-LAMBDA FN)) AA
      ;; Push alist (22: createAlistFromStack)
      (LIST 'CALL 'I2V
       (LIST 'CALL 'II2I (LIST 'GET-LOCAL 0) (LIST 'CONST ARGS) 22) 1)))
    (T (C::COMPILE-FUNC-CALL SYM ARGS (C::COMPILE-CODE SYM ARGS FN) AA))))
```

いわゆるLAMBDA式の場合、入れ子関数をコンパイルしてから呼び出す。
`FUNCTION`や`LABEL`と同様の処理をする。
LAMBDA式でなければ第一要素を関数呼び出しとみなす。

これで`PROG`を除くコードのコンパイルをすべて説明した。

### PROG
LISPの`PROG`はかなり複雑だ。
`(PROG (var1 var2) ...)`のようにローカル変数を作り出したり、
`(PROG () label1 ... (GO label1))`のようにラベルジャンプをしたり、
`(PROG () ... (RETURN 1) ...)`のように途中でリターンしたりできる。

Ichigo Lispは1つのPROGを1つの関数としてコンパイルすることにした。
`(PROG (var1 var2) ...)`はよく見れば`(LAMBDA (var1 var2) ...)`とそっくりだ。
ローカル変数の初期値はNILだが、これは引数が足りない場合の処理が流用できる。
`RETURN`も関数からリターンするだけでいい。

そんなわけで`PROG`に出会うと、内側を入れ子関数としてコンパイルし、
即座に呼び出すコードを生成する。

```lisp
  (DE C::COMPILE-PROG-CALL (SYM ARG X)   ;; X of whole (PROG ...)
     (C::COMPILE-FUNC-CALL-WITH-ALIST SYM ARGS
      (LIST 'CONST (C::CREATE-SUBR-FROM-PROG X))
      NIL  ;; no arguments
      ;; Push alist (22: createAlistFromStack)
      (LIST 'CALL 'I2V
       (LIST 'CALL 'II2I (LIST 'GET-LOCAL 0) (LIST 'CONST ARGS) 22) 1)))
```

`C::CREATE-SUBR-FROM-PROG`は`C::CREATE-SUBR-FROM-LAMBDA`と似ているが、
`GO`や`RETURN`に対応するため、コンパイルの仕方が異なる。

```lisp
  (DE C::CREATE-SUBR-FROM-PROG (PR) (PROG (IDX-OBJ)
   (SETQ IDX-OBJ (C::COMPILE-PROG PR))
   (RETURN (LIST 'SUBR (CAR IDX-OBJ) (LENGTH (CADR FN)) (CDR IDX-OBJ)))))
  (DE C::COMPILE-PROG (EXP) (PROG (BODY LS OBJS ASM)  ;; EXP = (PROG ...)
   (SETQ BODY (C::TRANSFORM (CDDR EXP)))
   (IF *PRINT-LAP* (PRINT BODY))
   (SETQ LS (C::GET-LABELS BODY))
   (SETQ ASM (C::COMPILE-PROG-BODY (LIST 'PROG LS) (CADR EXP) BODY))
   (IF *PRINT-LAP* (PRINT ASM))
   (BSTART)
   (C::ASSEMBLE ASM)
   (SETQ OBJS (C::GET-CONSTS ASM))
   (RETURN (CONS (LOAD-WASM) OBJS))))
```

`C::CREATE-SUBR-FROM-LAMBDA`が`C::COMPILE-LAMBDA`を呼ぶのに対し、
`C::CREATE-SUBR-FROM-PROG`は`C::COMPILE-PROG`を呼び出す。
`C::COMPILE-PROG`は`C::COMPILE-LAMBDA`にそっくりだが、
`C::GET-LABELS`でラベル一覧を取得して、
それを`C::COMPILE-PROG-BODY`に渡すという処理が追加されている。

```lisp
  (DE C::GET-LABELS (BODY)
   (MAPCON BODY (FUNCTION (LAMBDA (X)
    (IF (ATOM (CAR X)) (LIST (CAR X)) NIL)))))
```

`C::GET-LABELS`は簡単だ。
`PROG`から単独で現れるアトムを取り出すだけだ。

```lisp
  (DE C::COMPILE-PROG-BODY (FI ARGS BODY) (PROG (N CV COV SV FRAGMENTS)
   ;; Replace captured variables
   (SETQ CV (C::CAPTURED-VARS ARGS BODY))
   (SETQ COV (REMOVE-IF-NOT (FUNCTION (LAMBDA (X) (GET X 'COMMON))) ARGS))
   (SETQ SV (REMOVE-IF-NOT (FUNCTION (LAMBDA (X) (GET X 'SPECIAL))) ARGS))
   (SETQ BODY (C::REPLACE-CV-REF ARGS BODY CV))
   ;; Create fragments
   (SETQ FRAGMENTS (MAPLIST (CONS NIL (CADR FI)) (FUNCTION (LAMBDA (X)
    (C::GET-PROG-FRAGMENT-AFTER (CAR X) BODY)))))
   (SETQ N 0)
   (RETURN (LIST 'PROGN
    (LIST 'BLOCK
     (CONC (LIST 'PROGN)
      (C::INIT-CV-STACK ARGS CV)
      (C::INIT-COMMON-VARS ARGS COV)
      (C::INIT-SPECIAL-VARS ARGS SV))
     (LIST 'SET-LOCAL 1 0)  ;; $idx = 0
     (LIST 'LOOP (CONS 'PROGN (MAPLIST FRAGMENTS (FUNCTION (LAMBDA (FR)
      (C::COMPILE-PROG-FRAGMENT FI ARGS (CAR FR) (SETQ N (+ N 1))))))))
     (LIST 'CONST NIL))
    (CONC (LIST 'PROGN) (C::CLEANUP-SPECIAL-VARS ARGS SV))))))
```

`C::COMPILE-PROG-BODY`は`C::COMPILE-FUNC`とよく似ているが、
本体を`(LOOP ...)`で囲んでいる点と、
`C::GET-PROG-FRAGMENT-AFTER`を呼び`FRAGMENTS`を作るというところが大きく異なる。
ここでフラグメントとはラベルとラベルで挟まれた部分を指す。
例えば次のようなコードを考える。

```lisp
(PROG ()
  (A) (B)
 LABEL1
  (C) (D) (E)
 LABEL2
  (F))
```

`PROG`の始まりから`LABEL1`に挟まれた`(A) (B)`が最初のフラグメント、
`LABEL1`と`LABEL2`で挟まれた`(C) (D) (E)`が次のフラグメント、
`LABEL2`と`PROG`の終わりで挟まれた`(F)`が最後のフラグメントだ。

```lisp
  (DE C::GET-PROG-FRAGMENT-AFTER (LBL BODY)
   (IF (NULL LBL)
    (IF (OR (NULL BODY) (ATOM (CAR BODY)))
     NIL
     (CONS (CAR BODY) (C::GET-PROG-FRAGMENT-AFTER LBL (CDR BODY))))
    (COND
     ((NULL BODY) (ERROR (SYMCAT '$$|Label not found: | LBL)))
     ((EQ (CAR BODY) LBL) (C::GET-PROG-FRAGMENT-AFTER NIL (CDR BODY)))
     (T (C::GET-PROG-FRAGMENT-AFTER LBL (CDR BODY))))))
```

`C::GET-PROG-FRAGMENT-AFTER`はラベル`LBL`の次のフラグメントを返す。
これを`MAPLIST`で呼んでやればすべてのフラグメントが手に入る。
フラグメントを手に入れたらそれらをコンパイルする。

```lisp
  (DE C::COMPILE-PROG-FRAGMENT (FI ARGS FRGM N)
   (LIST 'WHEN (LIST '< (LIST 'GET-LOCAL 1) N)
    (CONS 'PROGN (MAPLIST FRGM (FUNCTION (LAMBDA (E)
     (C::COMPILE-PROG-CODE FI ARGS (CAR E))))))))
```

基本的には`C::COMPILE-PROG-CODE`するだけなのだが、
`(WHEN (< (GET-LOCAL 1) N) ...)`という余分なものがついている。
フラグメントには整数の番号が振られ、
ローカル変数1番は`GO`によるジャンプ先が入る。
この意味を知るには`GO`の処理を読むのが良い。

```lisp
  (DE C::COMPILE-GO-CALL (FI ARG X)   ;; X of (GO . X=(label))
   (IF (OR (ATOM FI) (NOT (EQ (CAR FI) 'PROG)))
    (ERROR '$$|GO cannot be used outside PROG|)
    (LIST 'PROGN
     (LIST 'SET-LOCAL 1 (1+ (POSITION (CAR X) (CADR FI))))
     (LIST 'BR-LOOP))))
```

`C::COMPILE-GO-CALL`は飛び先のラベルをフラグメントの番号に直したものを
ローカル変数1番に入れて、ループの先頭に戻る。

```lisp
(PROG ()
  (FRAGMENT1)
 LABEL1
   (FRAGMENT2)
 LABEL2
   (FRAGMENT3))
```

このようなコードを考えたとき、`LABEL1`にジャンプするというのは、
`PROG`の先頭に戻り、`FRAGMENT1`を無視して`FRAGMENT2`から実行するのと同じだ。
`LABEL2`にジャンプするというのは、`PROG`の先頭に戻り、
`FRAGMENT1`と`FRAGMENT2`を無視して`FRAGMENT3`から実行するのと同じだ。
この「無視する」というのが`(WHEN (< (GET-LOCAL 1) N) ...)`によって実現される。

どうしてこんな面倒な事をやっているかというと、WebAssemblyにGOTOがないからだ。
GOTOがない言語でGOTOを実現するのは骨が折れる。
詳しい話は[この記事](/2022/wasm_goto2.html)に書いた。

`GO`と比べ、`RETURN`は簡単だ。

```lisp
  (DE C::COMPILE-RETURN-CALL (FI ARG X)   ;; X of (RETURN . X)
   (IF (OR (ATOM FI) (NOT (EQ (CAR FI) 'PROG)))
    (ERROR '$$|RETURN cannot be used outside PROG|)
    (LIST 'PROGN (C::COMPILE-CODE FI ARG (CAR X)) (LIST 'BR-BLOCK))))
```

`C::COMPILE-CODE`で戻り値を作り出し、
`(BR-BLOCK)`は関数本体を抜ける。
ただそれだけだ。

これですべてのコードがコンパイルできるようになった。
のこりはアセンブルだけだ。

## C::ASSEMBLE

`C::ASSEMBLE`は`C::COMPILE-FUNC`（や`C::COMPILE-PROG-BODY`）が作り出した
アセンブリコードをWebAssemblyのバイナリ表現にアセンブルする。

```lisp
  (DE C::ASSEMBLE (ASM) (PROG (SIDX)
   (SETQ SIDX (NEXT-SUBR))
   (C::WASM-HEADER )
   (C::TYPE-SECTION)
   (C::IMPORT-SECTION 8 1024)
   (C::FUNC-SECTION)
   (C::ELM-SECTION SIDX)
   (C::CODE-SECTION ASM)
  ))
```

`NEXT-SUBR`は次の関数のインデックスを返す、
WebAssemblyで書かれた関数だ。整数を1つ増やして返すだけなので何も面白くない。
`C::WASM-HEADER`から`C::ELM-SECTION`までの呼び出しは、
引数`ASM`を使っていないことからわかるように、
すべての関数で（インデックスを除き）共通のコードを生成する。

```lisp
  (DE C::WASM-HEADER () (PROG ()
   (BWRITES '(0x00 0x61 0x73 0x6d))
   (BWRITES '(0x01 0x00 0x00 0x00))
  ))
  (DE C::TYPE-SECTION () (PROG ()
   (BWRITE 0x01)  ;; section number
   (BWRITE 0x26)  ;; section size
   (BWRITE 0x07)  ;; 7 entry
   (BWRITE 0x60)  ;; functype (void -> i32)
   (BWRITE 0x00)  ;; no arguments
   (BWRITE 0x01)  ;; 1 value
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x60)  ;; functype (i32 -> void)
   (BWRITE 0x01)  ;; 1 parameter
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x00)  ;; 0 value
   (BWRITE 0x60)  ;; functype (i32 -> i32)
   (BWRITE 0x01)  ;; 1 parameter
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x01)  ;; 1 value
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x60)  ;; functype (i32*i32 -> i32)
   (BWRITE 0x02)  ;; 2 parameters
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x01)  ;; 1 value
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x60)  ;; functype (i32*i32 -> void)
   (BWRITE 0x02)  ;; 2 parameters
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x00)  ;; 0 value
   (BWRITE 0x60)  ;; functype (i32*i32*i32 -> i32)
   (BWRITE 0x03)  ;; 3 parameters
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x01)  ;; 1 value
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x60)  ;; functype (i32*i32*i32 -> void)
   (BWRITE 0x03)  ;; 3 parameters
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x7f)  ;; i32
   (BWRITE 0x00)  ;; 0 value
  ))
  (DE C::IMPORT-SECTION (MEM TBL) (PROG (MS TS SS)
   (SETQ MS (ULEB128 MEM))
   (SETQ TS (ULEB128 TBL))
   (SETQ SS (+ 0x19 (LENGTH MS) (LENGTH TS)))
   (BWRITE 0x02)  ;; section number
   (BWRITE SS)  ;; section size
   (BWRITE 0x02)  ;; 2 entries
  ;js"memory" memory limit is ...
   (BWRITES '(0x02 0x6a 0x73 0x06 0x6d 0x65 0x6d 0x6f 0x72 0x79 0x02 0x00))
   (BWRITES MS)  ;; memory size
  ;js"table" table limit is ...
   (BWRITES '(0x02 0x6a 0x73 0x05 0x74 0x61 0x62 0x6c 0x65 0x01 0x70 0x00))
   (BWRITES TS)  ;; table size
  ))
  (DE C::FUNC-SECTION () (PROG ()
   (BWRITE 0x03)  ;; section number
   (BWRITE 0x02)  ;; section size
   (BWRITE 0x01)  ;; 1 entry
   (BWRITE 0x00)  ;; type index 0 (v2i)
  ))
  (DE C::ELM-SECTION (SIDX) (PROG (I)
   (SETQ I (ULEB128 SIDX))
   (BWRITE 0x09)  ;; section number
   (BWRITE (+ 0x06 (LENGTH I)))  ;; section size
   (BWRITE 0x01)  ;; 1 entry
   (BWRITE 0x00)  ;; table index 0
   (BWRITE 0x41)  ;; i32.const
   (BWRITES I)
   (BWRITE 0x0b)  ;; end
   (BWRITE 0x01)  ;; 1 function
   (BWRITE 0x00)  ;; function index 0 (see function section)
  ))
```

`BWRITE`はバッファに1バイト書き出すWebAssemblyで書かれた関数だ。
`ULEB128`はWebAssemblyで使われている整数エンコードのための関数だ。

```lisp
  (DE ULEB128 (N) (PROG (B V)
   (SETQ B (LOGAND N 0x7F))
  ;; Suppress sign extension. Note that fixnum is 30 bit.
   (SETQ V (LOGAND (LEFTSHIFT N -7) 0x7fffff))
   (RETURN
    (IF (ZEROP V)
     (CONS B NIL)
     (CONS (LOGOR B 0x80) (ULEB128 V))))))
  (DE LEB128 (N) (PROG (B V)
   (SETQ B (LOGAND N 0x7F))
   (SETQ V (LEFTSHIFT N -7))
   (RETURN
    (IF (OR (AND (ZEROP V) (ZEROP (LOGAND B 0x40)))
            (AND (EQ V -1) (NOT (ZEROP (LOGAND B 0x40)))))
     (CONS B NIL)
     (CONS (LOGOR B 0x80) (LEB128 V))))))
```

ここまでで面白いところは特に無いだろう。
気になることがある人はWebAssemblyの仕様書でも読んでもらいたい。
ここからが本番だ。

```lisp
  (DE C::CODE-SECTION (ASM) (PROG (INST LEN CS SS)
   (SETQ INST (C::ASSEMBLE-CODE ASM 0))
   (SETQ LEN (LENGTH INST))
   (SETQ CS (ULEB128 (+ 0x0b (LENGTH INST))))
   (SETQ SS (ULEB128 (+ 0x0c (LENGTH INST) (LENGTH CS))))
   (BWRITE 0x0a)  ;; section number
   (BWRITES SS)  ;; section size
   (BWRITE 0x01)  ;; 1 entry
   (BWRITES CS)  ;; code size
   (BWRITE 0x01)  ;; 1 local variable sets
   (BWRITE 0x02)  ;; 2 local variables with the same type ($frame, $idx)
   (BWRITE 0x7f)  ;; i32
  ;; Init frame pointer
   (BWRITE 0x41)  ;; i32.const
   (BWRITE 0x00)  ;; 0 ($getsp)
   (BWRITE 0x11)  ;; call_indirect
   (BWRITE 0x00)  ;; type index 0 (v2i)
   (BWRITE 0x00)  ;; end of call_indirect
   (BWRITE 0x21)  ;; local.set
   (BWRITE 0x00)  ;; local index 0 ($frame)
  ;;; Body instructions
   (BWRITES INST)
   (BWRITE 0x0b)  ;; end
  ))
```

この関数は、コードセクションの情報に続けて、
ローカル変数0番にスタックポインタを入れるコードを生成し、
そしてアセンブルしたコードを続ける。

```lisp
  (DE C::ASSEMBLE-CODE (X L)
   (COND
    ((ATOM X) (C::ASSEMBLE-ATOM X L))
    ((EQ (CAR X) 'CONST) (C::ASSEMBLE-CONST (CADR X) L))
    ((EQ (CAR X) 'GET-LOCAL) (C::ASSEMBLE-GET-LOCAL (CADR X) L))
    ((EQ (CAR X) 'SET-LOCAL) (C::ASSEMBLE-SET-LOCAL (CDR X) L))
    ((EQ (CAR X) 'CALL) (C::ASSEMBLE-CALL (CDR X) L))
    ((EQ (CAR X) 'LOAD) (C::ASSEMBLE-LOAD (CDR X) L))
    ((EQ (CAR X) 'STORE) (C::ASSEMBLE-STORE (CDR X) L))
    ((EQ (CAR X) 'PROGN) (C::ASSEMBLE-PROGN (CDR X) L))
    ((EQ (CAR X) 'BLOCK) (C::ASSEMBLE-BLOCK (CDR X) L))
    ((EQ (CAR X) 'IF) (C::ASSEMBLE-IF (CDR X) L))
    ((EQ (CAR X) 'WHEN) (C::ASSEMBLE-WHEN (CDR X) L))
    ((EQ (CAR X) 'LOOP) (C::ASSEMBLE-LOOP (CDR X) L))
    ((EQ (CAR X) '<) (C::ASSEMBLE-LESS (CDR X) L))
    ((EQ (CAR X) 'RETURN) (C::ASSEMBLE-RETURN (CADR X) L))
    ((EQ (CAR X) 'BR-LOOP) (C::ASSEMBLE-BR-LOOP (CDR X) L))
    ((EQ (CAR X) 'BR-BLOCK) (C::ASSEMBLE-BR-BLOCK (CDR X) L))
    (T (ERROR (SYMCAT (CAR X) '$$| is not asm opcode|)))))
```

`C::ASSEMBLE-CODE`はこれまで`C::COMPILE-XXX`が作り出した
アセンブリコードを実際にバイナリに変換する。

```lisp
  (DE C::ASSEMBLE-ATOM (X L)
   (COND
    ((FIXP X) (CONS 0x41 (LEB128 X)))
    (T (ERROR (SYMCAT X '$$| is not supported asm instruction|)))))
```

アセンブリコードに整数が現れるケースが本当にあるのか怪しいが、
`C::ASSEMBLE-ATOM`は整数をスタックに置く命令 (0x41) に変換を生成する。

```
  (DE C::ASSEMBLE-CONST (X L)  ;; X of (CONST X)
   (COND
    ((NULL X) (LIST 0x41 0x00))
    ((FIXP X) (CONS 0x41 (LEB128 (C::ENCODE-FIXNUM X))))
    ((OR (SYMBOLP X) (CONSP X)) (CONS 0x41 (LEB128 (FENCODE X))))
    (T (ERROR (SYMCAT X '$$| is not supported const|)))))
  (DE C::ENCODE-FIXNUM (X)
   (+ (LEFTSHIFT X 2) 2))
```

定数命令`CONST`はNILの場合は0x00をスタックに置く (0x41) 命令を作り、
整数の場合はエンコードのための調整をしたあとにスタックに置く。
シンボルの場合もアドレスを調整してからスタックに置く。
`FENCODE`はWebAssemblyで書かれた関数だ。
WebAssemblyの本物の整数とLISPのfixnumが微妙に食い違うせいで、
どうしても厄介な処理が生まれてしまうのが悲しい。

```
  (DE C::ASSEMBLE-GET-LOCAL (X L)  ;; X of (GET-LOCAL X)
   (CONS 0x20 (LEB128 X)))
  (DE C::ASSEMBLE-SET-LOCAL (X L)  ;; X of (SET-LOCAL X=(idx val))
   (NCONC (C::ASSEMBLE-CODE (CADR X) L) (CONS 0x21 (LEB128 (CAR X)))))
```

`GET-LOCAL`はローカル変数を取り出す命令 (0x20) を作る。
`SET-LOCAL`は値を作り出す命令を生成して、
それに続けてローカル変数に書き込む命令 (0x21) を作る。

```lisp
  (DE C::ASSEMBLE-TYPE (X)
   (COND
    ((EQ X 'V2I) 0)
    ((EQ X 'I2V) 1)
    ((EQ X 'I2I) 2)
    ((EQ X 'II2I) 3)
    ((EQ X 'II2V) 4)
    ((EQ X 'III2I) 5)
    ((EQ X 'III2V) 6)
    (T (ERROR (SYMCAT X '$$| is not supported type|)))))
  (DE C::ASSEMBLE-CALL (X L)  ;; X of (CALL . X=(TYPE . ARGS))
   (NCONC
    ;; Push arguments
    (MAPCON (CDR X) (FUNCTION (LAMBDA (Y)
     (C::ASSEMBLE-CODE (CAR Y) L))))
    ;; Call the function.
    (LIST 0x11 (C::ASSEMBLE-TYPE (CAR X)) 0x00)))  ;; call_indirect
```

`CALL`は引数を作り出す命令を生成して、
関数呼び出しをする命令 (0x11) を続ける。
型をタイプセクションに合わせた番号に変換する処理もする。

```lisp
  (DE C::ASSEMBLE-LOAD (X L)  ;; X of (LOAD . X=(CELL))
   (NCONC
    ;; Push the address
    (MAPCON X (FUNCTION (LAMBDA (Y)
     (C::ASSEMBLE-CODE (CAR Y) L))))
    ;; Load
    (LIST 0x28 0x02 0x00)))  ;; align=2 (I'm not sure if it's necessary)
  (DE C::ASSEMBLE-STORE (X L)  ;; X of (STORE . X=(CELL VAL))
   (NCONC
    ;; Push the address and value
    (MAPCON X (FUNCTION (LAMBDA (Y)
     (C::ASSEMBLE-CODE (CAR Y) L))))
    ;; Store
    (LIST 0x36 0x02 0x00)))  ;; align=2 (I'm not sure if it's necessary)
```

`LOAD`と`STORE`は自信なさげなコメントが付いているが、
格納場所（および値）を作り出す命令を生成して、
読み込み (0x28) 、書き込み (0x36) を続ける。

```lisp
  (DE C::ASSEMBLE-PROGN (X L)  ;; X of (PROGN . X)
   (MAPCON X (FUNCTION (LAMBDA (Y)
     (C::ASSEMBLE-CODE (CAR Y) L)))))
```

`PROGN`は特に固有の命令を生成しない。
引数を順次アセンブルするだけだ。

```lisp
  (DE C::ASSEMBLE-BLOCK (X L)  ;; X of (BLOCK . X)
   (CONC
    (LIST 0x02 0x7f)  ;; block with i32
    (MAPCON X (FUNCTION (LAMBDA (Y)
      (C::ASSEMBLE-CODE (CAR Y) (1+ L)))))
    (LIST 0x0b)))
```

`BLOCK`はWebAssemblyにおけるブロックを作り出す。
`(1+ L)`で入れ子レベルを増やしていることに注意。
忘れているかもしれないが、`BLOCK`は
`C::COMPILE-PROG-BODY`と`C::COMPILE-FUNC`が
下準備と本体のコードを囲むのに使っている。
本体を途中で抜けても後始末が行われるようにするために
`BLOCK`が使われているのだ。
低レベルな言語にブロックなんて命令があることに驚く人もいるかも知れないが、
WebAssemblyとはそういうものなのだ。

```lisp
  (DE C::ASSEMBLE-IF (X L)  ;; X of (IF . X)
   (CONC
    (C::ASSEMBLE-CODE (CAR X) L)
    (LIST 0x04 0x7f)  ;; if with i32
    (C::ASSEMBLE-CODE (CADR X) (1+ L))
    (LIST 0x05)  ;; else
    (C::ASSEMBLE-CODE (CAR (CDDR X)) (1+ L))
    (LIST 0x0b)))
  (DE C::ASSEMBLE-WHEN (X L)  ;; X of (WHEN . X)
   (CONC
    (C::ASSEMBLE-CODE (CAR X)) L
    (LIST 0x04 0x40)  ;; if without value
    (C::ASSEMBLE-CODE (CADR X) (1+ L))
    (LIST 0x0b)))
```

`IF`と`WHEN`は条件部のコードをアセンブルしたあとに、
分岐のための命令 (0x04) が続く。
ジャンプ命令がなく、 begin (0x04) と end (0x0b) のような構造を持つことに
驚く人もいるかも知れないが、WebAssemblyとはそういうものなのだ。

```lisp
  (DE C::ASSEMBLE-LOOP (X L)  ;; X of (LOOP . X)
   (CONC
    (LIST 0x03 0x40)  ;; loop without value
    (C::ASSEMBLE-CODE (CAR X) (1+ L))
    (LIST 0x0b)))
```

`LOOP`は`BLOCK`とだいたい同じなので説明することがない。
名前の通りループする。
これは`PROG`のために使われる。
末尾再帰最適化のようなカッコいいことはしていない。

```lisp
  (DE C::ASSEMBLE-LESS (X L)  ;; X of (< . X=(a b))
   (CONC
    (C::ASSEMBLE-CODE (CAR X) L)
    (C::ASSEMBLE-CODE (CADR X) L)
    (LIST 0x48)))  ;; i32.lt_s
  (DE C::ASSEMBLE-RETURN (X L)  ;; X of (RETURN X)
   (CONC
    (C::ASSEMBLE-CODE X L)
    (LIST 0x0f)))  ;; return
  (DE C::ASSEMBLE-BR-LOOP (X L)  ;; X of (BR-LOOP . X=NIL)
    (CONS 0x0c (LEB128 (- L 2))))  ;; br
  (DE C::ASSEMBLE-BR-BLOCK (X L)  ;; X of (BR-BLOCK . X=NIL)
    (CONS 0x0c (LEB128 (- L 1))))  ;; br
```

`<`, `RETURN`, `BR-LOOP`, `BR-BLOCK`はすべて`PROG`のために使われる。
`<`はラベルジャンプ（っぽいなにか）のために、
`RETURN`は処理を途中で抜けるために、
`BR-LOOP`はラベルジャンプ時に`PROG`の先頭に戻るために、
`BR-BLOCK`は処理を途中で抜けるために使われる。
`RETURN`と`BR-BLOCK`の役割が完全にかぶっている上に、
`RETURN`は後始末が実行できないという問題があるので、
実は`RETURN`は今は使っていないのではないかという気もしてきたが、
気づかなかったことにしておく。

驚くべきことにソースコードはこれですべてだ。
これだけでLISPがWebAssemblyにコンパイルされてしまう。
もっともランタイム・インタプリタのためのWebAssemblyコードが山程あるのだが。

## 免責事項

私はコンパイラの専門家でもなければLISPの専門家でもない。
Ichigo Lispは特定の実装や書籍を参考に書いたのではなく、
私の脳の憶測に眠るうろ覚えな知識をごちゃ混ぜにしながら書いたものだ。
大体の仕組みは机に向かって考えたものではなく、走りながら考えた。
単純な関数をコンパイルする仕組みは川沿いを走っているときに考えた。
入れ子になった関数をコンパイルする仕組みは海沿いを走っているときに考えた。
何が言いたいかというと色々と雑だと言いたいのだ。
そんなわけで真っ当な計算機科学の知識がある方には
かなりお見苦しい内容だったと思うがどうか許してもらいたい。
許せない方はぜひとも何らかのコンパイラを実装して
ソースコードをすべて解説していただきたい。

*2023-09-18*
