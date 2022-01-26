# Portable PrologをCommon Lispで動かした

bit 1982年05月号『Prolog入門（2）』に
Portable PrologというLISPで書かれた小さなProlog処理系が載っている。
"Portable" という名前の通り色々な環境で動かすことを前提としており、
ほぼLISP 1.5の機能のみで書かれているため移植が簡単にできるようになっている。
これを少し書き換えてCommon Lispで動かした。

## 移植の方針
今回は「もとのプログラムを極力書き換えない」という方針で移植することにした。
コードを読んでいると `prog` を `let` に書き換えたり、
`cond` を `when` に書き換えたりしたくなるような箇所が多々あるが、
そういった気持ちをぐっとこらえ、最小限の変更のみを行った。

## 実際に変更した箇所
もとのコードが150行ほど。
これに対して変更したのが10行。新たに追加したのが3行。
書き換えた箇所の大部分はそもそもLISP 1.5にない機能を使っているところ、
つまり必ずしも portable とはいえないところなので、変更するのもしかたない。
極力書き換えないという方針は十分に守れたと思う。

ちなみに、あまりにももとのコードが残りすぎているので
ソースコードをここには載せないことにした。
読みたい人はAmazonなどでbitを買った上で以下の変更点を見てもらいたい。

### ダイナミックスコープへの対応
もとのコードはLISP 1.5なのでダイナミックスコープを使っている。
Common Lispで実現するのは簡単で、単に `special` と宣言すれば良い。

```lisp
(DECLAIM (SPECIAL
          FETCHED-SUBST EPILOG NEW-SUBST CLAUSE OLD-SUBST CUE UNDO-LIST))
```

宣言すべき変数はコードを読めばわかるが、
SBCLであれば関数をコンパイルした際に出る警告（undefined variable）
を読めば確実に対応できる。

### 属性リストの書き換え
もとのコードでは属性リストを書き換えるために `PUTPROP` を使っているが、
Common Lispでは `(setf (get ...) ...)` を使うことになる。
ちなみにLISP 1.5には `PUTPROP` はない。
純粋なLISP 1.5だと `PROP` を使って自分で定義する必要がある。

### シンボルの分解
Portable Prologでは先頭に `*` が付くシンボルが（Prologの）変数になるのだが、
これを実現するために（LISPの）シンボルの先頭の文字を取り出す必要がある。
もとのコードでは `CHARACTER` という関数を使っていたが、
Common Lispでは `symbol-name` と `aref` あたりを使うことになる。
ちなみに `CHARACTER` はLISP 1.5にはない。
純粋なLISP 1.5だと `UNPACK` を使うことになると思う。

### シンボルのエスケープ
Portable Prologでは `+` と `-` という記号が使われるが、
古いLISPでは+と-から始まるアトムは数になってしまうため、
`'/+` とか `'/-` といったように `/` でエスケープしている。
Common Lispでは `/` を取って `'+` とか `'-` と書けば良い。
ちなみに、この `/` はMACLISPなどの機能でLISP 1.5にはない。
純粋なLISP 1.5だとおそらく `'$$*+*` のように書くのだと思う。

### MAPCAR と LAMBDA
`(MAPCAR (CDDR FORM) '(LAMBDA ...))` のようなコードがあるが、
これはCommon Lispだと `(mapcar #'(lambda ...) (cddr form))` のように
引数の順番が入れ替わる。また、 `'` ではなく `#'` を使う。
ちなみにLISP 1.5にはMAPCARはない。
純粋なLISP 1.5だと自分で定義する必要がある。

### コロンの読み込み
Portable Prologでは節の終わりに `:` を置く。
Common Lispのreadで `:` を読むとリーダマクロが動いてしまう。
リードテーブルをいじってもいいのだが面倒なので、
コロンではなく別の記号を使うことにした。
前述の通り、+, -, * といった記号が他に使われており、
どことなく四則演算を感じるので `/` を使うことにした。
節の終わりに `/` を書くのは少々気持ち悪いが大した問題ではないだろう。

### PRINTの改行
Portable Prologの `(PROLOG)` という関数を呼ぶと、
`READ` と `PRINT` を繰り返すのだが、
Common Lispでこれをやると、 `print` のあとに改行されずに `read` が動き出し、
非常に見栄えが悪い。そこで、 `read` の直前に `terpri` を入れてやることにした。

## 動作確認
というわけで書き換えたコード動かした。
Portable Prologの文法は癖があり、
先頭が `+` なら節の定義、先頭が `-` なら質問となり、
頭部、各ゴールは `-` で区切る。
終端には本来 `:` が付くが、前述の通り `/` を代わりに使っている。
変数は先頭に `*` が付く。

```lisp
CL-USER> (prolog)

(PORTABLE PROLOG (IN LISP))
;;; failを定義するために = を定義する
;;; eq(X, X).
+(= *x *x)/

DEFINED
;;; バックトラックを見るためにfailを定義する
;;; fail :- eq(1, 2).
+(fail) -(= 1 2)/

DEFINED
;;; append([], X, X).
+(append () *x *x)/

DEFINED
;;; append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
+(append (*x . *xs) *ys (*x . *zs)) -(append *xs *ys *zs)/

DEFINED
;;; appendを呼んでみても成功したということしか分からない
-(append (a b) (c) *z)/

*S*
;;; callでLISPのprintを呼ぶと変数*zが見れる
-(append (a b) (c) *z) -(call print *z)/

(A B C)
*S*
;;; appendを逆向きに使う
-(append *x *y (a b c)) -(call print *x) -(call print *y)/

NIL
(A B C)
*S*
;;; すべての組み合わせを見るためにfailを使う
-(append *x *y (a b c)) -(call print *x) -(call print *y) -(call terpri) -(fail)/

NIL
(A B C)

(A)
(B C)

(A B)
(C)

(A B C)
NIL

NIL
;;; 終了
-(end)/
EPILOG
CL-USER>
```

## おわりに
もともと移植しやすいように書かれたコードだけあって、
かなり簡単にCommon Lispで動かせた。
レキシカルスコープを採用しながらも、
宣言一つでダイナミックスコープを使える
Common Lispの懐の深さも十分に感じることができて楽しかった。

*2022-01-26*