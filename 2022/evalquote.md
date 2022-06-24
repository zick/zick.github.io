# EVALQUOTEの話

## 動機

「LISPといえばS式だが、昔はM式というものがあった」
という話はなぜか多くの人が知っている [要出典] 。
しかしM式は人間が読み書きするものであり、計算機が読み書きするものではない。
昔の計算機のためのLISPプログラムの記述法を知る人は少ない [要出典] 。
具体的には[吸血鬼関数の話](/2022/vampire.html)を読んだ人から
「（このプログラムって）S式じゃないんですか？」という質問がやってきた。
そんなわけで昔のLISPプログラムの記述法の話をする。

## 実例

今どきのLispにおける `(car '(a b c))` は、
昔のLISPだと `CAR ((A B C))` のように書いた。
これだけだとよく分からないのでもっと例をあげる。

```lisp
(cons 1 2)  ;; 今
CONS (1 2)  ;; 昔

(cons 'a 'b)  ;; 今
CONS (A B)    ;; 昔

(cons 'a '(b))  ;; 今
CONS (A (B))    ;; 昔

(car '(a b c))  ;; 今
CAR ((A B C))   ;; 昔

((lambda (x) (cons x x)) 1)  ;; 今
(LAMBDA (X) (CONS X X)) (1)  ;; 昔

((lambda (x) (cons 'x x)) 'a)        ;; 今
(LAMBDA (X) (CONS (QUOTE X) X)) (A)  ;; 昔

((lambda (x) (cons (car x) x)) '(a b))  ;; 今
(LAMBDA (X) (CONS (CAR X) X)) ((A B))   ;; 昔
```

最初の `CONS (1 2)` から想像が付くように、
今どきのLispだと `(関数 引数1 引数2)` と書くところを、
昔のLISPは `関数 (引数1 引数2)` と書いた。
次の `CONS (A B)` から推測できるが、引数は勝手にクォートされる。
リストも当然クォートされるので `CAR ((A B C))` のような書き方になる。
`CAR (A B C)` と書くとCARに3つの引数を渡すことになってしまうので注意。
LAMBDAが出てくると少し面白くなってくる。基本は同じで
`((LAMBDA ...) 引数1 引数2)` は `(LAMBDA ...) (引数1 引数2)` となる。
面白いのはLAMBDAの内側だ。内側に関しては今と全く同じ記法になる。
具体的には `(関数 引数1 引数2)` という記法で、
引数が勝手にクォートされることはない。

## 正体

この古い記法を使うLISPはEvalquote Lispと呼ばれる。
今どきの記法を使うLispはEval Lispと呼ばれる。
Eval Lispは入力した式がevalによって評価される。
そこから想像できるように、
Evalquote Lispは入力した式がevalquoteによって評価される。

evalquoteの定義はLISP 1.5 Programmer's Manualでは次のように書かれている。

```
evalquote[fn;x] = apply[fn;x;NIL]
```

EVALQUOTEとはAPPLYである

ちなみにAPPLYの第3引数は環境を表す連想リストだ。
今どきのLispにはないことを考えると、evalquoteは完全にapplyだと言える。

今どきの記法はevalに合わせたもの、古い記法はapplyに合わせたものと考えると、
すべて納得がいくだろう。
関数と引数を分けて書くのは当然。
引数がクォートされるというのも納得がいく。
applyは受け取った引数を評価したりしないからだ。

## 理由

しかし、なぜevalquoteなどという関数を導入したのだろうか。
具体的な説明を見つけることはできなかったが、
LISP I Programmer's Manualを読めば少し想像ができる。
LISP IはEval LispでもなければEvalquote Lispでもない。
強いて名前をつけるならApply Lispだろう。
LISP Iでは関数と引数と環境の3つ組を入力する。
例えば `CAR ((A B C)) ()` のように書く。
3つ組はそのままAPPLYに渡される。
LISP I Programmer's Manualには
"The APPLY operator is the interpreter part of the LISP system"
とはっきりと書かれている。昔のLISPはevalではなくapplyが主役だったのだ。

しかしながらグローバル変数があれば環境が役に立つ場面はあまりなさそうだ。
そう考えると毎回NILを入力するのもバカバカしいし、
evalquoteというものが導入されたのも分からなくもない。

## 将来

LISP Iは関数と引数と環境の3つ組を受け取った。
LISP 1.5は関数と引数の2つ組を受け取った。
そして今どきのLispは式のみ（1つ組）を受け取る。
そう考えると将来のLispは何も受け取らないと考えるのが自然だろう。
対話環境でLispが対話を拒み一方的に話し続けると考えると面白そうだ。
詳細な設計は読者の練習問題とする。

*2022-06-24*
