# 吸血鬼関数の話

bit 1981年03月号『ナノピコ教室─解答(Lisp Poetry)』に
「吸血鬼関数」という面白いプログラムが載っている。
「吸血鬼に血を吸われた人は吸血鬼になる」という話のごとく、
吸血鬼関数に血を吸われた関数は吸血鬼関数になってしまう。

```lisp
DEFINE((
  (VAMPIRE(LABEL V(LAMBDA(G)(PROG ()
    (COND((NOT(ATOM G)) (RETURN(PROG2(V(CAR G))(V(CDR G))))))
    (SETQ V(LIST (QUOTE LABEL)(QUOTE V)V))
    (COND((MEMBER G(QUOTE(PROG REMPROP ATTRIB LIST RETURN GENSYM
                          SETQ EQ CAR ATOM RPLACD MEMBER NOT
                          MAPLIST PROG2 RPLACA H CDR EQUAL GET
                         )))
          (PROG(XX)
            (SETQ XX (GENSYM))
            (RPLACD XX (MAPLIST (CDR G) (QUOTE CAR)))
            ( (LABEL H(LAMBDA(Z)(COND
                    ((ATOM Z)NIL)
                    ((EQ G(CAR Z))(PROG2(RPLACA Z XX)(H (CDR Z))))
                    (T(PROG2(H (CAR Z))(H(CDR Z))))
                    )))
              V
            )
          )
    ))
    (REMPROP G (QUOTE EXPR))
    (REMPROP G (QUOTE FEXPR))
    (REMPROP G (QUOTE SUBR))
    (REMPROP G (QUOTE FSUBR))
    (ATTRIB G (LIST (QUOTE EXPR)V))
    (RETURN(EQUAL (GET G (QUOTE EXPR)) V))
  ))))
))
```

`VAMPIRE`は1引数関数で、シンボルまたはリストを受け取る。
シンボルを受け取ると、そのシンボルと結びついた元々の関数定義を消し去り、
代わりに吸血鬼関数の定義で置き換えてしまう。
リストを受け取ると、CARとCDRに分解しシンボルを見つけるまで再帰する。

```lisp
> VAMPIRE(APPEND)  ; APPENDの血を吸う
*T*
> GET(APPEND EXPR)  ; APPENDが吸血鬼関数に置き換わっている
(LABEL V (LAMBDA (G) ...))
```

この関数の面白いところは、
`VAMPIRE`の定義に使っているシンボルを受け取ったときの処理だ。
例えば、`VAMPIRE`は関数`CAR`を使って定義されているが、
単純に`CAR`の定義を`VAMPIRE`で置き換えてしまうと、
`VAMPIRE`は無限に再帰を繰り返し動作しなくなってしまう。
そこで、`VAMPIRE`は`GENSYM`で新しいシンボルを作り、
元々のシンボル`CAR`の定義を新しいシンボルにコピーしてから、
`VAMPIRE`で使われている`CAR`をすべて新しいシンボルに置き換える。
その後に`CAR`の定義を`VAMPIRE`で置き換える。
こうすることで、`VAMPIRE`は元々の`CAR`を呼び出すことができ、
なおかつ`VAMPIRE`の外で使われている`CAR`は`VAMPIRE`に置き換わる。

```lisp
> VAMPIRE(CAR)  ; CARの血を吸う
*T*
> CAR(A B C)  ; CARを呼び出すが、このCARはすでに吸血鬼関数になっている
*T*
> GET(A EXPR)  ; CARの引数として使われたAも吸血鬼関数に置き換わっている
(LABEL V (LAMBDA (G) ...))
```

LISP 1.5の柔軟さを最大限活かしたような素晴らしいプログラムだ。
ちなみに、この吸血鬼関数だが、
[Ichigo Lisp](https://pages.zick.run/ichigo/)で動作する。
"more options"からEvalquoteを選択すると、
このページに載っているプログラムがそのまま動く
（宗教上の理由でevalquote lispを使えない人はカッコの位置を微妙に変えて
適宜QUOTEを書き足してほしい）。
実のところ、Ichigo Lispを作った理由の何割かは
この吸血鬼関数を動かしたかったからだ。
当初の目標の一つを達成できてホッとした。

ちなみにbitに載っているオリジナルのプログラムでは
`GENSYM1`という1引数関数を使っているが、
LISP 1.5には`GENSYM1`は存在しないので`GENSYM`を代わりに使っている。

*2022-04-24*
