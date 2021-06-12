# めっちゃ式が再帰的なプログラム

[前回](/2021/recursion3.html)のあらすじ: 関数名を消し去るプログラムを書けた。

再帰的な関数を書けば問題を解決できるが、どうしても再帰呼び出しをしたくないし、
ついでにループもしたくない。そんな気分の日もあるだろう。
そんなとき、前回定義した`rec-compile/defuns`を使うと、
再帰的な関数を再帰的な式に変換してくれる。

```lisp
(rec-compile/defuns
 '(fact-tail 10 1)
 '((defun fact-tail (n acc)
  (if (= n 0)
      acc
      (fact-tail (- n 1) (* n acc))))))
; =>
; (LET ((N 10) (ACC 1))
;   #1=(IF (= N 0)
;          ACC
;          (LET ((N (- N 1)) (ACC (* N ACC)))
;            #1#)))
```

見た目上は関数が完全に消えているが、
`let`が`lambda`に書き換えられることを考慮すると関数を使っているともいえる。

```lisp
;;; 大体等価な式
((LAMBDA (N ACC)
   #1=(IF (= N 0)
          ACC
          ((LAMBDA (N ACC)
             #1#)
           (- N 1) (* N ACC))))
 10 1)
```

そのため関数**名**を消すという回りくどい表現を使ってきたが、
せっかくなので関数を完全に消し去りたい。
[前々回](/2021/recursion2.html)、次のような考察をした:

> これまで`#1=(... #1# ...)`という形の式が何度も現れたが、
> これらはすべて自由変数を含んでいた。
> （中略）
> 自由変数を含んでいなければ（副作用がない限り）常に同じ結果になるため、
> 再帰的なプログラムを書くためには自由変数は必須だろう。
> 自由変数を束縛するためには（字面上はともかく本質的には）
> 関数が必須であるため、関数から逃れることはできない。

これは正しいのだが、再帰的に何度も`let`や`lambda`が現れる必要はない。
一番外側に一度だけ`let`が現れればそれで変数を束縛できる。
再束縛する代わりに`psetq`で値を書き換えてやれば良い
（`setq`ではなく`psetq`を使う理由は演習問題）。

```lisp
(LET ((N 10) (ACC NIL))
  #1=(IF (= N 0)
         ACC
         (PROGN
          (PSETQ N (- N 1)
                 ACC (* N ACC))
          #1#)))
```

ほぼ関数を消し去ったといっても過言ではないだろう。
これは再帰的なプログラムというよりは反復的（ループ的）なプログラムだ。
そこから分かる通り、末尾再帰でない関数にはこの戦略は適用できない。
末尾再帰でない再帰は相変わらず`let`を使う必要がある。
スタックを使えば`let`も消せるがそこにロマンはないのでやらないことにする。

そんなわけで、末尾再帰のときのみ`progn`と`psetq`を使うように
`rec-compile/defuns`を書き換えた。
上のプログラムも`rec-compile/defuns`の出力なのだが、他の例も載せておこう。

```lisp
(rec-compile/defuns
 '(ack 2 1)
 '((defun ack (m n)
     (if (= m 0)
         (+ n 1)
         (if (= n 0)
             (ack (- m 1) 1)
             (ack (- m 1) (ack m (- n 1))))))))
; =>
; (LET ((M 2) (N 1))
;   #1=(IF (= M 0)
;          (+ N 1)
;          (IF (= N 0)
;              (PROGN
;               (PSETQ M (- M 1)
;                      N 1)
;               #1#)
;              (PROGN
;               (PSETQ M (- M 1)
;                      N
;                        (LET ((M M) (N (- N 1)))
;                          #1#))
;               #1#))))
```

これはわりと面白い例かと思う。
末尾再帰をしている2箇所は`psetq`を使っているが、
末尾でない再帰呼び出しは`let`を使っている。
関数を消し去るという野望は潰えたが、面白いので良しとしよう。

「末尾再帰」という言葉を使っているが、再帰である必要はない。
末尾位置で変数が束縛されていれば他の関数呼び出しでもかまわない。

```lisp
(rec-compile/defuns
 '(even? 6)
 '((defun even? (n)
     (if (= n 0)
         t
         (odd? (- n 1))))
   (defun odd? (n)
     (if (= n 0)
         nil
         (even? (- n 1))))))
; =>
; (LET ((N 6))
;   #1=(IF (= N 0)
;          T
;          (PROGN
;           (PSETQ N (- N 1))
;           (IF (= N 0)
;               NIL
;               (PROGN (PSETQ N (- N 1)) #1#)))))
```

まるでSchemeだ。
これは`even?`と`odd?`の両方で変数`n`を使っているからこそなせる技で、
変数の名前を変えると話が変わってくる。

```lisp
(rec-compile/defuns
 '(even? 6)
 '((defun even? (n)
     (if (= n 0)
         t
         (odd? (- n 1))))
   (defun odd? (m)
     (if (= m 0)
         nil
         (even? (- m 1))))))
; =>
; (LET ((N 6))
;   #1=(IF (= N 0)
;          T
;          (LET ((M (- N 1)) N)
;            (IF (= M 0)
;                NIL
;                (PROGN (PSETQ N (- M 1)) #1#)))))
```

`odd?`の引数を`m`に変更すると、
`odd?`の呼び出しで`let`を使うようになってしまった
（`let`に奇妙な`N`が現れる理由はこのあとすぐ）。
しかしながら、`even?`の呼び出しは再帰ではないにもかかわらず、
きちんと`psetq`を使っている。`n`はすでに束縛済みだからだ。
こういった無駄（？）を避けるには、引数の名前を揃えるか、
最初にすべての関数で現れる引数を束縛しれやれば良い。
引数の名前を自動で書き換えてもいいが、その方法はロマンがないので採用しない。

ここまで見た限りでは、末尾位置であれば`psetq`を使い、
そうでなければ`let`を使うという方針で実装できそうだが、
複数の関数が出てきて、末尾呼出と非末尾呼出が混ざると話が難しくなる。

```lisp
(defun f (x)
  (g (+ x 1)))  ; tail call
(defun g (y)
  (if (< y 10)
      (+ (f y)  ; non-tail call
         y)
      y))
(defun h (x y)
  (f (+ x y)))  ; tail call

(h 4 4)
```

これらの関数自身には特に意味はないのだが、動作を追ってみよう。
まず、`h`が`x`と`y`を束縛する。
そのため、`f`における`g`の呼び出しは`psetq`を使うことになる。
`g`における`f`の呼び出しは、非末尾位置なので`let`を使うことになる。
あまり考えずに変換すると次のようになる。

```lisp
(LET ((X 4) (Y 4))
  (PROGN
   (PSETQ X (+ X Y))
   #1=(PROGN
       (PSETQ Y (+ X 1))  ; Yを上書き
       (IF (< Y 10)
           (+
            (LET ((X Y))  ; Yは束縛しない
              #1#)        ; ここでYが上書きされる
            Y)            ; このYの値は？
           Y))))
```

だが、これは正しくない。`#1#`が`y`を書き換えたあとで、
`+`の引数として`y`を参照するためだ。
動作を正しくするためには`let`で`y`も束縛する必要がある。
値は重要でないため、`NIL`を入れておけば良い。

```lisp
(LET ((X 4) (Y 4))
  (PROGN
   (PSETQ X (+ X Y))
   #1=(PROGN
       (PSETQ Y (+ X 1))  ; Yを上書き
       (IF (< Y 10)
           (+
            (LET ((X Y) Y)  ; Yを束縛する
              #1#)          ; 上書きされるのはLETローカルのY
            Y)              ; このYは上書きされない
           Y))))
```

このように、非末尾呼出の際には変数を保護してやる必要がある。
どの変数を保護すべきか真面目に考えるのは割と面倒だ。
一番確実で簡単なのは、すべてのローカル変数、
つまり引数を保護するという方針だろう。

そんなわけで、こういった面倒事をやってくれる
`rec-compile/defuns`の定義は以下の通りだ。

```lisp
(defvar *undef* (gensym "undef"))
(defvar *defining* (gensym "defining"))

;; def = (name args exp pointer)
(defun make-def (defun)
  (list (cadr defun) (remove '&optional (caddr defun))
        (cadddr defun) (list *undef*)))
;; defs = ((name args exp pointer) ...)
(defun make-defs (defuns)
  (if (null defuns)
      nil
      (cons (make-def (car defuns))
            (make-defs (cdr defuns)))))

(defun lookup-def (key defs) (assoc key defs))
(defun def-name (def) (car def))
(defun def-args (def) (cadr def))
(defun def-exp (def) (caddr def))
(defun def-pointer (def) (cadddr def))
(defun set-def-pointer (def val)
  (caddr (list (rplaca (cadddr def) (car val))
               (rplacd (cadddr def) (cdr val))
               (def-pointer def))))

(defun let-binds (params args vars)
  (if (null params)
      vars
      (cons (list (car params) (car args))
            (let-binds (cdr params) (cdr args) vars))))

(defun setq-params (params args)
  (if (null params)
      nil
      (if (eq (car params) (car args))
          (setq-params (cdr params) (cdr args))
          (cons (car params)
                (cons (car args)
                      (setq-params (cdr params) (cdr args)))))))

(defun let-call (params args pointer vars cvars tailp)
  (if (and tailp (subsetp params vars))
      (if (setq-params params args)
          (list 'progn (cons 'psetq (setq-params params args)) pointer)
          pointer)
      (list 'let (let-binds params args (set-difference cvars params))
            pointer)))

(defun rec-list-compile (lst defs vars cvars tailp)
  (if (null lst)
      nil
      (cons (rec-compile (car lst) defs vars cvars nil)
            (rec-list-compile (cdr lst) defs vars cvars tailp))))

(defun rec-user-call-compile (exp def defs vars cvars tailp)
  (let-call
   (def-args def)
   (rec-list-compile (cdr exp) defs vars cvars nil)
   (if (eq (car (def-pointer def)) *undef*)
       (cadr
        (list
         (set-def-pointer def (list *defining*))
         (set-def-pointer
          def
          (rec-compile (def-exp def)
                       defs
                        (union (def-args def) vars)
                       (def-args def)
                       t))))  ; reset tailp
       (def-pointer def))
   vars
   cvars
   tailp))

(defun rec-call-compile (exp def defs vars cvars tailp)
  (if def
      (rec-user-call-compile exp def defs vars cvars tailp)
      (cons (car exp) (rec-list-compile (cdr exp) defs vars cvars nil))))

(defun expand-cond (clauses)
  (if (null clauses)
      nil
      (if (eq (caar clauses) t)
          (cadar clauses)
          (list 'if
                (caar clauses)
                (cadar clauses)
                (expand-cond (cdr clauses))))))

(defun rec-compile (exp defs vars cvars tailp)
  (cond ((atom exp) exp)
        ((eq (car exp) 'quote) exp)
        ((eq (car exp) 'if)
         (list 'if
               (rec-compile (cadr exp) defs vars cvars nil)
               (rec-compile (caddr exp) defs vars cvars tailp)
               (rec-compile (cadddr exp) defs vars cvars tailp)))
        ((eq (car exp) 'cond)
         (rec-compile (expand-cond (cdr exp)) defs vars cvars tailp))
        (t (rec-call-compile exp (lookup-def (car exp) defs) defs
                             vars cvars tailp))))

(defun rec-compile/defuns (exp defuns)
  (rec-compile exp (make-defs defuns) '() '() t))
```

束縛済みの変数を`vars`で、現在の関数の引数を`cvar`で、
末尾位置かどうかを`tailp`で引き回している点を除くと、
前回とそれほど変わらない。
`tailp`と`vars`次第で`let`の代わりに`psetq`を生成するだけだ。
本当は`and`を`if`に展開すべきなのだが面倒なので読者の演習問題とする。

さて、せっかくなので手で書き換えるには大きすぎるプログラムを変換したいが、
都合の良いプログラムはないものか。実はある。`rec-compile/defuns`自身だ。
ただし、`even?`/`odd?`で述べたとおり、変数の出現の順番の影響を受けるので、
あらかじめすべての変数を束縛する戦略を取る。具体的にはwrapper関数を作る。

```lisp
(defun wrapper (exp defuns &optional defs def vars cvars tailp clauses params
                             args pointer key val)
  (rec-compile/defuns exp defuns))
```

準備が整ったので、`rec-compile/defuns`に`rec-compile/defuns`自身を与えて、
`wrapper`を呼び出すと、次のような結果が得られる。

```lisp
(LET ((EXP '(FACT-TAIL 10 1))
      (DEFUNS
       '((DEFUN FACT-TAIL (N ACC)
           (IF (= N 0)
               ACC
               (FACT-TAIL (- N 1) (* N ACC))))))
      (DEFS NIL)
      (DEF NIL)
      (VARS NIL)
      (CVARS NIL)
      (TAILP NIL)
      (CLAUSES NIL)
      (PARAMS NIL)
      (ARGS NIL)
      (POINTER NIL)
      (KEY NIL)
      (VAL NIL))
  (PROGN
   (PSETQ DEFS
            (LET ((DEFUNS DEFUNS) EXP)
              #1=(IF (NULL DEFUNS)
                     NIL
                     (CONS
                      (LET ((DEFUN (CAR DEFUNS)) DEFUNS)
                        (LIST (CADR DEFUN) (REMOVE '&OPTIONAL (CADDR DEFUN))
                              (CADDDR DEFUN) (LIST *UNDEF*)))
                      (LET ((DEFUNS (CDR DEFUNS)))
                        #1#))))
          VARS 'NIL
          CVARS 'NIL
          TAILP T)
   #2=(IF (ATOM EXP)
          EXP
          (IF (EQ (CAR EXP) 'QUOTE)
              EXP
              (IF (EQ (CAR EXP) 'IF)
                  (LIST 'IF
                        (LET ((EXP (CADR EXP))
                              (DEFS DEFS)
                              (VARS VARS)
                              (CVARS CVARS)
                              (TAILP NIL))
                          #2#)
                        (LET ((EXP (CADDR EXP))
                              (DEFS DEFS)
                              (VARS VARS)
                              (CVARS CVARS)
                              (TAILP TAILP))
                          #2#)
                        (LET ((EXP (CADDDR EXP))
                              (DEFS DEFS)
                              (VARS VARS)
                              (CVARS CVARS)
                              (TAILP TAILP))
                          #2#))
                  (IF (EQ (CAR EXP) 'COND)
                      (PROGN
                       (PSETQ EXP
                                (LET ((CLAUSES (CDR EXP))
                                      TAILP
                                      CVARS
                                      VARS
                                      DEFS
                                      EXP)
                                  #3=(IF (NULL CLAUSES)
                                         NIL
                                         (IF (EQ (CAAR CLAUSES) T)
                                             (CADAR CLAUSES)
                                             (LIST 'IF (CAAR CLAUSES)
                                                   (CADAR CLAUSES)
                                                   (LET ((CLAUSES
                                                          (CDR CLAUSES)))
                                                     #3#))))))
                       #2#)
                      (PROGN
                       (PSETQ DEF
                                (LET ((KEY (CAR EXP))
                                      (DEFS DEFS)
                                      TAILP
                                      CVARS
                                      VARS
                                      EXP)
                                  (ASSOC KEY DEFS)))
                       (IF DEF
                           (PROGN
                            (PSETQ PARAMS
                                     (LET ((DEF DEF) TAILP CVARS VARS DEFS EXP)
                                       #4=(CADR DEF))
                                   ARGS
                                     (LET ((LST (CDR EXP))
                                           (DEFS DEFS)
                                           (VARS VARS)
                                           (CVARS CVARS)
                                           (TAILP NIL)
                                           DEF
                                           EXP)
                                       #5=(IF (NULL LST)
                                              NIL
                                              (CONS
                                               (LET ((EXP (CAR LST))
                                                     (DEFS DEFS)
                                                     (VARS VARS)
                                                     (CVARS CVARS)
                                                     (TAILP NIL)
                                                     LST)
                                                 #2#)
                                               (LET ((LST (CDR LST))
                                                     (DEFS DEFS)
                                                     (VARS VARS)
                                                     (CVARS CVARS)
                                                     (TAILP TAILP))
                                                 #5#))))
                                   POINTER
                                     (IF (EQ
                                          (CAR
                                           (LET ((DEF DEF)
                                                 TAILP
                                                 CVARS
                                                 VARS
                                                 DEFS
                                                 EXP)
                                             #6=(CADDDR DEF)))
                                          *UNDEF*)
                                         (CADR
                                          (LIST
                                           (LET ((DEF DEF)
                                                 (VAL (LIST *DEFINING*))
                                                 TAILP
                                                 CVARS
                                                 VARS
                                                 DEFS
                                                 EXP)
                                             #7=(CADDR
                                                 (LIST
                                                  (RPLACA (CADDDR DEF)
                                                          (CAR VAL))
                                                  (RPLACD (CADDDR DEF)
                                                          (CDR VAL))
                                                  (LET ((DEF DEF) VAL)
                                                    #6#))))
                                           (LET ((DEF DEF)
                                                 (VAL
                                                  (LET ((EXP
                                                         (LET ((DEF DEF)
                                                               TAILP
                                                               CVARS
                                                               VARS
                                                               DEFS
                                                               EXP)
                                                           (CADDR DEF)))
                                                        (DEFS DEFS)
                                                        (VARS
                                                         (UNION
                                                          (LET ((DEF DEF)
                                                                TAILP
                                                                CVARS
                                                                VARS
                                                                DEFS
                                                                EXP)
                                                            #4#)
                                                          VARS))
                                                        (CVARS
                                                         (LET ((DEF DEF)
                                                               TAILP
                                                               CVARS
                                                               VARS
                                                               DEFS
                                                               EXP)
                                                           #4#))
                                                        (TAILP T)
                                                        DEF)
                                                    #2#))
                                                 TAILP
                                                 CVARS
                                                 VARS
                                                 DEFS
                                                 EXP)
                                             #7#)))
                                         (LET ((DEF DEF)
                                               TAILP
                                               CVARS
                                               VARS
                                               DEFS
                                               EXP)
                                           #6#)))
                            (IF (AND TAILP (SUBSETP PARAMS VARS))
                                (IF (LET ((PARAMS PARAMS)
                                          (ARGS ARGS)
                                          TAILP
                                          CVARS
                                          VARS
                                          POINTER)
                                      #8=(IF (NULL PARAMS)
                                             NIL
                                             (IF (EQ (CAR PARAMS) (CAR ARGS))
                                                 (PROGN
                                                  (PSETQ PARAMS (CDR PARAMS)
                                                         ARGS (CDR ARGS))
                                                  #8#)
                                                 (CONS (CAR PARAMS)
                                                       (CONS (CAR ARGS)
                                                             (LET ((PARAMS
                                                                    (CDR
                                                                     PARAMS))
                                                                   (ARGS
                                                                    (CDR ARGS)))
                                                               #8#))))))
                                    (LIST 'PROGN
                                          (CONS 'PSETQ
                                                (LET ((PARAMS PARAMS)
                                                      (ARGS ARGS)
                                                      TAILP
                                                      CVARS
                                                      VARS
                                                      POINTER)
                                                  #8#))
                                          POINTER)
                                    POINTER)
                                (LIST 'LET
                                      (LET ((PARAMS PARAMS)
                                            (ARGS ARGS)
                                            (VARS
                                             (SET-DIFFERENCE CVARS PARAMS))
                                            TAILP
                                            CVARS
                                            POINTER)
                                        #9=(IF (NULL PARAMS)
                                               VARS
                                               (CONS
                                                (LIST (CAR PARAMS) (CAR ARGS))
                                                (LET ((PARAMS (CDR PARAMS))
                                                      (ARGS (CDR ARGS))
                                                      (VARS VARS))
                                                  #9#))))
                                      POINTER)))
                           (CONS (CAR EXP)
                                 (LET ((LST (CDR EXP))
                                       (DEFS DEFS)
                                       (VARS VARS)
                                       (CVARS CVARS)
                                       (TAILP NIL)
                                       DEF
                                       EXP)
                                   #5#))))))))))
```

長い。長過ぎる。長い上に読みづらい。
しかし、せっかくなので見どころを何点か紹介する。

まずは`cond`の展開。`cond`を`if`に変換するコード自体は、
非末尾再帰なので`let`を使ったコードになっているが、
`if`に変換したあとに、`rec-compile`の末尾再帰を行う箇所は、
`exp`のみを上書きしてループをしている。まさに末尾再帰だ。

次は`rec-compile`における`rec-call-compile`の呼び出し。
ここで`def`以外の引数は完全に名前が一致しているため、
`def`のみ`psetq`で値を入れて、あとは単純にコードが続いている。
`rec-call-compile`における`rec-user-call-compile`の呼び出しは、
すべての引数の名前が一致しているため、`psetq`も`progn`も現れず、
その場でコードをインライン展開している。

インパクトがあるのは`let-call`の第3引数`pointer`をつくるコード。
`psetq`で`pointer`を上書きするだけなのだが、このコードが長い。
とにかく長い。何か特別なことをやっているわけではないのだが単純に長い。
`let`が何度も現れ読みづらく、読めば読むほど味が出る。

ちなみに、この出力をCLISPに与えると、きちんと動作する。
具体的には以下の値が得られる。

```lisp
(LET ((N 10) (ACC 1))
 #1=(IF (= N 0) ACC (PROGN (PSETQ N (- N 1) ACC (* N ACC)) #1#)))
```

もちろん、これを再度評価すると`3628800`が得られる。

いったい何をやっているのか自分でもよくわからなくなってきているが、
とにかく循環リストは楽しいということだろう。

*2021-06-12*