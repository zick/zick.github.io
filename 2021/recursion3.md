# すごく式が再帰的なプログラム

[前回](/2021/recursion2.md)のあらすじ:
機械的な手順でプログラムから関数名を消すことができた。

初めて呼ぶ関数はインライン展開して、
関数の本体を `#n=` でラベル付けして、
（相互）再帰的な呼び出しは `(let (...) #n#)` に書き換える。
これだけでプログラムから関数名を消せる。例えば、


```lisp
(defun fact (n)
  (if (= n 0)
      1
      (* (fact (- n 1)) n)))

(fact 10)
```

こんなプログラムは、

```lisp
(let ((n 10))
  #1=(if (= n 0)
         1
         (* (let ((n (- n 1))) #1#)
            n)))
```

こうなる。機械的な手順が分かれば次にやることは一つ。機械にやらせる。
そんなわけで関数定義と式を受け取り、関数名を消し去るプログラムを書く。
先に完成品の使用例を載せるがこんな感じに動く。

```lisp
(rec-compile/defuns
 '(fact 10)
 '((defun fact (n)
     (if (= n 0)
         1
         (* (fact (- n 1)) n)))))
; =>
; (LET ((N 10))
;   #1=(IF (= N 0)
;          1
;          (*
;           (LET ((N (- N 1)))
;             #1#)
;           N)))
```

関数定義は複数あっても動く。

```lisp
(rec-compile/defuns
 '(list (even? 6) (odd? 6))
 '((defun even? (n)
     (if (= n 0)
         t
         (odd? (- n 1))))
   (defun odd? (n)
     (if (= n 0)
         nil
         (even? (- n 1))))))
; =>
; (LIST
;  (LET ((N 6))
;    #1=(IF (= N 0)
;           T
;           (LET ((N (- N 1)))
;             #2=(IF (= N 0)
;                    NIL
;                    (LET ((N (- N 1)))
;                      #1#)))))
;  (LET ((N 6))
;    #2#))
```

このプログラム `rec-compile/defuns` の作り方は簡単。
冒頭に載せた手順をやるだけだ。
`#n=` や `#n#` を生成することはできないので、
そこをコンスセルを作って共有するようにしてやれば完成だ。

```lisp
(defvar *undef* (gensym "undef"))
(defvar *defining* (gensym "defining"))

;; def = (name args exp pointer)
(defun make-def (defun)
  (list (cadr defun) (caddr defun) (cadddr defun) (list *undef*)))
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

(defun let-binds (params args)
  (if (null params)
      nil
      (cons (list (car params) (car args))
            (let-binds (cdr params) (cdr args)))))
(defun let-call (params args pointer)
  (list 'let (let-binds params args) pointer))

(defun rec-list-compile (lst defs)
  (if (null lst)
      nil
      (cons (rec-compile (car lst) defs)
            (rec-list-compile (cdr lst) defs))))

(defun rec-user-call-compile (exp def defs)
  (let-call
   (def-args def)
   (rec-list-compile (cdr exp) defs)
   (if (eq (car (def-pointer def)) *undef*)
       (cadr
        (list
         (set-def-pointer def (list *defining*))
         (set-def-pointer def (rec-compile (def-exp def) defs))))
       (def-pointer def))))

(defun rec-call-compile (exp def defs)
  (if def
      (rec-user-call-compile exp def defs)
      (cons (car exp) (rec-list-compile (cdr exp) defs))))

(defun expand-cond (clauses)
  (if (null clauses)
      nil
      (if (eq (caar clauses) t)
          (cadar clauses)
          (list 'if
                (caar clauses)
                (cadar clauses)
                (expand-cond (cdr clauses))))))

(defun rec-compile (exp defs)
  (cond ((atom exp) exp)
        ((eq (car exp) 'quote) exp)
        ((eq (car exp) 'if)
         (list 'if
               (rec-compile (cadr exp) defs)
               (rec-compile (caddr exp) defs)
               (rec-compile (cadddr exp) defs)))
        ((eq (car exp) 'cond)
         (rec-compile (expand-cond (cdr exp)) defs))
        (t (rec-call-compile exp (lookup-def (car exp) defs) defs))))

(defun rec-compile/defuns (exp defuns)
  (rec-compile exp (make-defs defuns)))
```

関数の本体は単一の複合式（非アトムの式）を持つことを仮定していたり、
`quote`, `if`, `cond`しかスペシャルフォームをサポートしていなかったり、
その`cond`すら各節は単一の式を持つことを仮定しているが、
とりあえず、これで簡単なプログラムは変換できる。

せっかくなので手で書き換えるには大きすぎるプログラムを変換したいが、
上記の仮定を満たす都合の良いプログラムはあるだろうか。
実はある。`rec-compile/defuns`自身だ。
高階関数を頑なに使わなかったり、
`(cadr (list X Y))` などという不可解な式が現れるのはこの仮定を満たすためだ。
そんなわけで、`rec-compile/defuns`に`rec-compile/defuns`自身を与えてみると、
次のような結果が得られる。

```lisp
(LET ((EXP '(fact 10)) (DEFUNS '((defun fact (n)
                                    (if (= n 0)
                                        1
                                        (* (fact (- n 1)) n))))))
  (LET ((EXP EXP)
        (DEFS
         (LET ((DEFUNS DEFUNS))
           #1=(IF (NULL DEFUNS)
                  NIL
                  (CONS
                   (LET ((DEFUN (CAR DEFUNS)))
                     (LIST (CADR DEFUN) (CADDR DEFUN) (CADDDR DEFUN)
                           (LIST *UNDEF*)))
                   (LET ((DEFUNS (CDR DEFUNS)))
                     #1#))))))
    #2=(IF (ATOM EXP)
           EXP
           (IF (EQ (CAR EXP) 'QUOTE)
               EXP
               (IF (EQ (CAR EXP) 'IF)
                   (LIST 'IF
                         (LET ((EXP (CADR EXP)) (DEFS DEFS))
                           #2#)
                         (LET ((EXP (CADDR EXP)) (DEFS DEFS))
                           #2#)
                         (LET ((EXP (CADDDR EXP)) (DEFS DEFS))
                           #2#))
                   (IF (EQ (CAR EXP) 'COND)
                       (LET ((EXP
                              (LET ((CLAUSES (CDR EXP)))
                                #3=(IF (NULL CLAUSES)
                                       NIL
                                       (IF (EQ (CAAR CLAUSES) T)
                                           (CADAR CLAUSES)
                                           (LIST 'IF (CAAR CLAUSES)
                                                 (CADAR CLAUSES)
                                                 (LET ((CLAUSES (CDR CLAUSES)))
                                                   #3#))))))
                             (DEFS DEFS))
                         #2#)
                       (LET ((EXP EXP)
                             (DEF
                              (LET ((KEY (CAR EXP)) (DEFS DEFS))
                                (ASSOC KEY DEFS)))
                             (DEFS DEFS))
                         (IF DEF
                             (LET ((EXP EXP) (DEF DEF) (DEFS DEFS))
                               (LET ((PARAMS
                                      (LET ((DEF DEF))
                                        (CADR DEF)))
                                     (ARGS
                                      (LET ((LST (CDR EXP)) (DEFS DEFS))
                                        #4=(IF (NULL LST)
                                               NIL
                                               (CONS
                                                (LET ((EXP (CAR LST))
                                                      (DEFS DEFS))
                                                  #2#)
                                                (LET ((LST (CDR LST))
                                                      (DEFS DEFS))
                                                  #4#)))))
                                     (POINTER
                                      (IF (EQ
                                           (CAR
                                            (LET ((DEF DEF))
                                              #5=(CADDDR DEF)))
                                           *UNDEF*)
                                          (CADR
                                           (LIST
                                            (LET ((DEF DEF)
                                                  (VAL (LIST *DEFINING*)))
                                              #6=(CADDR
                                                  (LIST
                                                   (RPLACA (CADDDR DEF)
                                                           (CAR VAL))
                                                   (RPLACD (CADDDR DEF)
                                                           (CDR VAL))
                                                   (LET ((DEF DEF))
                                                     #5#))))
                                            (LET ((DEF DEF)
                                                  (VAL
                                                   (LET ((EXP
                                                          (LET ((DEF DEF))
                                                            (CADDR DEF)))
                                                         (DEFS DEFS))
                                                     #2#)))
                                              #6#)))
                                          (LET ((DEF DEF))
                                            #5#))))
                                 (LIST 'LET
                                       (LET ((PARAMS PARAMS) (ARGS ARGS))
                                         #7=(IF (NULL PARAMS)
                                                NIL
                                                (CONS
                                                 (LIST (CAR PARAMS) (CAR ARGS))
                                                 (LET ((PARAMS (CDR PARAMS))
                                                       (ARGS (CDR ARGS)))
                                                   #7#))))
                                       POINTER)))
                             (CONS (CAR EXP)
                                   (LET ((LST (CDR EXP)) (DEFS DEFS))
                                     #4#))))))))))
```

驚くべきことに、このプログラムをCLISPに与えると、きちんと動作する。
具体的には`(LET ((N 10)) #1=(IF (= N 0) 1 (* (LET ((N (- N 1))) #1#) N)))`
が得られる。これを更に評価すると`3628800`が得られる。

この遊びにどれほど意味があるのかはさておき、
いろいろな意味で再帰的なこのプログラムが動くというのは気持ちが良い。
せっかくのリスト構造だ。遊ばないともったいない。

*2021-06-05*
