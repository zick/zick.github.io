# 「コンパイラ: 原理と構造」のSECD機械へのコンパイラをClojureで書いた

[前回](/2021/typeinf.md)のあらすじ:
[コンパイラ: 原理と構造](https://www.amazon.co.jp/gp/product/4320124782/)
の型推論をClojureで書いた。

型推論の次は操作的意味論を定義してインタプリタを実装する話が載っているのだが、
大昔にPrologで操作的意味論をもとに
インタプリタを作るというのを複数回やったので、
インタプリタはすっ飛ばして、コンパイラを書くことにした。

## できあがったもの

```clojure
user> (compile-and-run-defs
       '((def fdec (fn p (tuple (- (get p 0) 1) (get p 1))))
         (def first (fn p (get p 0)))
         (def second (fn p (get (get p 1) 0)))
         (def third (fn p (get (get p 1) 1)))
         (def rec (fix rec xfi (if (eq (first xfi) 0)
                                 (third xfi)
                                 ((second xfi) (rec (fdec xfi))))))
         (def ntimes (fn x (fn f (fn i (rec (tuple x (tuple f i)))))))
         (def plus-ten (fn x (((ntimes 10) (fn n (+ n 1))) x)))
         (def neg (fn x (if x false true)))
         (def neg99 (fn x (((ntimes 99) neg) x)))
         (def poly-test (tuple (plus-ten 1) (neg99 false)))
         (def fact (fix rec x (if (eq x 0) 1 (* x (rec (- x 1))))))
         (def fact10 (fact 10))
         ))
fdec = <fun> : int * 'a -> int * 'a
first = <fun> : 'a * 'b -> 'a
second = <fun> : 'c * ('a * 'b) -> 'a
third = <fun> : 'a * ('c * 'b) -> 'b
rec = <fun> : int * (('a -> 'a) * 'a) -> 'a
ntimes = <fun> : int -> ('a -> 'a) -> 'a -> 'a
plus-ten = <fun> : int -> int
neg = <fun> : bool -> bool
neg99 = <fun> : bool -> bool
poly-test = (11 true) : int * bool
fact = <fun> : int -> int
fact10 = 3628800 : int
```

前回苦労して型を推論した`ntimes`も今回新たに書いた`fact`も
きちんと動いていることが確認できる。
これだけではコンパイラなのかインタプリタなのか区別がつかないので、
コンパイル結果も載せる。

```clojure
user> (secd-compile '(fix rec x (if (eq x 0) 1 (* x (rec (- x 1))))) nil)
((mk-rec
  ((acc x)
   (push 0)
   (prim eq)
   (if
    ((push 1))
    ((acc x) (acc rec) (acc x) (push 1) (prim -) (app) (prim *)))
   (ret))
  x
  rec))
```

これはちょっと高級すぎないかという気もするが、
そういうものなのだから仕方ない。

## 苦労したところ

本に書いてある規則をそのままコードに直すだけなので非常に楽だった。
分量的にもかなり短い。

ハマったのはClojurの規則。まず `(cons 1 2)` が動かないことに驚いた。
`cons`を行儀の良いリスト専用にしたいのなら、
気持ちは分からなくもないのでとりあえず受け入れたが、

```clojure
(list? (cons 1 (cons 2 (cons 3 nil))))
;; => false
```

これは何をいっているのかまったく分からない。

```clojure
(list? '(1 2 3))
;; => true
(= '(1 2 3) (cons 1 (cons 2 (cons 3 nil))))
;; => true
```

本格的に意味がわからなくなってきた。
すこし調べたところ、両者は型が違うらしい。

```clojure
(type '(1 2 3))
;; => clojure.lang.PersistentList
(type (cons 1 (cons 2 (cons 3 nil))))
;; => clojure.lang.Cons
```

だからどうしたという気もする。Consはリストでないといういのであれば
`(cons 1 2)` を受け入れてほしい。
あと`cons?`という関数があるのかと思ったが、そんなものはなかった。

それから、mapしたリストを文字列に変換しようとしたら、

```clojure
(str (map my-fn my-list))
;; => "clojure.lang.LazySeq@f0a0"
```

こうなって、すこし悲しい気持ちになった。
`(str (seq (map my-fn my-list)))` としたら動いたけど。

## ソースコード

コンパイラのコードも
コンパイルされたコードを動かすためのSECD機械のコードも非常に短い。

個人的な一番の見所は`eval-op`という小さな関数。
同じ記号が2個ずつならんでいるが、
1つ目は評価されずシンボルとして扱われ、
2つ目は評価され関数として扱われる。
このアホな感じの見た目が好き。

```clojure
(defn eval-op [op x y]
  ((case op + + - - * * / / eq =) x y))

(defn run-secd [stack env code dump]
  (if (empty? code)
    (first stack)
    (let [[c & cs] code]
      (case (first c)
        push (recur (cons (second c) stack) env cs dump)
        acc (recur (cons (get env (second c)) stack) env cs dump)
        ;; (mk-cls c x) => (cls env c x)
        mk-cls (recur (cons (list* 'cls env (rest c)) stack) env cs dump)
        ;; (mk-rec c x f) => (rec env c x f)
        mk-rec (recur (cons (list* 'rec env (rest c)) stack) env cs dump)
        app (let [[v [t e0 c0 x & f] & stk] stack]
              (recur stk
                     (if (= t 'cls)
                       (assoc e0 x v)
                       (assoc e0 x v (first f) (second stack)))
                     c0
                     (cons (list env cs) dump)))

        ret (let [[[e0 c0] & d] dump]
              (recur stack e0 c0 d))
        pair (let [[x y & stk] stack]
               (recur (cons [y x] stk) env cs dump))
        proj1 (let [[p & stk] stack]
                (recur (cons (first p) stk) env cs dump))
        proj2 (let [[p & stk] stack]
                (recur (cons (second p) stk) env cs dump))
        prim (let [[x y & stk] stack]
               (recur (cons (eval-op (second c) y x) stk) env cs dump))
        if (let [[cnd & stk] stack]
             (recur stk env (concat (if cnd (nth c 1) (nth c 2)) cs)
                    dump))
        (assert false (str "Unknown instruction: " c))))))

(defn secd-compile [x k]
  (cond
    ;; constants
    (or (integer? x) (boolean? x) (string? x))
    (cons (list 'push x) k)
    ;; variable
    (symbol? x) (cons (list 'acc x) k)
    ;; (fn var exp)
    (= (first x) 'fn)
    (cons (list 'mk-cls (secd-compile (nth x 2) '((ret))) (nth x 1)) k)
    ;; (fix name var exp)
    (= (first x) 'fix)
    (cons (list 'mk-rec (secd-compile (nth x 3) '((ret))) (nth x 2) (nth x 1))
          k)
    ;; (exp exp)
    (= (count x) 2)
    (secd-compile (first x)
                   (secd-compile (second x)
                                  (cons '(app) k)))
    ;; +, -, *, /, eq
    (some #{(first x)} '(+ - * / eq))
    (secd-compile (nth x 1)
                   (secd-compile (nth x 2)
                                  (cons (list 'prim (first x)) k)))
    ;; (tuple exp exp)
    (= (first x) 'tuple)
    (secd-compile (nth x 1)
                   (secd-compile (nth x 2)
                                  (cons '(pair) k)))
    ;; (get exp int)
    (= (first x) 'get)
    (secd-compile (nth x 1) (cons (if (= (nth x 2) 0) '(proj1) '(proj2)) k))
    ;; (if exp exp exp)
    (= (first x) 'if)
    (secd-compile (nth x 1)
                   (cons (list 'if
                               (secd-compile (nth x 2) nil)
                               (secd-compile (nth x 3) nil)) k))))

(defn format-val [val]
  (cond
    (and (seq? val) (or (= (first val) 'cls) (= (first val) 'rec))) "<fun>"
    (vector? val) (seq (map format-val val))
    :else val))

(defn compile-and-run-defs [defs]
  (loop [tenv {} env {} defs defs]
    (if (empty? defs)
      true
      (let [def (first defs)
            [name exp] (rest def)
            [nte ty] (type-inf tenv def)
            code (secd-compile exp nil)
            ret (run-secd nil env code nil)
            ne (assoc env name ret)]
        (println (str name " = " (format-val ret) " : " (format-type ty)))
      (recur nte ne (rest defs))))))
```

*2021-10-17*
