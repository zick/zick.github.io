# 「コンパイラ: 原理と構造」の型推論をClojureで書いた

[コンパイラ: 原理と構造](https://www.amazon.co.jp/gp/product/4320124782/)
という本を読んだ。最近は本の内容を3秒ほどで忘れてしまうので、
少しでも記憶を定着させるため手を動かすことにした。
大昔に決定性有限オートマトンを作ったり、
LALR構文解析器を作ったりするプログラムを（Common Lispのマクロとして）
書いたことがあるので字句解析と構文解析はすっ飛ばして、
いきなり型推論をするプログラムを書くことにした。型推論を書くのは初めてだ。
本ではSML#を使っているが、「最も稼げる言語はClojure」という噂を聞いたので
Clojureで書いてみることにした。Clojureを書くのも初めてだ。

## できあがったもの

```clojure
user> (type-inf-defs
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
         ))
fdec : int * 'a -> int * 'a
first : 'b * 'a -> 'b
second : 'b * ('a * 'c) -> 'a
third : 'b * ('a * 'c) -> 'c
rec : int * (('a -> 'a) * 'a) -> 'a
ntimes : int -> ('a -> 'a) -> 'a -> 'a
plus-ten : int -> int
neg : bool -> bool
neg99 : bool -> bool
poly-test : int * bool
```

それっぽく動いている。
多相型を扱えるので、`ntimes`を異なる型で呼び出してもちゃんと型が付く。

## 苦労したところ

基本的には本に書いてあるアルゴリズムをそのまま打ち込むだけなので、
Clojureの書き方がわからないこと以外に大きな問題はなかった。
問題は（紙面の都合で？）省略されているところ。
具体的には組み込み関数（四則演算）と再帰関数。

四則演算はすんなりできた。

```
W(Γ, prim(p, e1, e2)) =
  let (S1, t1) = W(Γ, e1)
      (S2, t2) = W(S1(Γ) e2)
      S3 = U({(t2, int), (S2(t1), int)})
  in (S3 S2 S1, int)
```

こんな感じにしたら、少なくても試した範囲では動いたのでヨシとする。

問題は再帰関数。最初は型付け規則を参考に次のように書いた。

```
W(Γ, fix f x => e) =
  let (S, t) = W(Γ{x : t1, f : t1 -> t2}, e)  (t1, t2: fresh)
  in (S, t1 -> S(t))
```

これで簡単なケースは動いたが、`ntiems`のような複雑なものが動かなかった。
具体的には `int -> ('a -> 'a) -> 'a -> 'a` となってほしいのに
`int -> ('b -> 'a) -> 'a -> 'a` となってしまった。
どうやら関数の戻り値として仮においた型変数（`t2`）がそのまま残ったらしい。
試行錯誤した結果、型代入`S`に `{t2 : t}` を追加したらうまく行った。

```
W(Γ, fix f x => e) =
  let (S, t) = W(Γ{x : t1, f : t1 -> t2}, e)  (t1, t2: fresh)
      S2 = S{t2 : t}
  in (S2, t1 -> S2(t))
```

とりあえずこれで`ntimes`はそれっぽく動いたが、これで話は終わりではない。
上記`ntimes`はタプルを使った複雑な定義をしているが、
これを関数を使った自然な定義に書き換えたら動かなくなる。

```clojure
user> (type-inf-defs
       '((def ntimes (fix rec x (fn f (fn i (if (eq x 0)
                                              i
                                              (f (((rec (- n 1)) f) i)))))))
         ))
ntimes : int -> ('b -> 'a) -> 'a -> 'a
```

型代入の中身を覗いてみたところ、再帰関数の戻り値のための型変数と
入れ子関数の引数のための型変数が結びついてしまっているため、
あとで型代入をいじってもうまくいかないようだ。
どこかで単一化をしないといけないような気がするが、
しばらく考えても分からなかったので諦めた。

(2021-10-16 削除)

~~このように頭の悪い読者もいるので、
大事な定義は省略せずに全部書いてもらいたい。~~

(2021-10-16 追加)

なんと著者の大堀先生直々に答を教えてもらった（
[tweet1](https://twitter.com/AtsushiOhori/status/1449189916030636032)
[tweet2](https://twitter.com/AtsushiOhori/status/1449192877645451265)
[tweet3](https://twitter.com/AtsushiOhori/status/1449196046974742530)）。

```
W(Γ, fix f x => e) =
  let (S, t) = W(Γ{x : t1, f : t1 -> t2}, e)  (t1, t2: fresh)
      S2 = U({t, S(t2)})
  in (S2 S, S2 S(t1 -> t))
```

実は `S2 = U({t, S(t2)})` はすでに試していたのだけれど、
`S2 S(t1 -> t)` とすべきところを `S2 S(t1) -> t` としていたため、
単一化の結果が部分式に反映されないという、バカバカしいミスをしていた。
これで無事動いた。

## ソースコード

「Clojureで」と書いたのにClojureの話が一切出てこなかったので、
ソースコードをまるごと貼り付けてお茶を濁す。

```clojure
;;; TYPE ::=
;;;   int
;;;   str
;;;   bool
;;;   (tyvar SYMBOL)
;;;   (fun TYPE TYPE)
;;;   (pair TYPE TYPE)
;;;   (poly (SYMBOL*) TYPE)
;;;
;;; EXP ::=
;;;   INT
;;;   STR
;;;   BOOL
;;;   SYMBOL
;;;   (fn SYMBOL EXP)
;;;   (EXP EXP)
;;;   (PRIME EXP EXP)
;;;   (tuple EXP EXP)
;;;   (get EXP INT)
;;;   (if EXP EXP EXP)

(use '[clojure.set :only (union intersection)])

(defn free-type-var [ty]
  (if (list? ty)
    (case (first ty)
      tyvar #{(second ty)}
      fun (union (free-type-var (nth ty 1))
                 (free-type-var (nth ty 2)))
      pair (union (free-type-var (nth ty 1))
                  (free-type-var (nth ty 2))))
    #{}))

(defn occurs [tv ty]
  (and (list? tv)
       (= (first tv) 'tyvar)
       (contains? (free-type-var ty) (second tv))))

(defn tyvar? [ty]
  (and (list? ty) (= (first ty) 'tyvar)))
(defn fun? [ty]
  (and (list? ty) (= (first ty) 'fun)))
(defn pair? [ty]
  (and (list? ty) (= (first ty) 'pair)))

(defn subst-type [ss ty]
  (if (list? ty)
    (case (first ty)
      tyvar (if-let [v (get ss (second ty))]
              v
              ty)
      poly (list 'poly (nth ty 1) (subst-type ss (nth ty 2)))
      fun (list 'fun (subst-type ss (nth ty 1)) (subst-type ss (nth ty 2)))
      pair (list 'pair (subst-type ss (nth ty 1)) (subst-type ss (nth ty 2))))
    ty))

(defn subst-env [ss env]
  (reduce-kv (fn [coll k v] (assoc coll k (subst-type ss v)))
             {}
             env))

(defn compose-subst [s1 s2]
  (merge-with (fn [v1 v2] v1)
              s1
              (reduce-kv (fn [coll k v] (assoc coll k (subst-type s1 v)))
                         {}
                         s2)))

(defn union-env [env1 env2]
  (merge-with (fn [v1 v2] v1) env1 env2))

(defn rewrite [e s]
  (if (empty? e)
    s
    (let [[ty1 ty2] (first e) e2 (rest e)]
      (cond
        (= ty1 ty2) (recur e2 s)
        (tyvar? ty1) (if (occurs ty1 ty2)
                       nil
                       (let [s1 {(second ty1) ty2}]
                         (recur
                          (map (fn [[t1 t2]] (list (subst-type s1 t1)
                                                   (subst-type s1 t2)))
                               e2)
                          (compose-subst s1 s))))
        (tyvar? ty2) (recur (conj e2 (list ty2 ty1)) s)
        (and (fun? ty1) (fun? ty2)) (recur (conj
                                            e2
                                            (list (nth ty1 1) (nth ty2 1))
                                            (list (nth ty1 2) (nth ty2 2)))
                                           s)
        (and (pair? ty1) (pair? ty2)) (recur (conj
                                              e2
                                              (list (nth ty1 1) (nth ty2 1))
                                              (list (nth ty1 2) (nth ty2 2)))
                                             s)
        :else nil))))

(defn unify [e]
  (rewrite e {}))

(defn matches [env1 env2]
  (loop [keys (intersection (set (keys env1)) (set (keys env2)))
         ret nil]
    (if (empty? keys)
      ret
      (recur (rest keys)
             (conj ret (list (get env1 (first keys))
                             (get env2 (first keys))))))))

(defn new-var []
  (list 'tyvar (gensym "var")))

(defn pts [x]
  (cond
    ;; constants
    (integer? x) (list {} 'int)
    (boolean? x) (list {} 'bool)
    (string? x) (list {} 'str)
    ;; variable
    (symbol? x) (let [ty (new-var)] (list {x ty} ty))
    ;; (fn var exp)
    (= (first x) 'fn) (let [[env ty] (pts (nth x 2))
                            arg (nth x 1)]
                        (if (contains? env arg)
                          (list (dissoc env arg)
                                (list 'fun (get env arg) ty))
                          (list env (list 'fun (new-var) ty))))
    ;; (exp exp)
    (= (count x) 2) (let [[env1 ty1] (pts (nth x 0))
                          [env2 ty2] (pts (nth x 1))
                          nty (new-var)]
                      (let [s (unify (conj (matches env1 env2)
                                           (list (list 'fun ty2 nty) ty1)))]
                        (list
                         (union-env (subst-env s env1)
                                    (subst-env s env2))
                         (subst-type s nty))))
    ;; +, -, *, /, eq
    (some #{(first x)} '(+ - * / eq))
    (let [[env1 ty1] (pts (nth x 1))
          [env2 ty2] (pts (nth x 2))]
      (let [s (unify (conj (matches env1 env2)
                          (list ty1 'int)
                          (list ty2 'int)))]
        (list (union-env (subst-env s env1)
                         (subst-env s env2))
              (if (= (first x) 'eq) 'bool 'int))))
    ))

(defn poly? [ty]
  (and (list? ty) (= (first ty) 'poly)))

(defn fresh-inst [ty]
  (if (poly? ty)
    (subst-type
     (reduce (fn [s v] (assoc s v (new-var))) {} (nth ty 1))
     (nth ty 2))
    ty))

(defn w [env x]
  (cond
    ;; constants
    (integer? x) (list {} 'int)
    (boolean? x) (list {} 'bool)
    (string? x) (list {} 'str)
    ;; variable
    (symbol? x) (if-let [v (get env x)]
                  (list {} (fresh-inst v))
                  nil)
    ;; (fn var exp)
    (= (first x) 'fn) (let [nt (new-var)
                            [s t] (w (assoc env (nth x 1) nt) (nth x 2))]
                        (list s (list 'fun (subst-type s nt) t)))
    ;; (fix name var exp)
    (= (first x) 'fix)
    (let [nt1 (new-var) nt2 (new-var)
          name (nth x 1) arg (nth x 2) exp (nth x 3)
          fty (list 'fun nt1 nt2)
          [s ety] (w (assoc env arg nt1 name fty) exp)
          s2 (unify (list (list ety (subst-type s nt2))))
          s3 (compose-subst s2 s)]
      (list s3 (subst-type s3 (list 'fun nt1 ety))))
    ;; (exp exp)
    (= (count x) 2) (let [[s1 t1] (w env (nth x 0))
                          [s2 t2] (w (subst-env s1 env) (nth x 1))
                          nt (new-var)
                          s3 (unify (list (list (list 'fun t2 nt)
                                                (subst-type s2 t1))))
                          s4 (compose-subst s3 (compose-subst s2 s1))]
                      (list s4 (subst-type s4 nt)))
    ;; +, -, *, /, eq
    (some #{(first x)} '(+ - * / eq))
    (let [[s1 t1] (w env (nth x 1))
          [s2 t2] (w (subst-env s1 env) (nth x 2))
          s3 (unify (list (list t2 'int)
                          (list (subst-type s2 t1) 'int)))
          s4 (compose-subst s3 (compose-subst s2 s1))]
      (list s4 (if (= (first x) 'eq) 'bool 'int)))
    ;; (tuple exp exp)
    (= (first x) 'tuple) (let [[s1 t1] (w env (nth x 1))
                          [s2 t2] (w (subst-env s1 env) (nth x 2))
                          s3 (compose-subst s2 s1)]
                           (list s3 (list 'pair (subst-type s2 t1) t2)))
    ;; (get exp int)
    (= (first x) 'get) (let [[s1 ty] (w env (nth x 1))
                             t1 (new-var) t2 (new-var)
                             s2 (unify (list (list ty (list 'pair t1 t2))))
                             s3 (compose-subst s2 s1)]
                         (list s3 (subst-type s2 (if (= (nth x 2) 0) t1 t2))))
    ;; (if exp exp exp)
    (= (first x) 'if)
    (let [[s1 t1] (w env (nth x 1))
          [s2 t2] (w (subst-env s1 env) (nth x 2))
          [s3 t3] (w (subst-env s2 (subst-env s1 env))
                     (nth x 3))
          s4 (unify (list (list (subst-type s3 (subst-type s2 t1)) 'bool)
                          (list (subst-type s3 t2) t3)))
          s5 (compose-subst s4 (compose-subst s3 (compose-subst s2 s1)))]
      (list s5 (subst-type s4 t3)))))

(defn maybe-kakko [x cond?]
  (if cond?
    (str "(" x ")")
    x))

(defn simplify-vars [vars]
  (let [alpha '(a b c d e f g h i j k l m n o p q r s t u v w x y z)]
    (reduce (fn [env pair] (assoc env (first pair) (second pair)))
            {} (map list vars alpha))))

(defn format-type [ty & [env]]
  (cond
    ;; (fn ty ty)
    (fun? ty)
    (let [arg (maybe-kakko (format-type (nth ty 1) env) (fun? (nth ty 1)))]
      (str arg " -> " (format-type (nth ty 2) env)))
    ;; (pair ty ty)
    (pair? ty) (let [ty1 (nth ty 1) ty2 (nth ty 2)
                     t1 (maybe-kakko (format-type ty1 env)
                                     (or (pair? ty1) (fun? ty1)))
                     t2 (maybe-kakko (format-type ty2 env)
                                     (or (pair? ty2) (fun? ty2)))]
                 (str t1 " * " t2))
    ;; (tyvar x)
    (tyvar? ty) (if-let [v (get env (second ty))]
                  (str "'" v)
                  (str (second ty)))
    ;; (poly (var*) ty)
    (poly? ty) (let [env (simplify-vars (nth ty 1))]
                 (format-type (nth ty 2) env))
    :else (str ty)))

(defn type-inf [env e]
  (cond
    ;; (def var exp)
    (and (list e) (= (first e) 'def))
    (let [var (nth e 1)
          exp (nth e 2)
          [s t] (w env exp)
          vars (free-type-var t)
          ty (if (empty? vars)
               t
               (list 'poly (seq vars) t))]
      (println (str var " : " (format-type ty)))
      (list (assoc env var ty) ty))))

(defn type-inf-defs [defs]
  (loop [env {} defs defs]
    (if (empty? defs)
      env
      (recur (first (type-inf env (first defs))) (rest defs)))))
```

括弧の数が少なかったりassocが探索ではなく追加をする関数だったりするのは、
人のソースコードを読むときは気持ち悪く感じたが、
自分で書くときは案外違和感なく書けた。
破壊的代入を使わないプログラムなのでClojureとの相性も良かった気がする。

*2021-10-15*
