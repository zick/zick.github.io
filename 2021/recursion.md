# 式が再帰的なプログラム

## 関数編

Common Lispには関数を定義する方法が複数あるが、そのなかに`flet`がある。

```lisp
;;; ローカル関数midnight-plus-oneを定義して呼び出す
(flet ((midnight-plus-one (n)
         (+ n 1)))
  (midnight-plus-one 1))  ; => 2

;;; fletの外側からはmidnight-plus-oneは見えない
(midnight-plus-one 1)  ; MIDNIGHT-PLUS-ONE is undefined

;;; fletで定義する関数自身からは自分の名前は見えない
(flet ((sum (n acc)
         (if (= n 0)
             acc
             (sum (- n 1) (+ n acc)))))  ; SUM is undefined
  (sum 10 0))
```

今見てほしいのは最後の例で、`flet`では再帰的な関数を定義できない。
再帰的なローカル関数を定義したいときは（常識的には）`labels`を使うのだが、
どうにかして`flet`で再帰的な関数を定義できないだろうか。

頭の悪い解決策として「式を再帰的にする」という方法がある。
基本的なアイディアは以下の通り。

```lisp
#1=(flet ((sum (n acc)
            ...
            #1#
            ...))
     ...)
```

`flet`から始まる式全体に`#1`というラベルをつけて、再帰呼び出しをする代わりに
`#1#`でラベルを参照する。こうして`flet`の中に無限に`flet`が現れるようになり、
実質的に再帰呼び出しができるようになる。自分自身を呼び出しているわけではなく、
（偶然同じ名前の）別の関数を呼び出しているのだから、再帰呼び出しではない
というツッコミを入れる人がいるかも知れないが、そういった些末な問題は
ここでは気にせず、肉付けをしていく。

```lisp
#1=(flet ((sum (n acc)
            (if (= n 0)
                acc
                #1#)))
     ...)
```

まだ`flet`のbodyが`...`のままだが、ここは意外と難しい。
ここに`(sum 10 0)`などと書いてしまうと、再帰的に`(sum 10 0)`が呼ばれ続ける。
それでは`(sum (- n 1) (+ n acc))`と書けばいいのかというと、
これでは最初の呼び出しで失敗してしまう。
`flet`の外側に`n`と`acc`を定義することでこの問題を解決できる。

```lisp
(let* ((n (+ 10 1)) (acc (- n)))
  #1=(flet ((sum (n acc)
              (if (= n 0)
                  acc
                  #1#)))
       (sum (- n 1) (+ n acc))))
```

`n`と`acc`の初期値が複雑だが、これは`flet`の最初の呼び出しで
それぞれ10と0にするための調整だ。

これで動けば話は簡単なのだが残念ながらこのままでは動かない。
`if`を実際に評価するときまで一切中身を見ないような単純な実装であれば
これでも動くと思うが、現実の処理系は評価前にcode walkをしてしまい、
code walkが止まらなくなってしまうようだ。
少なくてもSBCLとCLISPではstack overflowしてしまった。

code walkを避ける方法は1つ。クォートするしかない。
もちろんクォートしたままでは値が得られないので`eval`する必要がある。

```lisp
#1='(flet ((sum (n acc)
            (if (= n 0)
                acc
                (eval #1#))))
     ...)
```

`eval`からはローカル変数（つまり`n`と`acc`）が見えないという問題があるので、
`eval`の引数の中で変数を作り上げる必要がある。

```lisp
(defun let-eval (binds exp)
  (eval `(let ,binds ,exp)))

#1='(flet ((sum (n acc)
            (if (= n 0)
                acc
                (let-eval `((n ,(- n 1)) (acc ,(+ n acc))) #1#))))
     ...)
```

このままではトップレベルの式がクォートされているので何も評価されない。
式全体を`let-eval`で囲ってやる必要がある。
最後に残った`...`は簡単。`(sum n acc)`を呼ぶだけだ。

```lisp
(let-eval
 '((n 10) (acc 0))
 #1='(flet ((sum (n acc)
             (if (= n 0)
                 acc
                 (let-eval `((n ,(- n 1)) (acc ,(+ n acc))) #1#))))
      (sum n acc)))  ; => 55
```

これでやっと完成。無事`flet`で再帰（のような何か）ができた。
やっていることの頭の悪さのわりに意外ときれいな式だと思う。

## GOTO編

再帰的な式を使ってループを実現するのに関数を作る必要はない。
次のGOTO (Common Lispの`go`) を使ったプログラムを見てほしい。

```lisp
(prog (n acc)
   (setq n 10 acc 0)
   loop1
   (cond ((> n 0)
          (setq acc (+ n acc) n (1- n))
          (go loop1))
         (t (return acc))))  ; => 55
```

こんなプログラムが書けるのがCommon Lispの良いところという話は置いといて、
ラベル`loop1`を`#1=`に、`(go loop1)`を`#1#`に書き換えればいい。

```lisp
(let ((n 10) (acc 0))
  #1=(cond ((> n 0)
            (setq acc (+ n acc) n (1- n))
            #1#)
           (t acc)))  ; => 55?
```

`prog`は好きじゃないという人のために`let`を使うようにした。
このプログラム、CLISPでは動くがSBCLでは
code walkの問題でstack overflowしてしまう。
SBCLで動かす方法は簡単で、`let-eval`を使えば良い。
これは（本当に）簡単なので読者の演習問題とする。

## おわりに

実はこの話は、[Ring Lisp](https://github.com/zick/RingLisp)で再帰関数を定義する
方法として考え、そしてボツとなったアイディアだ。
「時間経過で関数定義が消えてしまうなら関数の中でdefunを呼べばどうだろう」
と思いつき

```lisp
#1=(defun rec ()
  #1#
  BODY)
```

こんな感じの式を思いついた。残念ながら「関数定義が消える」というのは、
束縛が消えるだけではなく、関数を構成するコンスセル自体が消えるので、
この方法ではうまく動かない。
[答](https://docs.google.com/presentation/d/1QCGnJk5FeI-1kdPB_g6xDd_DcqQxqGKOR8AbtKcXb8U/edit?usp=sharing#slide=id.g83a498bdde_1_329)
を知ってしまえば簡単なやり方も、
思いつくまでにはかなりの数の（動かない）アイディがボツになったので、
それをここで供養したい。

*2021-05-23*
