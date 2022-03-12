# WebAssemblyのgotoを妥協する

[前回](/2022/wasm_goto.md)のあらすじ:
WebAssemblyでgotoを作ろうとしたらめっちゃ大変。

```
(loop $loop
  (if (i32.gt_u (local.get $next) (i32.const 0))
      (then
       (local.set $jump (local.get $next))
       (local.set $next (i32.const 0))))
  (block $block2
    (block $block1
      (br_if $block1 (i32.eq (local.get $jump) (i32.const 1)))
      (br_if $block2 (i32.eq (local.get $jump) (i32.const 2)))
      (call $code1)
      (if (call $test1)
          (then  ;; Forward super jump
           (local.set $next (i32.const 2))
           (br $loop)))
      (call $code2))  ;; end of $block1
    (call $code3))    ;; end of $block2
  (call $code4)
  (if (call $test2)
      (then  ;; Backward super jump
       (local.set $next (i32.const 1))
       (br $loop)))
  (call $code5))
```

## 何が大変なのか

見た目のインパクトで何が大変なのか霞んでしまうが、
私にとって一番大変なところは「ラベルの数だけネストが深くなる」ところだろう。
上の例ではラベルが2つしかないので`block`は2つネストしているだけだが、
もしラベルが4つある場合は

```
(block $block4
  (block $block3
    (block $block2
      (block $block1
        ...))))
```

このようになり、人間が書くのはかなり嫌だし、
機械に生成させるのもためらうレベルだし、
それを読むのはもっと嫌だ。

## ネストが深くならない方法

そこでネストが深くならない方法を考えた。
その方法とは「（直接的な）ジャンプ命令を使わずifを使う」というものだ。
まずは次のC言語風の擬似コードを見てもらいたい。

```c
 code0();
label1:
 code1();
label2
 code2();
label3:
 code3();
label4:
 code4();
```

この `codeN()` は関数呼び出しではなく文の集まりで、
gotoを含む可能性があるとする。
前回はこれをswitchを使うように書き換えた。

```c
do {
  if (...) { ... }  // Set next and jump
  switch (jump) {
   case 0:
    code0();
   case 1:
    code1();
   case 2:
    code2();
   case 3:
    code3();
   case 4:
    code4();
  }
} while (next);
```

これをWebAssemblyに翻訳しようとすると `block` が入れ子になるというのが問題だ。
そこで今回はこれをifを使って次のように書き換える。

```c
do {
  if (...) { ... }  // Set next and jump
  if (jump <= 0) {
    code0();
  }
  if (jump <= 1) {
    code1();
  }
  if (jump <= 2) {
    code2();
  }
  if (jump <= 3) {
    code3();
  }
  if (jump <= 4) {
    code4();
  }
} while (next);
```

switchによるジャンプをWebAssemblyで実現しようとするとジャンプ命令
`br` や `br_if` を使う必要があるが、
ifによる分岐はWebAssemblyの `if` に翻訳することができる。
つまり、ネストが深くなることはない。

```
(loop $loop
  (if (i32.le_s (local.get $jump) (i32.const 0))
    (code0))
  (if (i32.le_s (local.get $jump) (i32.const 1))
    (code1))
  (if (i32.le_s (local.get $jump) (i32.const 2))
    (code2))
  (if (i32.le_s (local.get $jump) (i32.const 3))
    (code3))
  (if (i32.le_s (local.get $jump) (i32.const 4))
    (code4)))
```

ネストは2段ですむようになった。gotoは `(local.set $jump N)` と `(br $loop)` の
2命令で実現できる。 `(br $loop)` を呼ばなければループを抜けるので、
むしろC言語の例より簡単かもしれない。
これで人間にも機械にもやさしいgoto（っぽい何か）を実現することができた。

## 無駄は多くないのか

（途中でジャンプしなければ）比較命令がラベルの個数だけ実行され、
無駄じゃないかという主張は一見正しいが、
では他の手法に無駄がないのかというと、そんなことはない。
前回のswitch手法でも比較命令が何度も実行される。

```
(br_if $block1 (i32.eq (local.get $jump) (i32.const 1)))
(br_if $block2 (i32.eq (local.get $jump) (i32.const 2)))
(br_if $block3 (i32.eq (local.get $jump) (i32.const 3)))
(br_if $block4 (i32.eq (local.get $jump) (i32.const 4)))
```

`br` や `br_if` のジャンプ先には定数（ブロック）しか指定できないので、
ジャンプの飛び先がN個ある場合、 `br` 命令はN個必要になる。
各ラベルへジャンプする確率が均等だとすると、
比較命令は平均でN/2回実行されてしまう。
大幅な苦労をして比較命令の実行回数が半分になるだけでは
割りに合わないと私は感じてしまう。

## もっと効率よくできないのか

もっと頭の良い（もしくは悪い）方法がないわけではない。
飛び先を求めるために二分探索を行えば良い。

```
(if (i32.le_s (local.get $jump) (i32.const 2))
  (if (i32.le_s (local.get $jump) (i32.const 1))
    (br $block1)
    (br $block2))
  (if (i32.le_s (local.get $jump) (i32.const 3))
    (br $block3)
    (br $block4)))
```

仮にラベルが8個ある場合比較命令の実行回数は、
ifを使う手法では8回、
switchを使う手法で工夫をしなければ平均4回、
switchを使う手法で二分探索をすれば3回となる。

この程度の差ではあまりうれしくないが、仮にラベルが1024個ある場合、
ifを使う手法では1024回、
switchを使う手法で工夫をしなければ平均512回、
switchを使う手法で二分探索をすれば10回比較命令が実行される。

ここまで差がつくとなにか変わってくるかもしれない。
もっとも、ラベルの数がそこまで多くなることはあまりないだろうし、
WebAssemblyが1024段の `block` のネストを許すのかは分からないし、
そもそも本当に速くなるかは計測をしないとわからない。
気になる方がいたらぜひ計測をして結果をまとめてほしい。

*2022-03-12*
