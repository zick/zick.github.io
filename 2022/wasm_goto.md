# WebAssemblyでgotoをつくる

WebAssemblyには`br`というジャンプ命令が存在するが、
これはかなり癖がある。正直使いにくい。
常識的なジャンプ命令でできることができない。
この使いにくいジャンプ命令を使ってC言語のgoto相当のことをやる方法を考える。

## WebAssemblyの分岐命令

### if
WebAssemblyには`if`という命令があり、
高級言語の条件分岐をそのまま翻訳しやすいようになっている。

```
(if (condition)
    (then
     (instruction1)
     ...
     (instructionN))
    (else
     (instruction1)
     ...
     (instructionM)))
```

このコードはまず`(condition)`を実行し、
その結果が1なら`then`の内部を、0なら`else`の内部を実行する。
これはこれで便利なのだが、好きなところにジャンプするような
低レベルのジャンプを実現するのには適していない。

### loop
ループを実現するには`loop`という命令を使う。
`loop`命令自身はラベルを定義するために使い、
実際のジャンプは `br` (無条件ジャンプ) や `br_if` (条件付きジャンプ) を使う。

```
(loop $loop1
  ...
  (br_if $loop1 condition))
```

このコードでは`loop`内部のコードが上から実行され、
もし`condition`が1になれば`br_if`によりジャンプが行われ、
`loop`の先頭に戻り、再度実行を行う。
`br`や`br_if`は末尾に置く必要はなく好きな箇所に書ける。
`br`や`br_if`がジャンプを行わなければそのまま`loop`を抜ける。

この`loop`はC言語で言えば末尾にbreakが置かれた無限ループ
 `while(1) { /* do something */; break; } ` に相当し、
`br`や`br_if`はcontinueに相当する。
`loop`を入れ子にできることを考えると
Javaなどのラベル付きcontinueと言ったほうが正確かもしれない。

### block
C言語のcontinueに相当する命令もあれば、breakに相当する命令もある。`block`だ。
WebAssemblyの`block`は`loop`とほぼ同じ見た目をしているが、
`br`や`br_if`がジャンプを行う際には先頭に戻るのではなく、`block`から抜ける。

```
(block $block1
  ...
  (br_if $block1 condition)
  ...
)
```

このコードでは`block`内部のコードが上から実行され、
もし`condition`が1になれば`br_if`によりジャンプが行われ、
`block`から抜ける。つまり`br_if`以降のコードは実行されない。

`block`を単独で使うのは、C言語でいえば `do { ... } while(0);` に相当し、
`br`や`br_if`はbreakに相当する。
`block`と`loop`を組み合わせて `(block $b (loop $l ...))` とした場合は、
`(br $l)`はcontinue、`(br $b)`はbreakに相当する。
もちろん入れ子にできるのでラベル付きbreakという方が正確だが。

ここから分かるように`br`や`br_if`といったジャンプ命令は、
指定されたラベルの種類によって動作が変化する。
正直やめてもらいたい。別の命令を使ってほしい。

## 前向きgoto

それではこれらの命令を使ってgotoを実現する方法を考える。
まずは前向きのgotoの実現方法を見ていく。
次のようなC言語のプログラムを考える。

```c
 code1();
 if (test1()) goto label1;  // Forward jump
 code2()
label1:
 code3();
```

これをWebAssemblyで書くと次のようになる。

```
(block $block1
  (call $code1)
  (br_if $block1 (call $test1))
  (call $code2))
(call $code3)
```

C言語のラベルより上の内容は`block`の中に書き、
ラベルより下の内容は`block`の外に書く。
ここは試験に出るので覚えておくように。

次はもう少し複雑な「ラベルを飛び越す」gotoを見ていく。

```c
 code1();
 if (test1()) goto label1;  // Forward jump
 if (test2()) goto label2;  // Forward super jump
 code2()
label1:
 code3();
label2:
 code4();
```

上の`goto label2`は`label1`を飛び越して`label2`にジャンプする。
C言語なら何も気にならないのだが、WebAssemblyでは工夫が必要になる。

```
(block $block2
  (block $block1
    (call $code1)
    (br_if $block1 (call $test1))
    (br_if $block2 (call $test2))
    (call $code2))  ;; end of $block1
 (call $code3))   ;; end of $block2
(code4)
```

`block`が入れ子になった。`$block2`の方が外側にあるというのがポイントだ。
中身に関しては「ラベルより上の内容は`block`の中に、
ラベルより下の内容は`block`の外に」という原則をそのまま適用できる。

ラベルが2つ出てきただけでコードが複雑になり先行きが不安になってくる。

## 後ろ向きgoto

次は後ろ向きのgotoの実現方法を見ていく。

```c
 code1();
label1:
 code2();
 if (test1()) goto label1;  // Backward jump
 code3();
```

先程のC言語のプログラムのコードの順序を少し入れ替えたものだ。
これをWebAssemblyで書くと`block`ではなく`loop`を使うことになる。

```
(call $code1)
(loop $loop1
  (call $code2)
  (br_if $loop1 (call $test1))
  (call $code3))
```

C言語のラベルより上の内容は`loop`の外（上）に書き、
ラベルより下の内容は`loop`の中に書く。
厳密には最後のgotoより下の内容は`loop`の外（下）に書くこともできるが、
ラベルより下の内容はすべて`loop`の中に書いた方が話が単純になる。

前向きgotoと同様に、次は「ラベルを飛び越す」gotoを見ていく。

```c
 code1();
label1:
 code2();
label2:
 code3();
 if (test1()) goto label1;  // Backward super jump
 if (test2()) goto label2;  // Backward jump
 code4();
```

これをWebAssemblyで書くにはもちろん`loop`を入れ子にする。

```
(call $code1)
(loop $loop1
  (call $code2)
  (loop $loop2
    (call $code3)
    (br_if $loop1 (call $test1))
    (br_if $loop2 (call $test2))
    (call $code4)))
```

「ラベルより上の内容は`loop`の外（上）、ラベルより下の内容は`loop`の中」
という原則を守れば自然とこのコードが得られる。
比較的C言語のプログラムとの対応関係が取れているため、
前向きgotoの場合よりは読みやすいかもしれない。

## 双方向goto

道具は出揃ったので、前向きgotoと後ろ向きgotoの両方を含むプログラムを考える。

```c
 code1();
 if (test1()) goto label1;  // Forward jump
 code2();
label1:
 code3();
 if (test2()) goto label1;  // Backward jump
 code4();
```

これをWebAssemblyで書くには`block`と`loop`の両方を使うことになる。

```
(block $block1
  (call $code1)
  (br_if $block1 (call $test1))
  (call $code2))  ;; end $block1
(loop $loop1
  (call $code3)
  (br_if $loop1 (call $test2))
  (call $code4))
```

一見複雑な見た目をしているが、
「ラベルより上の内容は`block`の中、ラベルより下の内容は`block`の外（下）」
「ラベルより上の内容は`loop`の外（上）、ラベルより下の内容は`loop`の中」
という2つの原則を守ると自然に書くことができる。原則が便利すぎる。

それでは最後に「ラベルを飛び越す」gotoを見ていく。

```c
 code1();
 if (test1()) goto label2;  // Forward super jump
 code2();
label1:
 code3();
label2:
 code4();
 if (test2()) goto label1;  // Backward super jump
 code5();
```

さて、これをWebAssemblyで書いてこの記事を終わらせたいところだが、
残念ながらこれをそのままWebAssemblyに翻訳することはできない。

前向きgotoに関して
「ラベルより上の内容は`block`の中、ラベルより下の内容は`block`の外（下）」
という原則を守ると次のようなプログラムが得られる。

```
(block $block2
  (block $block1
    (call $code1)
    (br_if $block2 (call $test1))
    (call $code2))  ;; end of $block1
  (call $code3))    ;; end of $block2
(call $code4)
(br_if $loop1 (call $test2))  ;; TODO: Define $loop1
(call $code5)
```

一方、後ろ向きgotoに関して
「ラベルより上の内容は`loop`の外（上）、ラベルより下の内容は`loop`の中」
という原則を守ると次のようなプログラムが得られる。

```
(call $code1)
(br_if $block2 (call $test1))  ;; TODO: Define $block2
(call $code2)
(loop $loop1
  (call $code3)
  (loop $loop2
    (call $code4)
    (br_if $loop1 (call $test2))
    (call $code5)))
```

この2つのプログラムを合体させたいのだが、
`(call $code3)` と `(call $code4)` に注目してほしい。
`(call $code3)`は「`$block2`の内側」かつ「`$loop1`の内側」に存在する。
`(call $code4)`は「`$block2`の外側」かつ「`$loop1`の内側」に存在する。
つまり、`$block2`と`$loop1`が「部分的に」重なっている。
S式ではこのような部分的な重なりは表現できないため、
このようなプログラムを書くことはできない。
ちくしょう何でS式なんて採用したんだ。

## ぼくのかんがえたさいきょうのジャンプ
先程の例がうまく行かなかったのは、
前向きgotoと後ろ向きgotoのラベルが混ざっていたからだ。
そこで、プログラムから後ろ向きgotoを消し去る。
正確には、後ろ向きgotoのためのラベルを先頭に一箇所だけ定義して、
そこに戻ってから前向きgotoをすることにする。
ジャンプ先を覚えるために変数を使用する。
これをC言語で書くと次のようになる。

```c
int jump = 0, next = 0;
do {
  if (next) {
    jump = next;
    next = 0;
  }
  switch (jump) {
  case 0:
    code1();
    if (test1()) {
      next = 2;  // Forward super jump
      continue;
    }
    code2();
  case 1:
    code3();
  case 2:
    code4();
    if (test2()) {
      next = 1;  // Backward super jump
      continue;
    }
    code5();
  }
} while (next);
```

あとはこれをWebAssemblyに翻訳するだけだ。

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

うーん、これはconsidered harmful。

*2022-02-13*
