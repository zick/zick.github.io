# WebAssemblyのテキスト表現を書いてみた

数年前から[WebAssembly](https://developer.mozilla.org/ja/docs/WebAssembly)
というのが流行っているらしい。
WebAssemblyにはテキスト表現とバイナリ表現というものがあり、
通常はCやRustからWebAssemblyのバイナリ表現にコンパイルし、
ブラウザ上で実行するもののようだが [要出典] 、
人間がWebAssemblyのテキスト表現を書いて、
それをバイナリ表現に変換（アセンブル？）することもできる。

そのWebAssemblyのテキスト表現がS式だということで、
数年前に一部の人達が大喜びしていた。
例によって私はすっかり流行に乗りそこねてしまい放置していたのだが、
気が向いたので少し触ってみることにした。

[このページ](https://developer.mozilla.org/ja/docs/WebAssembly/Understanding_the_text_format)
に色々説明が載っているが、
この説明に則り、2つの整数を受け取りその平均を返す関数を書くと次のようになる。

```
(func $avg1 (param $x i32) (param $y i32) (result i32)
  local.get $x
  local.get $y
  i32.add
  i32.const 2
  i32.div_s)
```

1行目を見るとおっと思うが、2行目以降はJavaバイトコードと大差のない
特に面白みのないスタックマシンのコードだ。
十分手書きすることができるが、積極的に手書きしたいとは思わない。
しかし、悲しむ必要はない。もっとLISPっぽい書き方もできる。

```
(func $avg2 (param $x i32) (param $y i32) (result i32)
  (i32.div_s
   (i32.add (local.get $x) (local.get $y))
   (i32.const 2)))
```

少々ごちゃごちゃしたものが付いているとは言え、
かなりLISPに近い気分で書くことができる。読むのもはるかに容易だ。
この差はプログラムが複雑になるほど開いてくる。

```
(func $fact1 (param $n i32) (result i32)
  (local $ret i32)
  local.get $n
  i32.eqz
  (if
   (then
    i32.const 1
    local.set $ret)
   (else
    local.get $n
    i32.const 1
    i32.sub
    call $fact1
    local.get $n
    i32.mul
    local.set $ret))
  local.get $ret)
```

条件分岐にジャンプではなく`if`を使うというのが興味深いというのは置いておいて、
これは読みにくくて仕方ない。
書くのも常にスタックを意識しながら気合を入れて書かないといけない。
これをLISP風の書き方にしたら一気に楽になる。

```
(func $fact2 (param $n i32) (result i32)
  (local $ret i32)
  (if (i32.eqz (local.get $n))
      (local.set $ret (i32.const 1))
      (local.set
       $ret
       (i32.mul (call $fact2 (i32.sub (local.get $n) (i32.const 1)))
                (local.get $n))))
  (local.get $ret))
```

余計な`$ret`という変数が出てくることに目をつぶれば普通に読み書きできる。
なぜMDNのドキュメントがわざわざ読み書きしにくい方式で書いてあるのかが謎だ。
ちなみに、なぜ余計な変数を使っているかというと、
ifに入る前と出るときにスタックの深さが
変わってはいけないという制約があるためだ。
本来はthenとelseでスタックの変化量が等しければ問題ないはずだが。

*2022-02-11*