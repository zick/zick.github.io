# Googleスプレッドシートの名前付き関数でLISPインタプリタを作った

## はじめに

[こんなの](https://docs.google.com/spreadsheets/d/1m_QQlAaMtxKUjwncPMBJilq2ZbStVas83Qqw9Dn6E_M/edit?usp=sharing)
を作った。
エラーチェックをサボっていたり、
少し複雑な計算をすると途中で打ち切られたりするが、
それなりに遊べるはず。

## Googleスプレッドシートとは

Googleスプレッドシートはいわゆる表計算ソフト。
表に色々入力したり、色々計算させたりする。
`=SUM(A2:A10)` とか書くと2行A列から10行A列までの合計を計算してくれたりする。
スクリーンショットを貼り付けるためのソフトではないはず。

## 名前付き関数とは

Googleスプレッドシートには前述の`SUM`の他にも色々関数があるが、
ユーザが自分で新しい関数を定義することもでる。
例えばNの階乗を求める関数を定義してみよう。

メニューの [データ] - [名前付き関数] を選択し、 [新しい関数を追加] をクリック。
[関数名] に `FA` を入力、 [引数] に `n` を追加して、
[数式の定義] に `=IF(n<=0 , 1, n * FA(n-1))` を入力すると
関数 `FA` が定義されるので、テキトーなセルに `=FA(10)` と書くと
セルの内容が `3628800` になる。

このように名前が付いた関数を「名前付き関数」と呼ぶが、
ユーザは名前のない関数も定義できる。セルの中に直接
`=LAMBDA(n, n * n * n)(3)`
と書くとセルの内容は `27` になる。
この `LAMBDA` は名前付き関数の中で使うこともできる。

## なにが難しいのか

「こんな便利なものがあったらLISPインタプリタくらい書けて当然だろ」
というお叱りの声が聞こえてきそうだが、
面倒な点が何点かあるので、それを説明したい。

### 構造体も配列もない

まず、データ型がしょぼい。数値、真偽値、文字列くらいしか使えない。
構造体を定義することもできなければ、配列もない。
正確には「配列」という名称のものはあるが、
これは自由にインデックスアクセスできる「普通の配列」とは異なる不便なものだ。

この制約下で構造体を表現する一番簡単な方法は、文字列を使うことだろう。
例えば、 `x` と `y` の2つの引数を受け取り、
そのペアを表現するデータを作りたければ
`=CONCATENATE(";x=", x, ";y=", y, ";")`
などとしてやればよい。 `CONCATENATE` は文字列の連結を行う関数だ。
`x` が `77` 、 `y` が `66` なら `;x=77;y=66;` という文字列が作られる。
この文字列から `x` と `y` の値を取り出すのには正規表現を使えば簡単だ。
`x` を取り出すには
`=REGEXEXTRACT(the_pair_data, ";x=([^;]*);")`
と書いてやればよい。 `REGEXEXTRACT` は正規表現にマッチした部分を返す関数だ。
この場合、カッコで囲まれた `[^;]*` の部分を返す。
`the_pair_data` が `;x=77;y=66;` だとすると、
`77` が得られる。
それぞれの値に区切り文字を含むことができないという成約を受け入れれば、
任意個の値を文字列に埋め込むことができる。
入れ子構造を直接格納することもできないが、それも受け入れることにしよう。

値を書き換える場合も正規表現を使って色々できるが、
もっと簡単な方法として文字列の先頭に新たなデータをくっつけるという方法がある。
`REGEXEXTRACT` は最初にマッチしたものを返すため、
古いデータが後ろの方にあっても無視される。
雑なやり方だが記述が面倒な言語だとこういった手法も馬鹿にできない。

配列も同様のやり方で表現できる。
0番目の値が555、1番目の値が444、2番目の値が333の配列を
`|0:555|1:444|2:333|`
のように表すとしよう（このデータは `CONCATENATE` などを使って作ればよい）。
`n` 番目の値を取り出すには
`=REGEXEXTRACT(the_array_data, CONCATENATE("\|", n, ":([^\|]*)\|"))`
と書けばよい。 `the_array_data` が先程の配列で、 `n` が `2` であれば、
その値は `333` となる。

もっとも、配列に専用の表現を使わなくても、構造体に `a0`, `a1`, `a2` といった
フィールドを突っ込めばよかったのではないかと今更になって気づいたが、
あまり気にしないことにする。

### グローバル変数もmutableな変数もない

さて、構造体と配列を手に入れてかなり便利になったが、
まだ足らないものがある。mutableな変数だ。
まず、グローバル変数というものがない。
値を書き換えないのであれば、定数を返す関数を代わりに使うこともできるが、
値を書き換えたい場合は打つ手がない。
ローカル変数っぽいものとして、名前付き関数や
`LAMBDA` のパラメータがあるのだが、これも書き換えることができない。

この問題は書き換えたい変数を構造体（実体は文字列）に入れて、
それを（ほぼ）すべての関数で引き回すことで対処する。
[C++のテンプレートでLISPを作った](/2022/template.html)
ときとまったく同じやり方だ。

コンスセルを作ることを考えてみよう。
各コンスセルを区別するため、コンスセルにはユニークなIDを付けたいが、
それをするためには「世界に何個コンスセルがあるか」を覚える必要がある。
またコンスセルがmutableであることを考えると
「コンスセルのCARに何が入っているか」
「コンスセルのCDRに何が入っているか」
も覚えておく必要がある。
これらを表現するのに
`;head=1;car=|0:0|;cdr=|0:0|;`
のような構造体を使う。これは「世界にはコンセルが1つあり、
0個目のコンスセルのCARは0個目のコンスセル自身を指し、
0個目のコンスセルのCDRも0個目のコンスセル自身を指す」状態を表す。
この状態で新たなコンスセルを作る関数 `CONS` が呼ばれたとする。
`CONS`は世界を
`;head=2;car=|1:0|0:0|;cdr=|1:0|0:0|;`
のように書き換える。すなわち「世界にはコンセルが2つあり、
1個目のコンスセルのCARは0個目のコンスセルを指し、
0個目のコンスセルのCARは0個目のコンスセル自身を指し、
1個目のコンスセルのCDRは0個目のコンスセルを指す
0個目のコンスセルのCDRも0個目のコンスセル自身を指す」という状態だ。
`CONS`は新しく作ったコンスセルのID、すなわち `1` に加えて、
新しく変化した世界を返す必要がある。
多値などという便利なものがあるはずがないので、
本来の値と世界を一つに構造体に詰め込んで
`;value=1;head=2;car=|1:0|0:0|;cdr=|1:0|0:0|;`
のような値を返すことになる。

実際の関数`CONS`はCAR部の値 `a` と CDR部の値 `d` と現在の状態 `s` を受け取る、
次のような関数となっている。
```
=VSTATE(SHEAD(s), STATE(SHEAD(s)+1, CONCATENATE("|", SHEAD(s), ":", a, SCAR(s)), CONCATENATE("|", SHEAD(s), ":", d, SCDR(s))))
```
`VSTATE`は `value` と状態のペアを構造体に詰め込む関数。
`STATE` は新たな状態を作る関数。
`SHEAD` は状態から `head` を取り出す関数。
`SCAR` と `SCDR` はそれぞれ `car` と `cdr` を取り出す関数だ。

このように変数を書き換える関数は新たな状態を返すようにする。
変数を読む関数も現在の状態を受け取る必要がある。
結果としてほとんどの関数が状態を受け取ることになる。

### 逐次処理もループもない

さて、これでLISPを書くのに必要な道具はそろった。
道具はそろったのだが、その記述方法は想像以上に面倒だ。
例えば環境に新たな束縛を加える `ADDTOENV` という関数を考えてみる。
[DartLisp](https://github.com/zick/DartLisp)では次のようなコードになっている。
```dart
addToEnv(sym, val, env) {
  env.car = makeCons(makeCons(sym, val), env.car);
}
```
念の為Common Lispでも書いておくと次のような関数だ。
```lisp
(defun add-to-env (sym val env)
  (setf (car env) (cons (cons sym val) (car env))))
```
これが名前付き関数だと次のようなおぞましいものになる。
```
=LAMBDA(vvs, LAMBDA(v3s, LAMBDA(v4s, VSTATE(VAL(vs), v4s)) (SETCAR(VAL(vs), VAL(v3s), v3s))) (CONS(VAL(vvs), CAR_(VAL(vs), vs), vvs))) (CONS(sym, vl, vs))
```

このようなおぞましいコードが生まれる原因は
`LAMBDA` を使わないと逐次処理が書けないことにある。
まずコンスセルを作るだけの関数を考えてみよう。
`(cons x y)` と同等のことをやりたければ
`=CONS(x, y, s)`
と書けばいい。しかし、コンスセルを作った後に何かをする関数は急に面倒になる。
なぜなら「コンスセルが作られたあとの状態」に対して操作をする必要があるからだ。
`(cons z (cons x y)` と同等のことをやりたければ
`=LAMBDA(vs, CONS(z, VAL(vs), vs)) (CONS(x, y, s))`
のように書く必要がある。 `VAL` は状態から `value` を取り出す関数だ。
`CONS(x, y, s)` が作った新たな状態を受けるために `LAMBDA` を使う必要がある。
注目すべきはコードの読み方だ。右から左に読む必要がある。
まず、 `CONS(x, y, s)` が実行されてから
`CONS(z, VAL(vs), vs)` が実行される。
もう一つ処理を加えるとさらに面倒になる。
`(cons w (cons z (cons x y))` と同等のことをやりたければ
`=LAMBDA(vs, LAMBDA(vvs, CONS(w, VAL(vvs), vvs)) (CONS(z, VAL(vs), vs))) (CONS(x, y, s))`
このようなとんでもないコードになる。もちろんコードは右から読む必要がある。
まず、 `CONS(x, y, s)` が実行され、その結果が `vs` に入る。
次に、 `CONS(z, VAL(vs), vs)` が実行され、その結果が `vvs` に入る。
最後に `CONS(w, VAL(vvs), vvs)` が実行されてこれが全体の結果となる。
ここまで分かれば前述の `ADDTOENV` も読めるだろう。

逐次処理だけでなく、ループもないので代わりに再帰を使う必要があるのだが、
逐次処理の難しさと比べれば、そんなものは大した問題ではないだろう。

### printfもデバッガもない

これで必要な道具もそろったし、ある種のイディオムも説明した。
これで間違いなくLISPインタプリタを書けるのだが、
その作業を簡単に感じるのは、一発でバグのないプログラムを書ける人だけだろう。
まだ気づいていない人もいるかもしれないので念のため説明するが、
Googleスプレッドシートはプログラミングをするためのものではないのだ。
エラーが起きたとき、エラーの内容は説明してくれるが、
そのエラーがどこで起きたのかは教えてくれない。
当然ながらスタックトレースなんてものもない。
printfデバッグをしようにも、何かを表示する関数なんてものはない。
怪しいところを勘で見つけ出し、
状態のダンプをじっと眺めてデバッグをするしかない。
一言でいうと、つらい。

## おわりに

「それなりに大変なのは分かったけど、やっぱり関数が定義できるなら簡単だろ。
表計算ソフトなんだからちゃんと表計算でLISPを作れ」
という意見が飛んできそうなので書いておくが、
実は最初は名前付き関数や `LAMBDA` を使わずにLISPを書いていた。
なんとかREADが動くところまでは書けたのだが、
そのままの方針ではEVALを書くのは今の私には事実上不可能という結論になった
（学生の頃なら書けたかもしれない）。
そこで、名前付き関数を使ってLISPを書けば
新しいアイディアが生まれるかもしれないと思い作ったのがこれだ。
実際に当初と比べ幾分マシなアイディアが思いついたのだが、
結局の所、とてつもなく大変なので挑戦するのはまた今度にしたい。

*2022-10-01*
