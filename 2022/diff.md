# 2022年秋のLISPポエム

最近、共立出版のbitを創刊号から読んでいるのだが、
1970年12月号に記念すべき初めてのLISPの記事が載っており、
記号微分のプログラムが出てくる。

```lisp
DEFINE((
  (DIFF (LAMBDA (E X) (COND ((ATOM E) (COND ((EQ E X)
      (QUOTE ONE)) (T (QUOTE ZERO)))) ((EQ (CAR E) (QUOTE
      PLUS)) (LIST (QUOTE PLUS) (DIFF (CADR E) X) (DIFF (
      CADDR E) X))) ((EQ (CAR E) (QUOTE TIMES)) (LIST
      (QUOTE PLUS) (LIST (QUOTE TIMES) (CADDR E) (DIFF (
      CADR E) X)) (LIST (QUOTE TIMES) (CADR E) (DIFF (
      CADDR E) X)))) (T (PRINT (QUOTE ERROR)))) ))
))
TRACE((DIFF))
DIFF((PLUS (TIMES (PLUS (TIMES A X) B) X) C) X)
```

改行の仕方が非常に趣深いのは置いておいて、
現代のLISPしか知らない人は `DEFINE`, `TRACE`, `DIFF`
の呼び出しが括弧で囲まれていないことに驚くかもしれないが、
これはevalquote lispというやつだ。
詳しくは[この記事](/2022/evalquote.html)に書いたが、
現代のLISPだと `(関数 '引数1 '引数2)` と書くものを
evalquote lispだと `関数 (引数1 引数2)` と書く。
括弧の位置が違うのと、引数が勝手にクォートされるのが特徴だ。
引数を評価したいときは `(LAMBDA () (関数 引数1 引数2)) ()`
とLAMBDAで囲んでしまえば良い。

今どきevalquote lispが動かせる処理系はあまりないと思うが、
私の作った[Ichigo Lisp](/ichigo/)では more options から
Evalquote を選択すれば動かせる。

```lisp
> DIFF((PLUS (TIMES (PLUS (TIMES A X) B) X) C) X)
; 1 ENTER DIFF ((PLUS (TIMES (PLUS (TIMES A X) B) X) C) X)
; 2 ENTER DIFF ((TIMES (PLUS (TIMES A X) B) X) X)
; 3 ENTER DIFF ((PLUS (TIMES A X) B) X)
; 4 ENTER DIFF ((TIMES A X) X)
; 5 ENTER DIFF (A X)
; 5 EXIT DIFF=ZERO
; 5 ENTER DIFF (X X)
; 5 EXIT DIFF=ONE
; 4 EXIT DIFF=(PLUS (TIMES X ZERO) (TIMES A ONE))
; 4 ENTER DIFF (B X)
; 4 EXIT DIFF=ZERO
; 3 EXIT DIFF=(PLUS (PLUS (TIMES X ZERO) (TIMES A ONE)) ZERO)
; 3 ENTER DIFF (X X)
; 3 EXIT DIFF=ONE
; 2 EXIT DIFF=(PLUS (TIMES X (PLUS (PLUS (TIMES X ZERO) (TIMES A ONE)) ZERO)) (TIMES (PLUS (TIMES A X) B) ONE))
; 2 ENTER DIFF (C X)
; 2 EXIT DIFF=ZERO
; 1 EXIT DIFF=(PLUS (PLUS (TIMES X (PLUS (PLUS (TIMES X ZERO) (TIMES A ONE)) ZERO)) (TIMES (PLUS (TIMES A X) B) ONE)) ZERO)
(PLUS (PLUS (TIMES X (PLUS (PLUS (TIMES X ZERO) (TIMES A ONE)) ZERO)) (TIMES (PLUS (TIMES A X) B) ONE)) ZERO)
>
```

1970年のプログラムがブラウザでそのまま動くのは非常に味わい深い。
ただ、このプログラム、出力が少々見苦しい。
具体的には0を足したり掛けたりする式が多数出てくるのがダサい。
これを消す関数を追加してみよう。

```lisp
DEFINE((
  (SIMPLIFY (LAMBDA (E)
    (COND ((ATOM E) E)
          ((EQ (CAR E) (QUOTE PLUS))
           (SIMPLIFY-PLUS (SIMPLIFY (CADR E)) (SIMPLIFY (CADDR E))))
          ((EQ (CAR E) (QUOTE TIMES))
           (SIMPLIFY-TIMES (SIMPLIFY (CADR E)) (SIMPLIFY (CADDR E))))
          (T E))))
  (SIMPLIFY-PLUS (LAMBDA (X Y)
    (COND ((EQ X (QUOTE ZERO)) Y)
          ((EQ Y (QUOTE ZERO)) X)
          (T (LIST (QUOTE PLUS) X Y)))))
  (SIMPLIFY-TIMES (LAMBDA (X Y)
    (COND ((EQ X (QUOTE ZERO)) X)
          ((EQ Y (QUOTE ZERO)) Y)
          ((EQ X (QUOTE ONE)) Y)
          ((EQ Y (QUOTE ONE)) X)
          (T (LIST (QUOTE TIMES) X Y)))))
))
```

```lisp
> SIMPLIFY((PLUS (PLUS (TIMES X (PLUS (PLUS (TIMES X ZERO) (TIMES A ONE)) ZERO)) (TIMES (PLUS (TIMES A X) B) ONE)) ZERO))
(PLUS (TIMES X A) (PLUS (TIMES A X) B))
>
```

無事（相対的に）見やすい式が出力されるようになった。
まあ、bit 1970年11月号に出てくる
SNOBOLでの記号微分のプログラムでやっていることを
そのままLISPに移植しただけなのだが。

さてさて、正直なところ算数のできない私には微分なんてどうでもいいことなのだが、
ここに出てくるevalquote lispというやつについて改めて考えてみた。
古いLISPのプログラムをそれなりに読み、
少なくても同世代の人よりはevalquote lispに親しんでいる自信のある私だが、
それでも自分でevalquoteな式を書くときは少々頭を使う。
現代のLISP (eval lispと呼ばれている) と書き方が絶妙に違うから、
頭を切り替えないといけないからだ。
では、昔の人は頭を使わずにevalquoteな式を書けたのだろうか。
当時の人にインタビューしたわけではないので完全に私の妄想になるのだが、
恐らく多少なりとも苦労したのではないかと思う。
現代にevalquoteが残っていないのが何よりもの証拠だ。
evalquote lispとeval lispが異なるのは一番外側の式だけで、
関数の内側は同じ書き方になる。
すでに書いたとおり、LAMBDAの内側、例えば
`(LAMBDA () (関数 引数1 引数2)) ()`
の引数1と引数2は評価される。
関数定義に使うDEFINEの内側なども同様だ。
つまり、式の一番外側とそれ以外で頭を切り替えないといけない。
少なくても私にとってこれは大きな負担だ。
当時の人にとっても
「コンビニで買い物するたびに『レジ袋ください』と言わなければいけない」
程度には負担になったのではないかと推測される。

さらに考えてみたら当時はM式というものが使われていた。
曰く「S式になったプログラムはひじょうに見にくい」そうで、
人間はM式というものを使っていたらしい。
これは主に改行とインデントのせいだと思うのだが、
当時のカードを使ってプログラムを入力していた時代背景を考えると、
不要な改行やスペースは贅沢だったのかもしれない。
何らかの理由はあれど、当時の人達はM式とevalquoteとevalの3種類のプログラムを
脳内で変換する必要があり、それが
「予防接種のたびに『左利きなんで右腕に打ってもらっていいですか？』とお願いする」
程度には負担になっていたのではないかと推測される。
負担になっていなければ今もM式やevalquote lispが生き残っていていいはずだ。

さらにさらに考えてみるとこれはLISPだけの話ではない。
当時の手続き型言語は「フローチャートを書いてからプログラムを書く」
といったやり方が広く使われていたらしい。
これは「M式を書いてからS式を書く」というのに対応しているのではないだろうか。
ついでに言えば豊富な数学記号（例えば`≦`）を使ったプログラムを手書きしてから
コンピュータ上での表現（例えば`<=`）に直したプログラムを打ち込むという
流儀もあったらしい。
そう考えるとLISPだけが変なことをしていたというわけではなさそうだ。

こういった
「何らかの手書きの記述してから、それをコンピュータ向けの表現に変換する」
といった手順が広く使われていた原因は
「プログラミング言語と人間の考え方の間には大きなギャップがある」
という思想が原因ではないのだろうか。
いや、私の生まれる前の話なので全部妄想なのだが。
現代では事前にM式を書いたりフローチャートを書いたりする文化は絶滅している
（フローチャート自体は生き残っているが、
「プログラムを書くときは必ず事前にフローチャートを書きます」
なんて人間はさすがに絶滅しているだろう）。
人間とプログラミング言語の間にあったギャップは、
人間がコンピュータに歩み寄ることで解消したのだろう。
そう考えると非常に面白い。
人間とコンピュータの対話において、
人間がコンピュータに歩み寄るべきか、
コンピュータを人間に近づけるべきか、
どちらが楽なのかはしっかりと考えたほうがいいのかもしれない。知らんけど。

*2022-11-05*
