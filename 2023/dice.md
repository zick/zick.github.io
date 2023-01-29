# 2023年新春LISPポエム

共立出版のbit 1972年2月号に「サイの目くずし」
というゲームのLISPプログラムが載っている。
このサイの目くずしは三山くずしとかニムとか
石取りゲームと呼ばれる
「最後の石を取った人の勝ち」というゲームの亜種なのだが、
石を取る数をサイコロを90度だけ倒した目から選ぶというのが特徴だ。
ゲーム開始時にはサイコロを振るが、それ以降は90度倒すだけだ。
たとえば7個の石を使うとしよう。
ゲーム開始時にサイコロを振り、3が出た場合、
先手は3から90度倒した目である1, 2, 5, 6のいずれかの数だけ石を取る。
現在の目である3とその裏面にある4は選べない。
先手が2を選んだ場合、後手は1, 3, 4, 6のいずれかの数だけ石を取る。
石は最初に7個あり先手が2つ取ったため残りは5個。
そのため後手は6を選ぶことはできない（選ぶと負けとなる）。
ここで後手が1を選んだ場合、石は残り4個となり、
先手は2, 3, 4, 5のいずれかの数だけ石を取るので、
4個石を取れば勝ちとなる。より詳しいルールはbitを読んでほしい。

さて、このサイの目くずしをコンピュータと遊べる
素敵なLISPプログラムは次のようになっている。

```
*
* COMPUTER GA M TO IU ME WO DASHITE SA WO I TO SHITA
*
SAINOME[M;I]
 = [MINUSP[I]->'MAITTA' ;
    ZEROP[I]->'OKINODOKUSAMA' ;
    T->SAINOME[[MTEST[M;CSETQ[K;READ[]]]->
     [ZEROP[I:=DIFFERENCE[I;K]]->I:=-1;
      MINUSP[I]->PROG2[PRINT['KOETA'];I:=0] ;
      T->CADDR[PRINT[LIST['ME'; EQSIGN; M:=
        [LESSP[CSETQ[J;ADD1[REMAINDER[SUB1[I];9]]];7]->
          [MTEST[K;J]->J; ONEP[J]->5; EQUAL[4;J]->2;
           OR[AND[EQUAL[2;J];NOT[EQUAL[2;I]]];EQUAL[6;J]]->3;
           T->1] ;
         MTEST[K;M:=PLUS[8;TIMES[-2;QUOTINENT[J;3]]]]->M ;
         T->QUOTINENT[M;2]] ;
        COMMA; 'GOOKEI'; EQSIGN;
        DIFFERENCE[N;I:=DIFFERENCE[I;M]]]]]] ;
       T->PROG2[PRINT['ERROR'];M]] ; I] ]      ;

*
* X TO IU ME NO TSUGI NI Y TO IU ME GA DASERU KA
*
MTEST[X;Y] = AND[NUMBERP[Y];LESSP[Y;7];LESSP[0;Y];
    NOT[OR[EQUAL[X;Y];EQUAL[X;DIFFERENCE[7;Y]]]]]     ;

*
* MAIN PROGRAM
*
SAINOMEKUZUSI[]
 = PROG[[N] ;
 A;  PRINT['\\*IKUTSU ..*'] ;
     [NULL[N:=READ[]]->RETURN[NIL]] ;
     PRINT['\\*HAJIME NO ME ..*'] ;
     PRINT[SAINOME[READ[];PROG2[PRINT['OSAKINODOOZO'];N]]] ;
     TERPRI[] ; GO[A] ]
```

これで `SAINOMEKUZUSHI [ ]` を評価すると次のような対話を楽しめる。

```
IKUTSU ..
OK 17
HAJIME NO ME ..
OK 3
OSAKINIDOOZO
OK 1
(ME = 4 , GOOKEI = 5)
OK 2
(ME = 1 , GOOKEI = 8)
OK 6
ERROR
OK 4
(ME = 5 , GOOKEI = 17)
OKINODOKUSAMA
```

令和生まれの若者は「これはLISPじゃないだろ」と思ってしまうかもしれないが、
これは確かにLISPだ。M式に近い言語で書かれているに過ぎない。
慶応大のKLISPという処理系で動いたらしい。

これをCommon Lispに移植するのは簡単だ。M式をS式になおしてやればいい。

```lisp
(defun okread ()
  (print 'ok)
  (read))

(defun sainome (m i &aux j k)
  (declare (special n))
  (cond
    ((minusp i) 'maitta)
    ((zerop i) 'okinodokusama)
    (t (sainome (cond ((mtest m (setq k (okread)))
                       (cond ((zerop (setq i (- i k))) (setq i -1))
                             ((minusp i) (prog2 (princ 'koeta) (setq i 0)))
                             (t (caddr (princ (list 'me '= (setq m (cond ((< (setq j (1+ (rem (1- i) 9))) 7) (cond ((mtest k j) j) ((= 1 j) 5) ((= j 4) 2)
                                                                                                                   ((or (and (= 2 j) (not (= 2 i))) (= 6 j)) 3)
                                                                                                                   (t 1)))
                                                                         ((mtest k (setq m (+ 8 (* -2 (truncate j 3))))) m)
                                                                         (t (truncate m 2))))
                                                    '|,| 'gookei '=
                                                    (- n (setq i (- i m)))))))))
                       (t (prog2 (princ 'error) m))) i))))

(defun mtest (x y)
  (and (numberp y) (< y 7) (< 0 y)
       (not (or (= x y) (= x (- 7 y))))))

(defun sainomekuzushi ()
  (prog (n)
     (declare (special n))
   A (princ '|IKUTSU ..|)
     (cond ((null (setq n (okread))) (return nil)))
     (princ '|HAJIME NO ME ..|)
     (print (sainome (okread) (prog2 (princ 'osakinidoozo) n)))
     (terpri) (go A)))
```

記法と関数名が少々異なるのを除けばだいたい同じだ。
特筆すべきは、入出力を真似るためにちょっと工夫していることと、
ダイナミックスコープを使うのに明示的に宣言がたされていること、
それからCSETQで使っている変数を&auxで作っていることくらいだろう。

ちなみにLISP 1.5の `(CSETQ VAR VAL)` は `(VAL)` を返すが、
どうやらKLISPでは `VAL` を返すようだ。

さてさて、Common Lispになおしてもこのプログラムは読みやすいとはいいがたい。
なにが読みにくいのか考えてみたが、特に問題だと思ったのは、
condの入れ子が深いことと、setqが入れ子になっていることだと思った。
とくに `(setq m ...)` の中に `(setq m ...)` が入れ子で現れるのはすごい。

condの深い入れ子はS式でも読みにくいが、M式だとそれが顕著に感じた。
これはcondが括弧一つで表現されるからだと思う。
条件分岐というのは大事なものなのだからもう少し目立つようにしてもらいたい。
今後M式を書くときはcondをあまり入れ子にしないよう気をつけようと思った（小並感）

*2023-01-29*
