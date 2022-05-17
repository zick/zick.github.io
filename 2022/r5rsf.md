# R5RSのformal semanticsをOCamlで書いた

## 背景
bitのバックナンバーを読んでいたら見つけた
「プログラム検証入門」という記事（1980年9月〜12月号掲載）が非常に面白かった。
その記事に参考文献として載っていた
「数理情報学入門―スコット・プログラム理論」という本を買ってみたら、
これまたすごく面白かった。
この本で使っている記号が、R5RSのformal semanticsで使っている記号と
大体同じであることを思い出し、R5RSのformal semanticsを読み直してみたら、
以前よりしっかり理解できて、これまた非常に楽しかった。
楽しかったのだが、
[call-with-valuesの定義](https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-10.html#%_sec_7.2.4)
がおかしいことに気づいた。

```
cwv: E* -> K -> C  [call-with-values]
cwv =
  twoarg(λε1ε2κ.applicate ε1 <> (λε*.applicate ε2 ε*))
```

見ての通りκを受け取ったのに使わず捨てている。
このκは継続なので、それを捨てたら文字通り処理を継続できない。
それから型も正しくないように見える。
おそらく間違っているのだが自信が持てない。
R7RSではどうなっているのかと見てみたら、相変わらずκを捨てていた。
いよいよ自信がなくなってきたので、頼りない私の脳ではなく
計算機の力を使ってこの定義が正しいか確かめることにした。

## 方針
R5RSのformal semanticsをOCamlに「直訳」する。
私の予想が正しければcall-with-valuesは型エラーでコンパイルできないはずだ。

### 抽象構文

構文は主役でないので動けば何でもいい。

```
Exp -> K | I | (E0 E*)
    |  (lambda (I*) Γ* E0)
    |  ...
```

こんな定義をテキトーにOCamlの型に落とし込む。

```ocaml
type tyExp =
  ExK of tyCon
  | ExI of tyIde
  | ExA of tyExp * (tyExp list)
  | ExL1 of (tyIde list) * (tyExp list) * tyExp
```

LISPっぽさを微塵も感じないがそんなことは重要ではない。


### 領域方程式

（再帰的な）データをどうやって数学的に表現するか、
というのが表示的意味論の大事なところだと思うが、
領域方程式をOCamlに直訳する上でそんな難しいことは一切考えなくて良い。

```
F = L * (E* -> K -> C)
E = Q + H + R + ...
S = L -> (E * T)
C = S -> A
K = E* -> C
```

方程式というだけあって、左辺と右辺の記号が入り乱れているが、
何も考えず定義どおりに再帰的な型を定義するだけだ。

```ocaml
type tyF = tyL * (tyEl -> tyK -> tyC)
and  tyE =
  EQ of tyQ
| ER of tyR
| ...
and  tyEl = tyE list
and  tyS = tyL -> (tyE * tyT)
and  tyC = tyS -> tyA
and  tyK = tyEl -> tyC
```

### 意味関数

構文と意味領域ができたら、構文を意味領域に対応させる意味関数を定義する。
ラムダ式をそのままOCamlの関数にすれば良い。

```
ℰ[[K]] = λρκ.send(𝒦[[K]])κ
ℰ[[(E0 E*)]] =
  λρκ.ℰ(permute(<E0>§E*))
    ρ
    (λε*.((λε*.applicate(ε*↓1)(ε*†1)κ)
      (unpermute ε*)))
```

こんな定義を次のように書く。

```ocaml
let rec fnE exp =
  match exp with
    ExK c -> fun env cont -> send (fnK c) cont
  | ExA (f, a) -> fun env cont ->
      fnEl (f::a) env (fun el -> applicate (List.hd el) (List.tl el) cont)
  | ...
and ...
```

引数の順番を（評価順を定義しないために）並び替える処理や
エラー処理などは適宜省略する。
こればっかりは直訳するより頭を使ってでも省略する方が楽だ。

### 補助関数

意味関数から呼ばれる補助関数や、Schemeのビルトイン手続きも
意味関数同様にOCamlの関数にすれば良い。

```
send = λεκ.κ<ε>
cwv =
  twoarg(λε1ε2κ.applicate ε1 <> (λε*.applicate ε2 ε*))
```

これをこう。

```ocaml
let send ev cont = cont [ev]

(* 問題のコード *)
let cwv = twoarg(fun e1 e2 k -> applicate e1 [] (fun el -> applicate e2 el))
```

cwvがコンパイルできるかはひとまず置いておこう。

### 環境と内部記憶

実装方法が自明でないものとして環境と内部記憶がある。
環境は識別子からロケーションへの関数、
内部記憶はロケーションから値への関数だが、
`ρ[x/i]`
といった記法で定義が書き換わる。
真面目に実装するならテーブルを作るのがいいと思うが、
手を抜くのを優先して、ある意味定義通りの直訳をした。

```ocaml
let addToEnv k v env =
  fun ide ->
    if ide = k then v
    else env ide

(* 同様に addToStore も実装する *)
```

値を追加するたび関数がどんどん入れ子になっていく素敵なつくりだ。
これを使ってビルトイン手続きを登録できる。

```ocaml
let addPfn name fn (env, store) =
  let l = newl store in
    (addToEnv (name :> tyIde) l env,
     addToStore l ((EF (l, fn)), TTrue) store)

let (defEnv, defStore) =
  (addPfn "call/cc" cwcc
  (addPfn "+" add (empEnv, empStore))
  )
```

ごちゃごちゃしているのでもう少し整理したほうがいいと思う。
でも今の所call/ccと+の2つしか手続きがないのであまり問題になってない。

## 結果

さて、例のcall-with-valuesだが、定義しようとすると次のようになる。

```
# let cwv = twoarg(fun e1 e2 k -> applicate e1 [] (fun el -> applicate e2 el));;
Error: This expression has type tyK -> tyC
       but an expression was expected of type tyC = tyS -> tyA
       Type tyK = tyEl -> tyC is not compatible with type
         tyS = tyL -> tyE * tyT
       Type tyEl = tyE list is not compatible with type tyL = int
```

予想通り型エラーとなった。`C`が欲しい場所に`K -> C`があると。
ここで使われていないκ（に対応するk）の型がちょうど`K`なので、
kを書き足してやるとちゃんと型が付く。

```
# let cwv = twoarg(fun e1 e2 k -> applicate e1 [] (fun el -> applicate e2 el k));;
val cwv : tyE list -> tyK -> tyC = <fun>
```

R5RSに書かれている `E* -> K -> C` という型とも一致している。
これで自信を持ってR5RSの定義が間違っていると言えるようになった。

これだけだと少しさみしいのでcall/ccを使ってみた。

```
(* (+ (call/cc (lambda (k) (+ (k 99) 3))) 1) => 100 *)

# fnE (ExA (ExI "+", [
  (ExA ((ExI "call/cc"), [
    (ExL1 (["k"], [],
      ExA (ExI "+", [ExA (ExI "k", [ExK (KNum 99)]); ExK (KNum 3)])
      ))
    ]));
ExK (KNum 1)])) defEnv fnId defStore;;
- : tyA = [ER 100]
```

ごちゃごちゃして何をやっているのか分からないがちゃんとcall/ccが動いている。
call/ccは`λε*κ'.κε*`なる関数を作って継続κ'を正しく捨てるのが面白い。

## まとめ

OCamlを使ったらR5RSのformal semanticsを簡単に直訳できた。
良くも悪くもこの作業は表示的意味論を一切理解していなくてもできてしまう。
残りの部分の実装は読者の練習問題とする。

*2022-05-17*
