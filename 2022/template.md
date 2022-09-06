# C++のテンプレートでLISPを作った話

## はじめに

1年半ほど前、C++のテンプレートでLISPを実装した
（[コード](https://github.com/zick/TempLisp)）。
C++コードの「コンパイル時」にLISPが動くというものだ。
実行時にはコンパイル時に作られた文字列定数を表示するだけだ。
どう作ったか覚えているうちに日本語の文章を書こうと思っていたのに、
すっかり時間が経ってしまいもはやほとんど覚えていないが、
完全に忘れてしまう前に何かしら書いておこうと思う。

## 超高速C++テンプレート入門

C++に詳しくない人向けにテンプレートのすごく雑な説明をする。
私自身C++に詳しくないので、多分に間違いが含まれているだろう。
C++に詳しい方は生暖かい目で見守ってほしい。

### 単純な型

C++の変数には型がある。
整数型の変数には整数しか代入できない。
文字列型の変数には文字列しか代入できない。

```c++
int n = 42;  // 整数型の変数
std::string s = "hello";  // 文字列型の変数
n = 21;  // 整数型の変数に整数を代入
// s = 7;  コンパイルエラー（文字列型の変数に整数は代入できない）
```

変数の型はコンパイル時に決まる。
型の異なる値を代入しようとするとコンパイルエラーとなる。

プログラムの実行時には値こそが大事なるのだが、
今回はコンパイル時にLISPプログラムを動かすのが目的なので、
値ではなく型が大事だ。

### 複雑な型

他の型を含んだ新しい型を作ることができる。

```c++
struct IPoint {  // 整数を2つ含む構造体
  int x;
  int y;
};
IPoint ip = {3, 4};  // 座標(3, 4)

struct DPoint {  // 浮動小数点数を2つ含む構造体
  double x;
  double y;
};
DPoint dp = {3.5, 4.6};  // 座標(3.5, 4.6)
```

構造体は便利なのだが、単純な作りだと上の例のように、2次元座標を表すためにも、
整数の場合と浮動小数点数の場合で別の構造体を作ることになる。
座標に対する操作を行う関数もそれぞれの構造体向けに別々に作ることになる。

これではあまりにも不便だ。
そこでC++のテンプレートというものを使うと一回の定義で様々な型に対応できる。

```c++
template<typename T>  // Pointは型Tを受け取る
struct Point {        // Pointは型Tの変数を2つ含む
  T x;
  T y;
};

Point<int> ip = {3, 4};         // intのPoint
Point<double> dp = {3.5, 4.6};  // doubleのPoint
```

型を変数にしてしまい、「型Tの2次元座標」を定義することで、
一度に整数と浮動小数点数の両方をサポートできる。

### 型の公開

ここでは実用的なプログラムの話ではなく、
非実用的なプログラムの話をするため、
上記`Point`よりも更に単純な例を使って説明を続ける。

```c++
template<typename T>  // Wrapperは型Tを受け取る
struct Wrapper {      // Wrapperは型Tの変数を1つ含む
  T value;
};

Wrapper<int> int_wrapper;        // 整数を含む構造体
Wrapper<double> double_wrapper;  // 浮動小数点数を含む構造体
```

`Wrapper`は型Tの変数を1つだけ含む構造体だ。
この「型T」は`Wrapper`の内側からしか見えないが、
`using`というものを使って別名をつければば外側からも見えるようになる。

```c++
template<typename T>
struct Wrapper {
  using type = T;  // 型Tにtypeという別名を付けて公開する
  type value;
};

Wrapper<int> int_wrapper;  // 整数を含む構造体
Wrapper<int>::type i = 3;  // ただの整数 `int i` と同じ
```

すでに述べたとおり、変数の型はコンパイル時に決まる。
`Wrapper<int>::type`という一見複雑なものがコンパイル時に`int`になり、
整数型の変数が作られる。
これはコンパイル時に型が「計算」されたとも言える。

もう少し露骨な例を見てみよう。

```c++
template<typename A, typename D>  // Consは型Aと型Dを受け取る
struct Cons {
  using car = A;  // carという名前で型Aを公開する
  using cdr = D;  // cdrという名前で型Dを公開する
};

Cons<int, double>::car i = 4;    // 整数 `int i` と同じ
Cons<int, double>::cdr d = 5.6;  // 浮動小数点数 `double d` と同じ
```

コンパイル時に
`Cons<int, double>::car`が`int`になり、
`Cons<int, double>::cdr`が`double`になるのは、
なんらかの「計算」が行われていると感じられただろうか。

### 条件分岐

このようにテンプレートをつかって型を計算するのだが、
計算というからには条件分岐くらいはしたい。
そんなわけで次は条件分岐の話をする。

先程の`Wrapper`に少し手を加えて
「型Tとして**単**精度浮動小数点数が指定されたときには、
**倍**精度浮動小数点数を使う」ようにしよう。

```c++
template<typename T>
struct Wrapper {
  using type = T;  // 基本的にtypeはT
  type value;
};
template<>
struct Wrapper<float> {  // Tが単精度浮動小数点数なら
  using type = double;   // typeは倍精度浮動小数点数
  type value;
}

Wrapper<int> int_wrapper;        // 整数を含む構造体
Wrapper<int>::type i = 3;        // ただの整数
Wrapper<float> float_wrapper;    // 倍精度浮動小数点数を含む構造体
Wrapper<float>::type d = 1.0;    // 倍精度浮動小数点数（単精度ではない）
```

これは「型Tがfloatの場合とそれ以外の場合で条件分岐をしている」といえる。
もう少し露骨な例を見てみよう。

```c++
// 特に意味のない空の構造体
struct NIL {};
struct T {};

// 型TがNILかそれ以外か判定する構造体
// 結果は型answerとして公開する
template<typename T1>  // Nullは型T1を受け取る
struct Null {
  using answer = NIL;  // answerは基本的にはNIL
};
template<>
struct Null<NIL> {     // 型T1がNILの場合は
  using answer = T;    // answerはTになる
};

Null<T>::answer this_is_nil;  // 型はNIL
Null<NIL>::answer this_is_t;  // 型はT
```

こうすると「条件分岐により型が計算される」感じが分かるだろう。
`Null<Something>`と書くと`Something`が`NIL`なのかそれ以外なのか
「コンパイル時」に計算されるのだ。

### 再帰

条件分岐ができるのは嬉しいが、それだけでは物足りない。
やはり反復、欲を言えば再帰がしたくなる。
そんなわけで次は再帰の話だ。

まず、`Wrapper`を含む`Wrapper`というものを考えてみよう。

```c++
int i;                                 // 整数
Wrapper<int> wi;                       // 整数のWrapper
Wrapper<Wrapper<int>> wwi;             // 整数のWrapperのWrapper
Wrapper<Wrapper<int>>::type wi2;       // 整数のWrapper
Wrapper<Wrapper<int>>::type::type i2;  // 整数
```

`Wrapper`にn回包まれた型Tを取り出すには`type`をn回使う必要がある。
つまり`Wrapper`に100回包まれていたら`type`を100回も使う必要があるのだ。
これは面倒なので一気に型Tを取り出す`Unwrap`というものを作ってみる。

```c++
template<typename T>
struct Unwrap {    // Wrapper型の一番内側のTを取り出すための構造体
  using type = T;  // 基本的にはtypeはTになる
};
template<typename T2>
struct Unwrap<Wrapper<T2>> {               // TがWrapper<T2>という形の場合
  using type = typename Unwrap<T2>::type;  // T2を再帰的にUnwrapする
};

Unwrap<Wrapper<Wrapper<Wrapper<Wrapper<int>>>>>::type i;  // 整数 `int i` と同じ
```

文法の細かいところはさておき、紛れもなく再帰だ。
これがあればリストをたどるような型が書ける。
例えばリストの末尾の要素を取り出す型を書いてみよう。

```c++
struct NIL {};
struct T {};
template<typename A, typename D>
struct Cons {
  using car = A;
  using cdr = D;
};

template<typename T>
struct Last {          // リストの末尾の要素を取り出すための構造体
  using answer = NIL;  // 基本的にanswerはNIL
};
template<typename A, typename D>
struct Last<Cons<A, D>> {                   // TがCons<A, D>の形の場合
  using answer = typename Last<D>::answer;  // 再帰的にDにLastを適用
};
template<typename A>
struct Last<Cons<A, NIL>> {  // TがCons<A, NIL>の形の場合
  using answer = A;          // answerはAになる
};

// (T NIL T) の末尾の要素はT
Last<Cons<T, Cons<NIL, Cons<T, NIL>>>>::answer t;  // T
```

さて、ここまでできればLISPが実装できるのは自明だろう。

### 実行時の処理

ここまでの内容で `Eval<Expression>::value` などと書けば、
何かしらの型が得られるようなプログラムが書けそうだが、
単に型が得られただけでは実行時に何も起こらないので面白くない。
実行時に何かが表示されるようにしてみよう。

```c++
// 構造体NILとTは自分の名前を文字列として返せる
struct NIL { static std::string toString() { return "NIL"; } };
struct T { static std::string toString() { return "T"; } };

template<typename T1>
struct Eval {
  using value = NIL;  // valueは基本的にNIL
};
template<>
struct Eval<T> {      // T1がTの場合
  using value = T;    // valueはT
};

int main() {
  // 標準出力にEvalの結果を表示
  std::cout << Eval<NIL>::value::toString() << std::endl;  // NIL
  std::cout << Eval<T>::value::toString() << std::endl;    // T
  return 0;
}
```

シンボルにちょっとした関数を付け加えることで、実行時に名前を表示できる。
コンスにも同様の仕組みを作ればよいのだが、
単純に作ると実行時の文字列の連結などが行われるようになる。
可能な限りすべてをコンパイル時に行うためには
`constexpr`というものを使えば良い。
ただし、どう使えばいいのか私も覚えていないので説明は割愛する。
お近くのC++に詳しい人に聞いてください。

## テンプレートでLISPをつくる

すでに述べたとおり、シンボルは（基本的に）空の構造体、
コンスセルは2つの型をとる構造体で表現できる。

```c++
// 実際には表示用のコードが含まれる
struct T {};
struct QUOTE {};
struct IF {};
// NILのCAR/CDRをNILにするとちょっとだけ嬉しい
struct NIL {
  using car = NIL;
  using cdr = NIL;
};
template<typename A, typename D>
struct Cons {
  using car = A;
  using cdr = D;
};

using example1 = Cons<T, Cons<T, Cons<T, NIL>>>;  // (T T T)
using example2 = typename example1::car;          // T
using example3 = typename example1::cdr;          // (T T)
```

evalは型Expと型Envをとる構造体として表現できる。

```c++
template<typename Exp, typename Env>
struct Eval {
  // シンボルはAssocで環境から値を取り出す
  using value = typename Assoc<Exp, Env>::value::cdr;
};
template<typename T1, typename T2, typename Env>
struct Eval<Cons<T1, T2>, Env> {
  // 複合式はEvComに任せる
  using value = typename EvCom<T1, T2, Env>::value;
};

// Assocは読者の練習問題とする。
// ヒント: 空リスト、キーが一致、キーが不一致の3通りの場合分けをすればよい

template<typename Fn, typename Arg, typename Env>
struct EvCom {
  // 基本的にApplyを呼ぶ
  using value = typename Apply<
    Fn, typename Evlis<Arg, Env>::value, Env>::value;
};
template<typename Arg, typename Env>
struct EvCom<QUOTE, Arg, Env> {
  // QUOTEの場合第一引数をそのまま返す
  using value = typename Arg::car;
};
// IFの場合...
<template ...> struct EvCom<IF, Arg, Env> { ... };
// LAMBDAの場合...
<template ...> struct EvCom<LAMBDA, Arg, Env> { ... };
```

この調子で続きを書いていけばLISPをつくることができる。
文法こそ奇妙だが、基本的にはPrologでLISPをつくるのと大差ない。

ただし、このLISPには問題がある。
まず、中身が等しいコンスを区別することができない。
C++テンプレートでは中身が同じものは等しいと判断されてしまうからだ。
それから、変数への（再）代入ができない。
C++テンプレートには副作用がないからだ。
そしてなによりも、簡単すぎて面白くない。
これは最大の問題だろう。

## 不純なLISPをつくる

C++テンプレートには副作用がない。
この純粋な世界の上で副作用のある不純なLISPの世界をつくりたい。
そこで型をつかって「メモリ」を表現する。
この「メモリ」は「『アドレス』と『値』のペア」の集合とする。
実装上は連想リストを使う。
「アドレス」は整数で表現する。
これまで説明していなかったが、C++のテンプレートは型だけではなく整数も使える。

```c++
// （コンパイル時に）整数をもつ構造体
template<int i>
struct Int {
  static constexpr int value = i;
};

// メモリ: 次のアドレスと連想リストをもつ
template<typename NextId, typename Alist>
struct Memory {
  using nextId = NextId;
  using alist = Alist;
};

// メモリの初期状態
using mem0 = Memory<Int<0>, NIL>;
```

コンスセルは変更可能 (mutable) なものと変更不可能 (immutable) なものを分ける。
評価する式やメモリを表現する連想リストは変更不可能なものを使い、
環境や関数CONSが作り出すものは変更可能なものを使う。
変更不可能なコンスは従来通り構造体`Cons<A, D>`を使って表現する。
変更可能なコンスはメモリへの参照とする。
その実態はメモリ上に置かれた変更不可能なコンスだ。

```c++
// 変更可能なコンスへの参照
template<typename Id>  // Idはアドレス
struct ConsRef {
  using id = Id;
};

// 新しく変更可能なコンスを作り出す
template<typename Car, typename Cdr, typename Mem>
struct MCons {
private:
  using currId = typename Mem::nextId;
  using nextId = Int<currId::value + 1>;
public:
  // 新しい変更可能なコンス
  using value = ConsRef<currId>;
  // メモリの次の状態
  using memory = Memory<nextId, Cons<Cons<currId, Cons<Car, Cdr>>, MemA<Mem>>>;
};

// MCar, MCdrは読者の練習問題とする
// ヒント: メモリは連想リストなのでAssocが使える
```

副作用のないC++テンプレートの世界で「メモリ」を書き換えることはできないので、
変更可能なコンスを作るとき（や書き換えるとき）は
現在のメモリをもとに新しいメモリを作り出す。
具体的には連想リストの先頭に要素を追加する。

さて、これで不純なEvalを書く準備はできた。
現在のメモリを受け取り、評価後のメモリを返せばよい。

```c++
template<typename Exp, typename Env, typename Mem>
struct Eval {
  using value = ...;   // 式の値
  using memory = ...;  // メモリの次の状態
};

void test() {
  // 使用例: メモリを引き回していることに注意
  // (defun gen (x) (lambda (y) (setq x (+ x y))))
  using ret1 = Eval<
    List<DEFUN, GEN, List<X>,
      List<LAMBDA, List<Y>, List<SETQ, X, List<ADD, X, Y>>>>,
    InitMemory::env, InitMemory::memory>;
  // (setq x (gen 100))
  using ret2 = Eval<List<SETQ, X, List<GEN, Int<100>>>,
    InitMemory::env, ret1::memory>;
  // (x 10)
  using ret3 = Eval<List<X, Int<10>>,
    InitMemory::env, ret2::memory>;
  // (x 90)
  using ret4 = Eval<List<X, Int<90>>,
    InitMemory::env, ret3::memory>;
  ...
}
```

詳細は複雑な割に面白くないので省略する。
とにかくこれで、それなりに面白いLISPになった。

## おわりに

そんなわけで、C++のテンプレートでLISPを作った（1年半前に）。
直接`LIST<CAR, LIST<QUOTE, LIST<A, B, C>>>`
のようなコードを書くのは少し面倒だが、
S式から変換するプログラムは
[簡単に書ける](https://github.com/zick/TempLisp/blob/master/templisp.lisp)
ので、手軽に色々と遊ぶことができる。
驚くべきことに、超循環評価器を動かすこともできる。
これがコンパイル時に動くのだから面白い。

*2022-09-06*
