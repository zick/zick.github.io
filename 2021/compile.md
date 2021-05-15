# lambdaを返すだけの雑なコンパイルをC++で試す

「コンパイル」と聞いて想像するものは人によって異なるが、LISPの世界には単に
「lambdaを返すだけ」のテクニックが存在する。以下のコードは簡単なmetacircular
*interpreter*だ。

```lisp
(defun eval% (exp env)
  (cond ((self-evaluating? exp)
         exp)
        ((symbolp exp)
         (lookup-value exp env))
        ...
        ((eq (car exp) 'if)
         (if (eval% (car exp) env)
             (eval% (cadr exp) env)
             (eval% (caddr exp) env)))
        ...))
```

`eval%`は式と環境を受け取り、値を返す。
`(eval% '(car '(a b c)) global-env)` のように使うことができる。
これを「コンパイラに書き換えろ」と聞くと大変な作業に聞こえるが、
すごく簡単にコンパイラにする方法がある。

```lisp
(defun compile% (exp)
  (cond ((self-evaluating? exp)
         (lambda (env) exp))
        ((symbolp exp)
         (lambda (env) (lookup-value exp env)))
        ...
        ((eq (car exp) 'if)
         (let ((cc (compile% (car exp)))
               (ct (compile% (cadr exp)))
               (ce (compile% (caddr exp))))
           (lambda (env)
             (if (funcall cc env)
                 (funcall ct env)
                 (funcall ce env)))))
        ...))
```

`compile%`は式を受け取ると「環境を受け取り値を返す関数」を返す。
`(funcall (compile% '(car '(a b c))) global-env)` のように使うことができる。
`eval%`と比べると、直接値を返していたところをlambdaでラッピングするようになり、
再帰的に`eval%`を呼び出していたところを`compile%`を呼び出すようにしただけだ。
これを本当に「コンパイル」と呼んでいいのかと思う人もいるかも知れないが、
少なくても実行速度の向上は期待できる。例えば `(self-evaluating? exp)` が真
になるケースでは、 `eval%` の場合は実行時に `self-evaluating?` が呼び出され、
その後に `exp` を返すが、 `compile%` の場合は**コンパイル時に**
`self-evaluating?` が呼び出され、実行時には比較命令は実行されない。
`if` などのスペシャルフォームに関しては、 `eval%` の場合は実行時に式の構造を
たどっていくが、 `compile%` の場合は**コンパイル時**に式をたどり、実行時には
単に必要な命令を実行するだけだ。すごく雑に言えば、いくらかの比較命令と
ポインタのデリファレンスをコンパイル時に行い、
実行時のコストを節約することができる。

LISPの世界ではたまに見かけるテクニックだが、名称がわからないので、
ここでは「雑なコンパイル」と呼ぶことにする。
この雑なコンパイルはLISP以外の言語でも関数がfirst classの言語なら実装できる。
ただ、関数がfirst classなのか非常に微妙なC++だとどうなるのだろうと
気になってしまったので試してみた。
これのためだけにLISPインタプリタとコンパイラを書く元気はないので、
[Ring Lisp](https://github.com/zick/RingLisp)を改造することにした。
Ring Lispはリングバッファ特有の面倒な話があるが、
ヒープサイズを十分に大きくしてやれば何も問題は起きない。

こちらがもともとのインタプリタのコード。
所々変なものがあるが、基本的には上記の `eval%` と同じだ。

```c++
uintptr_t eval(uintptr_t obj, uintptr_t env) {
 eval:  // eval(obj, env)
  if (isType(obj, Type::kNil) || isType(obj, Type::kErr) ||
      isType(obj, Type::kStl) || isType(obj, Type::kSbr) || isFnum(obj)) {
    return obj;
  } else if (isType(obj, Type::kSym)) {
    uintptr_t bind = findVar(obj, env);
    if (bind == nil) {
      return makeError(*toData(obj)->data.str + " has no value");
    }
    return toCons(bind)->cdr;
  }
  ...
  if (op == sym_if) {
    uintptr_t c = eval(safeCar(args), env);
    RETURN_IF_ERROR(c);
    RETURN_IF_STALE(c);
    if (c == nil) {
      // Call eval(obj, env)
      obj = safeCar(safeCdr(safeCdr(args)));
      goto eval;
    } else {
      // Call eval(obj, env)
      obj = safeCar(safeCdr(args));
      goto eval;
    }
  }
  ...
}
```

そしてこちらが新たに書いたコンパイラのコード。
所々変なものがあるが、基本的には上記の `compile%` と同じだ。

```c++
using compiled_t = std::function<uintptr_t(uintptr_t)>;
compiled_t compileEval(uintptr_t obj) {
  if (isType(obj, Type::kNil) || isType(obj, Type::kErr) ||
      isType(obj, Type::kStl) || isType(obj, Type::kSbr) || isFnum(obj) ||
      isType(obj, Type::kCfn)) {
    return [obj](uintptr_t env) { return obj; };
  } else if (isType(obj, Type::kSym)) {
    return [obj](uintptr_t env) {
      uintptr_t bind = findVar(obj, env);
      if (bind == nil) {
        return makeError(*toData(obj)->data.str + " has no value");
      }
      return toCons(bind)->cdr;
    };
  }
  ...
  if (op == sym_if) {
    compiled_t cc = compileEval(safeCar(args));
    compiled_t ct = compileEval(safeCar(safeCdr(args)));
    compiled_t ce = compileEval(safeCar(safeCdr(safeCdr(args))));
    return [cc, ct, ce](uintptr_t env) {
      uintptr_t c = cc(env);
      RETURN_IF_ERROR(c);
      RETURN_IF_STALE(c);
      if (c == nil) {
        return ce(env);
      } else {
        return ct(env);
      }
    };
  }
  ...
}
```

見ての通り、非常に機械的にコードを変換できる。
ほとんど頭を使うことなく雑に書くことができた。
単純ミスによるコンパイルエラーは何箇所があったが、
(C++の) コンパイルさえ通ってしまえば驚くべきことに一発で動いた。

せっかくなので速度を比較してみた。
ベンチマークは1から100000の総和。3回実行してその中央値をとった。
トータルの実行時間なので `compileEval` はコンパイルの時間も含む。

* eval: `1.50s user 0.02s system 99% cpu 1.518 total`
* compileEval: `0.93s user 0.02s system 99% cpu 0.953 total`

だいたい1.6倍ほど速くなった。頭を使わず書いたコードでこの結果は素晴らしい。
もっとも、ごみ集めや末尾再帰などの難しい問題を一切考慮していないし、
`std::function` のサイズやコピーのコストにも目をつぶっているのだが。
そういった難しいことを考えるのは面倒なので読者の練習問題とする。

*2021-05-15*
