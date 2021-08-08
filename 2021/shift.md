# shift/reset プログラミング入門の練習問題をやった

結構前（10年くらい前？）に限定継続が一部界隈で流行ったが、
私はそのとき流行りに乗りそこねてしまった。
それ以降、限定継続の話が出るたびに浅井先生の
[shift/reset プログラミング入門](http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-j.pdf)
をチラ見して、ある程度納得した後、そのまま忘れるというのを3回ほど繰り返した。
流石に記憶が定着してなさすぎるだろと反省して、手を動かして練習問題を解いた。

普段書いてないOCamlを書くのはしんどいので、Scheme (Racket) を使った。
別にSchemeも普段書いているわけではないけど。

```scheme
; 練習問題3
; shiftで「適当な値」を返せとあったのでテキトーに値を入れた
(* 5 (reset (+ (shift k 1) (* 3 4)))) ; => 5
(string-append (reset (if (shift k "hoi") "hello" "hi")) " world")  ; => "hoi world"
(car (reset (let ((x (shift k (list 2)))) (cons x x))))  ; => 2
(string-length (reset (string-append "x" (number->string (shift k "hello")))))  ; => 5

; 練習問題4
; call/ccの例題でよくあるやつ
(define (times lst)
  (cond ((null? lst) 1)
        ((= (car lst) 0) (shift k 0))
        (else (* (car lst) (times (cdr lst))))))
(reset (times '(1 2 3 0 4 5)))  ; => 0

; 練習問題5
; 型がないので継続にテキトーに値を渡した
((reset (* 5 (+ (shift k k) (* 3 4)))) 1)  ; => 65
((reset (string-append (if (shift k k) "hello" "hi") " world")) #t)  ; => "hello world"
((reset (car (let ((x (shift k k))) (cons x x)))) 1)  ; => 1
((reset (string-length (string-append "x" (number->string (shift k k))))) 42)  ; => 3

; 練習問題6
; 型がないので空リスト以外の値を渡して満足した
(define (id lst)
  (cond ((null? lst) (shift k k))
        (else (cons (car lst) (id (cdr lst))))))
((reset (id '(1 2 3))) '(4))  ; => '(1 2 3 4)

; 練習問題7
; コード量がムダに多い
; 変数 t が使えるのはSchemeの特権
(define (make-node left val right) (list left val right))
(define (node-left n) (car n))
(define (node-val n) (cadr n))
(define (node-right n) (caddr n))
(define tree1 (make-node (make-node '() 1 '()) 2 (make-node '() 3 '())))
(define tree2 (make-node '() 1 (make-node '() 2 (make-node '() 3 '()))))
(define (walk-tree t)
  (cond ((null? t) '())
        (else (walk-tree (node-left t))
              (shift k (cons (node-val t) k))
              (walk-tree (node-right t)))))
(define (start-tree t)
  (reset (walk-tree t) 'done))
(define (same-fringe? t1 t2)
  (let loop ((ret1 (start-tree t1))
             (ret2 (start-tree t2)))
    (cond ((eq? ret1 'done) (eq? ret2 'done))
          ((eq? ret2 'done) #f)
          ((not (eq? (car ret1) (car ret2))) #f)
          (else (loop ((cdr ret1) #f) ((cdr ret2) #f))))))
(same-fringe? tree1 tree2)  ; => #t

; 練習問題8
; 型付printfを考える話なのに型がない
((reset (string-append "hello " (shift k (lambda (x) (k x))) "!")) "world")  ; => "hello world!"

; 練習問題9
; これすき
(define (get)
  (shift k (lambda (state) ((k state) state))))
(define (tick)
  (shift k (lambda (state) ((k #f) (+ state 1)))))
(define (run-state thunk)
  ((reset (let ((result (thunk)))
            (lambda (state)
              result)))
   0))
; Schemeの評価順序はまあ
(run-state (lambda () (- (begin (tick) (get)) (begin (tick) (get)))))  ;=> -1

; 練習問題10
; tickより簡単だと思う
(define (put new-state)
  (shift k (lambda (state) ((k #f) new-state))))
(run-state (lambda () (tick) (tick) (put 42) (get)))  ;=> 42

; 練習問題11
; 面倒なので省略
; 読者の演習問題とする

; 練習問題12
; amb的なアレ
(define (choice lst)
  (shift k (for-each (lambda (x) (k x)) lst)))

; 練習問題13
; call/ccでよく見るやつ
(let ((x (choice '(1 2 3 4 5)))
      (y (choice '(1 2 3 4 5)))
      (z (choice '(1 2 3 4 5))))
  (if (= (+ (* x x) (* y y)) (* z z))
      (begin (display x)
             (display " ")
             (display y)
             (display " ")
             (display z)
             (newline))
      #f))
```

これで記憶があと3年位は残ってくれると嬉しいのだけれど。

*2021-08-08*
