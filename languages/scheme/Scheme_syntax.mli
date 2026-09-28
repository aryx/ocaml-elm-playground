(* Scheme_syntax: from what was read to code, the special forms
   checked and most of them rewritten into a few.

   Scheme has seven forms the machine knows (Scheme.mli's [desc]):
   quote, a variable, lambda, if, set!, a call, and begin -- plus
   define at the top. Everything else is *derived* (R5RS, section 7.3,
   defines them this way), rewritten here before running:

       (let ((x 1)) body)      ((lambda (x) body) 1)
       (let* ((x 1) (y x)) b)  (let ((x 1)) (let ((y x)) b))
       (letrec ((f e)) b)      ((lambda () (define f e) b))
       (let loop ((i 0)) b)    ((letrec ((loop (lambda (i) b))) loop) 0)
       (and a b)               (if a b #f)
       (or a b)                (let ((t a)) (if t t b))
       (cond [q a] [else b])   (if q a b)
       (when c a b)            (if c (begin a b) (void))
       `(a ,b ,@c)             (cons 'a (cons b (append c '())))

   and a body's defines become variables of the body, set in turn, as
   letrec* does. The hidden variable of or is named " t", with a space,
   which no program can write: the rewriting's hygiene, cheaply.

   An error says which form and what it expected, DrScheme's way, and
   carries the span of the text at fault, which DrScheme paints pink:

       (define (f) )   define: expected an expression for the
                       function's body, but nothing's there *)

exception Error of string * Sexpr.span

(* [top x]: a form of the Definitions window or the prompt: a
   definition, define-struct, or an expression *)
val top : Sexpr.t -> Scheme.expr

(* [datum x]: the value of (quote x): a symbol, a list... *)
val datum : Sexpr.t -> Scheme.t
