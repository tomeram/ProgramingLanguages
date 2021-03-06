(*
  Tests for the lambda calculus parser and reducers.

  EXTEND THIS FILE TO TEST YOUR SOLUTION THOROUGHLY!
*)

open Utils
open Parser
open Reducer

let rec evaluate ~verbose reduce t =
  if verbose then print_string (format_term t) else ();
  match reduce t with
  | None ->
    if verbose then print_string " =/=>\n\n" else ();
    t
  | Some t' ->
    if verbose then print_string " ==>\n\n" else ();
    evaluate ~verbose reduce t'


let test_and_1 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and tru) tru)
"

let test_and_2 = "
let tru = (\\t.(\\f.t)) in
let fls = (\\t.(\\f.f)) in
let and = (\\b.(\\c. ((b c) fls))) in
((and fls) ((and tru) tru))
"

let env = "
let tru = (\\t. (\\f. t)) in
let fls = (\\t. (\\f. f)) in
let test = (\\l. (\\m. (\\n. ((l m) n)))) in
let and = (\\b. (\\c.  ((b c) fls))) in

let pair = (\\f. (\\s. (\\b.  ((b f) s)))) in
let fst = (\\p. (p tru)) in
let snd = (\\p. (p fls)) in

let c0 = (\\s. (\\z. z)) in
let c1 = (\\s. (\\z. (s z))) in
let c2 = (\\s. (\\z. (s (s z)))) in
let c3 = (\\s. (\\z. (s (s (s z))))) in
let c4 = (\\s. (\\z. (s (s (s (s z)))))) in
let c5 = (\\s. (\\z. (s (s (s (s (s z))))))) in
let c6 = (\\s. (\\z. (s (s (s (s (s (s z)))))))) in
let c7 = (\\s. (\\z. (s (s (s (s (s (s (s z))))))))) in
let c8 = (\\s. (\\z. (s (s (s (s (s (s (s (s z)))))))))) in
let c9 = (\\s. (\\z. (s (s (s (s (s (s (s (s (s z))))))))))) in
let c10 = (\\s. (\\z. (s (s (s (s (s (s (s (s (s (s z)))))))))))) in

let scc = (\\n. (\\s. (\\z. (s ((n s) z))))) in
let plus = (\\m. (\\n. (\\s. (\\z. ((m s) ((n s) z)))))) in
let times = (\\m. (\\n. (\\s. (m (n s))))) in
let power = (\\m. (\\n. (n m))) in
let iszero = (\\m. ((m (\\x. fls)) tru)) in
let prd = (let zz = ((pair c0) c0) in
           let ss = (\\p. ((pair (snd p)) ((plus c1) (snd p)))) in
           (\\m. (fst ((m ss) zz)))) in
let leq = (\\m. (\\n. (iszero ((n prd) m)))) in
let equal = (\\m. (\\n. ((and ((leq m) n)) ((leq n) m)))) in

let Y = (\\f. ((\\x. (f (x x))) (\\x. (f (x x))))) in
let Z = (\\f. ((\\x. (f (\\y. ((x x) y)))) (\\x. (f (\\y. ((x x) y)))))) in
"

let test_fact_l = env ^ "
let fact_l = (Y (\\f. (\\n. (((test (iszero n)) c1) (((times n) (f (prd n)))))))) in
((equal (fact_l c2)) c2)
"

let test_fact_s = env ^ "
let fact_s = (Z (\\f. (\\n. ((((test (iszero n)) (\\x. c1)) (\\x. (((times n) (f (prd n)))))) (\\x.x))))) in
((equal (fact_s c2)) c2)
"

(* Testing basic syntax *)
let simple_test1 = "
  a
"

let simple_test2 = "
  (\\a.b)
"

let simple_test3 = "
  (a b)
"

let simple_test4 = "
  (a)
"

let simple_test5 = "
  let a = b in c
"

(* multi rule tests *)
let multi_test11 = "
  let a = (\\b.c) in d
"

let multi_test12 = "
  let a = b in (\\c.d)
"

let multi_test13 = "
  let a = (\\b.c) in (\\d.e)
"

let multi_test21 = "
  let a = (b c) in d
"

let multi_test22 = "
  let a = b in (c d)
"

let multi_test23 = "
  let a = (b c) in (d e)
"

let multi_test31 = "
  let a = (\\a.c) in d
"

let multi_test32 = "
  let a = b in (\\a.a)
"

let multi_test33 = "
  let a = (\\a.b) in (\\a.a)
"

let multi_test41 = "
  ((\\a.(a b)) d)
"

let multi_test42 = "
  (d (\\a.(a b)))
"

let multi_test43 = "
  ((\\a.(a b)) (\\c.(d e)))
"

(* Testing expressions with let *)
let test_let1 = "
  let foo = (\\a.a) in
  (foo d)
"

let test_let2 = "
  let foo = (\\a.a) in
  let bar = (\\b.(foo b)) in
  (bar c)
"

let test_let3 = "
  let foo = (\\a.a) in
  let bar = (\\b.(func b)) in
  let func = (\\c.(bar c)) in
  (func c)
"

let test_let4 = "
  let foo = (\\a.(a a)) in
  (foo foo)
"

let test ~verbose ~sem ~reduce s =
  printf "\nEvaluating:\n%s\nin %s semantics:\n\n" s sem;
  let result = (evaluate ~verbose reduce (parse s)) in
  printf "Result is: %s\n\n" (format_term result)

let test_lazy = test ~sem:"lazy" ~reduce:reduce_lazy
let test_strict = test ~sem:"strict" ~reduce:reduce_strict
let test_normal = test ~sem:"normal-order" ~reduce:reduce_normal
let test_all ~verbose s =
  test_lazy ~verbose s;
  test_strict ~verbose s;
  test_normal ~verbose s


let () =
  test_all ~verbose:true test_and_1;
  test_all ~verbose:true test_and_2;

  test_lazy ~verbose:false test_fact_l;
  test_strict ~verbose:false test_fact_s;
  test_normal ~verbose:false test_fact_l;
  test_normal ~verbose:false test_fact_s;

  (* Testing basic syntax *)
  test_all ~verbose:true simple_test1;
  test_all ~verbose:true simple_test2;
  test_all ~verbose:true simple_test3;
  test_all ~verbose:true simple_test4;
  test_all ~verbose:true simple_test5;

  (* multi rule tests *)
  test_all ~verbose:true multi_test11;
  test_all ~verbose:true multi_test12;
  test_all ~verbose:true multi_test13;

  test_all ~verbose:true multi_test21;
  test_all ~verbose:true multi_test22;
  test_all ~verbose:true multi_test23;

  test_all ~verbose:true multi_test31;
  test_all ~verbose:true multi_test32;
  test_all ~verbose:true multi_test33;

  test_all ~verbose:true multi_test41;
  test_all ~verbose:true multi_test42;
  test_all ~verbose:true multi_test43;

  (* Testing expressions with let *)
  test_all ~verbose:true test_let1;
  test_all ~verbose:true test_let2;
  test_all ~verbose:true test_let3;
