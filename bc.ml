open Core

(* AST description *)
type sExpr =
	| Atom of string
	| List of sExpr list

type expr =
	| Num of float
	| Var of string
	| Op1 of string*expr
	| Op2 of string*expr*expr
	| Fct of string*expr list

type statement =
	| Assign of string*expr
	| Return of expr
	| Break
	| Continue
	| Expr of expr
	| If of expr*statement list*statement list
	| While of expr*statement list
	| For of statement*expr*statement*statement list
	| FctDef of string*string list*statement list

type block = statement list

(* Enviroment and scope *)

module HashMap = Map.Make(String)

(* type map = HashMap *)

(* May hold last statmenTresult here *)
(* type env = N of float {varMap: 'a Map.Make(String).t; fctMap: 'a Map.Make(String).t} *)

(* # Map.empty ;;
- : ('a, 'cmp) Core.Map.comparator -> ('a, 'b, 'cmp) Core.Map.t = <fun> *)

(* (Core.String.t, float, Core.String.comparator_witness) Core.Map.t *)

(* (Core.String.t, 'a, Core.String.comparator_witness) Core.Map.t, *)

type env = {varMap: (Core.String.t, float, HashMap.Key.comparator_witness) Core.Map.t; fctMap: (Core.String.t, float, HashMap.Key.comparator_witness) Core.Map.t}

type envQueue = env list

(* let newScope (q: envQueue): envQueue =
	let newEnv: env String = {varMap = HashMap.empty; fctMap = HashMap.empty}
	[]::q *)

let getVar (v: string) (q: envQueue): float = 0.0
let setVar (v: string) (f: float) (q: envQueue): envQueue = q

let runFct (v: string) (a: float list) (q: envQueue): float = 0.0
let setFct (v: string) (p: string list) (s: block) (q: envQueue): envQueue = q

let newScope (q: envQueue): envQueue = {varMap = HashMap.empty; fctMap = HashMap.empty}::q

let removeScope (q: envQueue): envQueue = q

(* Results *)
type exprResult =
	| Ok of float
	| Error of string

type statementResult =
	| Normal of envQueue
	| Brake
	| Continue
	| Return of expr
	| Expr of exprResult
	| Error of string

(* Expr evaluators *)

let evalVar (v: string) (q: envQueue): exprResult = Ok(getVar v q)

(* let op1VarEval (s: string) (v: string) (q: envQueue): exprResult =
	let f = (getVar v q) in
	match s with
		| "++x" ->
			let r = setVar v (f +. 1.) q in
			Ok((f +. 1.), r)
		| "--x" -> (
			setVar v (f -. 1.) q;
			Ok(f -. 1.)
		)
		| "x++" -> (
			setVar v (f +. 1.) q;
			Ok(f)
		)
		| "x--" ->
			setVar v (f -. 1.) q;
			Ok(f)
		| "!" ->
			if (f = 0.) then
				Ok(1.)
			else
				Ok(0.)
		| _ -> Error("Invalid operator") *)

(* let%test "preInc" = let q: (newScope []) in op1VarEval "++x" "s" q = Ok(1., (setVar "s" 1. q))
let%test "preDec" = op1VarEval "--x" "s" (newScope []) = Ok(-1.)
let%test "postInc" = op1VarEval "x++" "s" (newScope []) = Ok(0.)
let%test "postDec" = op1VarEval "x--" "s" (newScope []) = Ok(0.)
let%test "notVar" = op1VarEval "!" "s" (newScope []) = Ok(1.)
let%test "invalid" = op1VarEval "+" "s" (newScope []) = Error("Invalid operator") *)

let op2Eval (s: string) (op0: float) (op1: float): exprResult =
	match s with
		| "+" -> Ok(op0 +. op1)
		| "-" -> Ok(op0 -. op1)
		| "*" -> Ok(op0 *. op1)
		| "/" ->
			if (op1 = 0.) then
				Error("Divide by zero")
			else
				Ok(op0 /. op1)
		| "^" -> Ok(op0 ** op1)
		| "<" ->
			if (op0 < op1) then
				Ok(1.)
			else
				Ok(0.)
		| "<=" ->
			if (op0 <= op1) then
				Ok(1.)
			else
				Ok(0.)
		| ">" ->
			if (op0 > op1) then
				Ok(1.)
			else
				Ok(0.)
		| ">=" ->
			if (op0 >= op1) then
				Ok(1.)
			else
				Ok(0.)
		| "==" ->
			if (op0 = op1) then
				Ok(1.)
			else
				Ok(0.)
		| "!=" ->
			if (op0 <> op1) then
				Ok(1.)
			else
				Ok(0.)
		| "&&" ->
			if (op0 = 0. || op1 = 0.) then
				Ok(0.)
			else
				Ok(1.)
		| "||" ->
			if (op0 <> 0. || op1 <> 0.) then
				Ok(1.)
			else
				Ok(0.)
		| _ -> Error("Invalid operator")
;;

let%test "add" = op2Eval "+" 2. 3. = Ok(5.)
let%test "subtract" = op2Eval "-" 2. 3. = Ok(-1.)
let%test "multiply" = op2Eval "*" 2. 3. = Ok(6.)
let%test "divide" = op2Eval "/" 6. 3. = Ok(2.)
let%test "divideZero" = op2Eval "/" 2. 0. = Error("Divide by zero")
let%test "exponent" = op2Eval "^" 2. 2. = Ok(4.)
let%test "less" = op2Eval "<" 2. 3. = Ok(1.)
let%test "lessEqual" = op2Eval "<=" 3.1 3. = Ok(0.)
let%test "greater" = op2Eval ">" 2. 3. = Ok(0.)
let%test "greaterEqual" = op2Eval ">=" 3. 3. = Ok(1.)
let%test "equal" = op2Eval "==" 2. 3. = Ok(0.)
let%test "notEqual" = op2Eval "!=" 2. 3. = Ok(1.)
let%test "and" = op2Eval "&&" 0. 3. = Ok(0.)
let%test "or" = op2Eval "||" 2. 0. = Ok(1.)
let%test "invalid" = op2Eval "**" 2. 3. = Error("Invalid operator")

let rec evalExpr (e: expr) (q: envQueue): statementResult =
	match e with
		| Num(f) -> Ok(f)
		| Var(s) -> evalVar s q
		| Op1(s, exp) -> (
			match exp with
				| Var(v) -> (
					let f = (getVar v q) in
						match s with
							| "++x" ->
								let r = setVar v (f +. 1.) q in
								Ok(f +. 1.) (* Must return r *)
							| "--x" ->
								let r =  setVar v (f -. 1.) q in
								Ok(f -. 1.) (* Must return r *)
							| "x++" ->
								let r =  setVar v (f +. 1.) q in
								Ok(f) (* Must return r *)
							| "x--" ->
								let r =  setVar v (f -. 1.) q in
								Ok(f) (* Must return r *)
							| "!" ->
								if (f = 0.) then
									Ok(1.)
								else
									Ok(0.)
							| _ -> Error("Invalid operator")
				)
				| Num(f) ->
					if (f = 0.) then
						Ok(1.)
					else
						Ok(0.)
				| _ -> Error("Invalid operator")
		)
		| Op2(s, exp0, exp1) -> (
			match ((evalExpr exp0 q), (evalExpr exp1 q)) with
				| (Ok(f0), Ok(f1)) -> (op2Eval s f0 f1)
				| _ -> Error("Invalid operator")
			)
		| Fct(s, expL) -> Error("Invalid operator")

(* Test for expression *)
let%test "evalNum" =
	evalExpr (Num 10.0) [] = Ok(10.)

(* Stament evaluators *)

let evalCode (_code: block) (_q:envQueue): statementResult =
	(* crate new environment *)
	(* user fold_left on evalStament and q, outputs q *)
	(* pop the local environment *)
	print_endline "Not implemented";
	Error("Not implemented")

let evalStatement (s: statement) (q:envQueue): envQueue =
	(* Could have a check here that finds breaks, returns, etc, that just returns the state *)
	match s with
	| Assign(v, e) -> (
		let f = evalExpr e q in
			match f with
				| Expr(e) -> setVar v q f (* This needs to work only if exprResult doesn't have errors *)
				|
	)
	| Return(e) -> q
	| Break -> q
	| Continue -> q
	| Expr(e) -> evalExpr e q
	| If(e, codeT, codeF) ->(
			match (evalExpr e q) with
			| Ok(f) -> (
				(* if (f > 0.0) then
					evalCode codeT q
				else
					evalCode codeF q
				; *)q)
			| Error(s) -> (* Error(s) *) q
		)
	| While(e,b) -> q
	| For(a, e, i, b) -> q
	| FctDef(s, sL, b) -> q

	let runCode (code: block): unit =
		(* evalCode code []; *)
		()
	;;

(* Intergration testing ---------------------------------------------------- *)

(*
	v = 10;
	v // display v
*)
let p1: block = [
		Assign("v", Num(1.0));
		Expr(Var("v"))
]

let%expect_test "p1" =
	runCode p1;
	[%expect {| 1. |}]

(*
	v = 1.0;
	if (v>10.0) then
		v = v + 1.0
	else
		for(i=2.0; i<10.0; i++) {
			v = v * i
		}
	v   // display v
*)
let p2: block = [
	Assign("v", Num(1.0));
	If(
		Op2(">", Var("v"), Num(10.0)),
		[Assign("v", Op2("+", Var("v"), Num(1.0)))],
		[For(
			Assign("i", Num(2.0)),
			Op2("<", Var("i"), Num(10.0)),
			Expr(Op1("x++", Var("i"))),
			[
				Assign("v", Op2("*", Var("v"), Var("i")))
			]
		)]
	);
	Expr(Var("v"))
]

let%expect_test "p2" =
	runCode p2;
	[%expect {| 3628800. |}]

(* TODO: Broken? *)
(*  Fibbonaci sequence
	define f(x) {
		if (x<1.0) then
			return (1.0)
		else
			return (f(x-1)+f(x-2))
	}

	f(3)
	f(5)
*)
let p3: block =
	[
		FctDef("f", ["x"], [
			If(
				Op2("<", Var("x"), Num(1.0)),
				[Return(Num(1.0))],
				[Return(Op2("+",
					Fct("f", [Op2("-", Var("x"), Num(1.0))]),
					Fct("f", [Op2("-", Var("x"), Num(1.0))])
				))])
		]);
		Expr(Fct("f", [Num(3.0)]));
		Expr(Fct("f", [Num(5.0)]))
	]

let%expect_test "p3" =
	runCode p3;
	[%expect {|
		2.
		5.
	|}]

(*
	!(!(!(!(1)))) || (12/2)
*)
let p4: block =
	[
		Expr(
			Op2("||",
				Op1("!",
					Op1("!",
						Op1("!",
							Op1("!", Num(1.))
						)
					)
				),
				Op2("/", Num(12.), Num(2.))
			)
		)
	]

let%expect_test "p4" =
	runCode p4;
	[%expect {|
		1.
	|}]

(*
	(a = .4) * 2 || (1.2 / .4)
*)
let p5: block =
	[
		Assign("a", Num(0.4));
		Expr(
			Op2("||",
				Op2("*", Var("a"), Num(2.)),
				Op2("/", Num(1.2), Num(0.4))
			)
		)
	]

let%expect_test "p5" =
	runCode p5;
	[%expect {|
		1.
	|}]

(*
	z=1;
	while (z) z--;
*)
let p6: block =
	[
		Assign("z", Num(1.));
		While(Var("z"),
		[
			Expr(Op1("x--", Var("z")))
		])
	]

let%expect_test "p6" =
	runCode p6;
	[%expect {|
		1.
	|}]

(*
	y = 0;
	while (y <= 5) {
		++y;
		continue;
		break;
		y = 3;
		y;
	}
*)
let p7: block =
	[
		Assign("y", Num(0.));
		While(
			Op2("<=", Var("y"), Num(5.)),
			[
				Expr(Op1("++x", Var("y")));
				Continue;
				Break;
				Assign("y", Num(3.));
				Expr(Var("y"))
			])
	]

let%expect_test "p7" =
	runCode p7;
	[%expect {|
		1.
		2.
		3.
		4.
		5.
		6.
	|}]

(*
	for (i=5; i > 0; i--) {
		5
	}
*)
let p8: block =
	[
		For(
			Assign("i", Num(5.)),
			Op2(">", Var("i"), Num(0.)),
			Expr(Op1("x--", Var("i"))),
			[
				Expr(Num(5.))
			]
		)
	]

let%expect_test "p8" =
	runCode p8;
	[%expect {|
		5.
		5.
		5.
		5.
		5.
	|}]

(*
	for (i=0; i < 5; i++) {
		i
		break;
		i+2
	};
*)
let p9: block =
	[
		For(
			Assign("i", Num(0.)),
			Op2("<", Var("i"), Num(5.)),
			Expr(Op1("x++", Var("i"))),
			[
				Expr(Var("i"));
				Break;
				Expr(Op2("+", Var("i"), Num(2.)))
			]
		)
	]

let%expect_test "p9" =
	runCode p9;
	[%expect {|
		0.
	|}]

(*
	if (0) {
		2;
	} else if (1) {
		3
	} else 1
*)
let p10: block =
	[
		If(
			Num(0.),
			[
				Expr(Num(2.))
			],
			[
				If(
					Num(1.),
					[
						Expr(Num(3.))
					],
					[
						Expr(Num(1.))
					]
				)
			]
		)
	]

let%expect_test "p10" =
	runCode p10;
	[%expect {|
		3.
	|}]

(*
	if (1) 2
*)
let p11: block =
	[
		If(
			Num(1.),
			[
				Expr(Num(2.))
			],
			[

			]
		)
	]

let%expect_test "p11" =
	runCode p11;
	[%expect {|
		2.
	|}]

(*
	define f(){
		return (y)
	}

	define g(y){
		y = 1 + 4;
		return (f())
	}
	g(2)
*)
let p12: block =
	[
		FctDef(
			"f",
			[],
			[
				Return(
					Var("y")
				)
			]
		);
		FctDef(
			"g",
			["y"],
			[
				Assign(
					"y",
					Op2("+", Num(1.), Num(4.))
				);
				Return(
					Fct("f", [])
				)
			]
		);
		Expr(Fct("g", [Num(2.)]))
	]

let%expect_test "p12" =
	runCode p12;
	[%expect {|
		5.
	|}]

(*
	define a (x, y) {
		return (y*x);
	}
	a(2,2)
*)
let p13: block =
	[
		FctDef(
			"a",
			["x"; "y"],
			[
				Return(Op2("*", Var("y"), Var("x")))
			]
		);
		Expr(Fct("a", [Num(2.); Num(2.)]))
	]

let%expect_test "p13" =
	runCode p13;
	[%expect {|
		4.
	|}]

(*
	define f(x) {
		if (x <= 1) {
			return (1);
		} else {
			return (x * f(x-1));
		}
	}

	f(13)
*)
let p14: block =
	[
		FctDef(
			"f",
			["x"],
			[
				If(
					Op2("<=", Var("x"), Num(1.)),
					[
						Return(Num(1.))
					],
					[
						Return(
							Op2("*", Var("x"), Fct("f", [
								Op2("-", Var("x"), Num(1.))
							]))
						)
					]
				)
			]
		);
		Expr(Fct("f", [Num(13.)]))
	]

let%expect_test "p14" =
	runCode p14;
	[%expect {|
		6227020800.
	|}]
