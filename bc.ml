open Base
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
	| For of statement*expr*expr*statement list
	| FctDef of string*string list*statement list

type block = statement list

(* Enviroment and scope *)

type storedFct = {parameters: string list; body: statement list}

type env = {
	mutable varMap: ((string, float, String.comparator_witness) Map.t);
	mutable fctMap: ((string, storedFct, String.comparator_witness) Map.t)
	}

(* type env = N of float *)
type envQueue = env list

let newScope (q: envQueue): envQueue =
	{varMap = Map.empty (module String); fctMap = Map.empty (module String)}::q

let removeScope (q: envQueue): envQueue =
	match q with
		| _hd::tl -> tl
		| _ -> q
;;

(* Assumes q has at least one valid enviroment *)
let getVar (v: string) (q: envQueue): float =
	(* Check current scope *)
	let currentScope = List.hd_exn q in
	match Map.find currentScope.varMap v with
		| Some(f) -> f
		| None -> (
			(* Check global scope *)
			let globalScope = List.last_exn q in
			match Map.find globalScope.varMap v with
				| Some(f) -> f
				(* Otherwise 0 *)
				| None -> 0.
		)
;;

let setVar (v: string) (f: float) (q: envQueue): envQueue =
	(* Add to current scope *)
	let currentScope = List.hd_exn q in
	(* TODO Change may not work, check then add *)
	let newMap = Map.change currentScope.varMap v ~f:(fun _x -> Some(f)) in
	let newScope = {varMap = newMap; fctMap = currentScope.fctMap} in
	newScope::(List.tl_exn q)
;;

let setFct (s: string) (p: string list) (b: block) (q: envQueue): envQueue =
	let newFct = {parameters = p; body = b} in
	let currentScope = List.hd_exn q in
	(* TODO Change may not work, check then add *)
	let newMap = Map.change currentScope.fctMap s ~f:(fun _x -> Some(newFct)) in
	let newScope = {varMap = currentScope.varMap; fctMap = newMap} in
	newScope::(List.tl_exn q)
;;

(* Results *)

(* For simplicity, expr will not have error checking (just return float) *)

type statementResult =
	| Normal of envQueue
	| Continue of envQueue
	| Break of envQueue
	| Return of float

(* Expr evaluators *)

let op1VarEval (s: string) (v: string) (q: envQueue): float*envQueue =
	let f = (getVar v q) in
		match s with
			| "++x" ->
				let q = setVar v (f +. 1.) q in
				((f +. 1.), q)
			| "--x" ->
				let q = setVar v (f -. 1.) q in
				((f -. 1.), q)
			| "x++" ->
				let q = setVar v (f +. 1.) q in
				(f, q)
			| "x--" ->
				let q = setVar v (f -. 1.) q in
				(f, q)
			| "!" ->
				if (f = 0.) then
					(1., q)
				else
					(0., q)
			| _ -> (0., q) (* Error *)
;;

let op2Eval (s: string) (op0: float) (op1: float): float =
	match s with
		| "+" -> op0 +. op1
		| "-" -> op0 -. op1
		| "*" -> op0 *. op1
		| "/" ->
			if (op1 = 0.) then
				0. (* Some sort of divide by zero error *)
			else
				op0 /. op1
		| "^" -> op0 ** op1
		| "<" ->
			if (op0 < op1) then
				1.
			else
				0.
		| "<=" ->
			if (op0 <= op1) then
				1.
			else
				0.
		| ">" ->
			if (op0 > op1) then
				1.
			else
				0.
		| ">=" ->
			if (op0 >= op1) then
				1.
			else
				0.
		| "==" ->
			if (op0 = op1) then
				1.
			else
				0.
		| "!=" ->
			if (op0 <> op1) then
				1.
			else
				0.
		| "&&" ->
			if (op0 = 0. || op1 = 0.) then
				0.
			else
				1.
		| "||" ->
			if (op0 <> 0. || op1 <> 0.) then
				1.
			else
				0.
		| _ -> 0. (* Error *)
;;

let rec evalExpr (e: expr) (q: envQueue): float*envQueue =
	match e with
		| Num(f) -> (f, q)
		| Var(v) -> ((getVar v q), q)
		| Op1(s, exp) -> (
			match exp with
				| Var(v) -> op1VarEval s v q
				| Num(f) ->
					if (f = 0.) then
						(1., q)
					else
						(0., q)
				| _ -> evalExpr exp q
		)
		| Op2(s, exp0, exp1) ->
				let ((f0, _), (f1, _)) = ((evalExpr exp0 q), (evalExpr exp1 q)) in
				(op2Eval s f0 f1, q)
		| Fct(s, expL) -> (* (0., q) *)
				let fL = List.map ~f:(fun x -> match (evalExpr x q) with | (f, _) -> f) expL in
				(runFct s fL q)

and

(* Stament evaluators *)
(*
1. Check if block is done, if so return result
2. If not, check if previous result was short circuit and pass it out
3. If Normal, call eval with new state and less block
*)
evalCodeInternal (code: block) (r: statementResult): statementResult =
	match code with
		| [] -> r
		| hd::_ ->
			match r with
				| Normal(q) -> evalCodeInternal (List.tl_exn code) (evalStatement hd q)
				| Break(q) -> Break(q)
				| Continue(q) -> Continue(q)
				| Return(f) -> Return(f)

and

evalCode (code: block) (q: envQueue): statementResult =
	evalCodeInternal code (Normal(q))

and

addParamArgs (parameters: string list) (args: float list) (q: envQueue): envQueue =
	match parameters with
		| [] -> q
		| _ -> let r = evalCode [Assign((List.hd_exn parameters), Num((List.hd_exn args)))] q in
			match r with
				| Normal(q) | Continue(q) | Break(q) -> addParamArgs (List.tl_exn parameters) (List.tl_exn args) q
				| Return(_f) -> q (* Error? *)

and

(* Try recursively calling a function for this instea dof fold_left *)
runFct (v: string) (args: float list) (q: envQueue): float*envQueue =
	(* Check current scope *)
	let fct =
		let currentScope = List.hd_exn q in
		match Map.find currentScope.fctMap v with
			| Some(fct) -> fct
			| None -> (
				(* Otherwise global scope or bust *)
				let globalScope = List.last_exn q in Map.find_exn globalScope.fctMap v
			) in
	let q = newScope q in
	let q = addParamArgs fct.parameters args q in
	let r = evalCode fct.body q in
	let result = (
		match r with
			| Return(f) -> f
			| _ -> 0.
	) in
	let q = removeScope q in
	(result, q)

and

loop (e: expr) (code: block) (post: expr) (q: envQueue): statementResult =
	let (f, q) = evalExpr e q in
		if (f <> 0.) then
			let r  = evalCode code q in
				match r with
					| Normal(q) | Continue(q) -> (
							let (_f, q) = evalExpr post q in loop e code post q
								(*match r with
									| Normal(q) | Continue(q) -> loop e code post q
									| Break(q) -> Break(q)
									| Return(_f) -> Break(q) (* Error? *) *)
						)
					| Break(q) -> Break(q)
					| Return(_f) -> Break(q) (* Error? *)
		else
			Normal(q)

and

evalStatement (s: statement) (q: envQueue): statementResult =
	(* Could have a check here that finds breaks, returns, etc, that just returns the state *)
	match s with
		| Assign(v, e) ->
				let (f, q) = evalExpr e q in
					let q = setVar v f q in
						Normal(q)
		| Return(e) ->
				let (f, _q) = evalExpr e q in Return(f)
		| Break -> Break(q)
		| Continue -> Continue(q)
		| Expr(e) ->
				let (f, q) = evalExpr e q in
					print_float f;
					print_newline ();
					Normal(q)
		| If(e, codeT, codeF) ->
				let (f, q) = evalExpr e q in
					if (f <> 0.) then
						evalCode codeT q
					else
						evalCode codeF q
| While(e, b) -> loop e b (Num(0.)) q
		| For(a, e, i, b) -> (
				let r = evalCode [a] q in
				match r with
					| Normal(q) | Continue(q) -> loop e b i q
					| Break(q) -> Break(q)
					| Return(_f) -> Break(q) (* Error? *)
			)
		| FctDef(s, p, b) -> let q = setFct s p b q in Normal(q)
;;

	let runCode (code: block): unit =
		ignore (evalCode code (newScope []))
	;;

(* Test for expressions *)

(* op1VarEval Tests *)

(* let%test "preInc" = op1VarEval "++x" "s" [{varMap = Map.empty (module String); fctMap = Map.empty (module String)}] = (1., [])
let%test "preDec" = op1VarEval "--x" "s" [{varMap = Map.empty (module String); fctMap = Map.empty (module String)}] = (-1., [])
let%test "postInc" = op1VarEval "x++" "s" [{varMap = Map.empty (module String); fctMap = Map.empty (module String)}] = (0., [])
let%test "postDec" = op1VarEval "x--" "s" [{varMap = Map.empty (module String); fctMap = Map.empty (module String)}] = (0., [])
let%test "notVar" = op1VarEval "!" "s" [{varMap = Map.empty (module String); fctMap = Map.empty (module String)}] = (1., []) *)
(* let%test "invalid" = op1VarEval "+" "s" (newScope []) = Error("Invalid operator") *)

(* op2Eval Tests *)

let%test "add" = op2Eval "+" 2. 3. = 5.
let%test "subtract" = op2Eval "-" 2. 3. = -1.
let%test "multiply" = op2Eval "*" 2. 3. = 6.
let%test "divide" = op2Eval "/" 6. 3. = 2.
(* let%test "divideZero" = op2Eval "/" 2. 0. = Error("Divide by zero") *)
let%test "exponent" = op2Eval "^" 2. 2. = 4.
let%test "less" = op2Eval "<" 2. 3. = 1.
let%test "lessEqual" = op2Eval "<=" 3.1 3. = 0.
let%test "greater" = op2Eval ">" 2. 3. = 0.
let%test "greaterEqual" = op2Eval ">=" 3. 3. = 1.
let%test "equal" = op2Eval "==" 2. 3. = 0.
let%test "notEqual" = op2Eval "!=" 2. 3. = 1.
let%test "and" = op2Eval "&&" 0. 3. = 0.
let%test "or" = op2Eval "||" 2. 0. = 1.
(* let%test "invalid" = op2Eval "**" 2. 3. = Error("Invalid operator") *)

(* Generic exprestion test *)
let%test "evalNum" = evalExpr (Num 10.0) [] = (10., [])

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


(* TODO: It appears the for loop doesn't loop the last time *)
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
			Op1("x++", Var("i")),
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

(* TODO: Disect what's wrng *)
(* Fibonacci sequence
	define f(x) {
		if (x<2.0) then
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
				Op2("<", Var("x"), Num(2.0)),
				[Return(Num(1.0))],
				[Return(Op2("+",
					Fct("f", [Op2("-", Var("x"), Num(1.0))]),
					Fct("f", [Op2("-", Var("x"), Num(2.0))])
				))])
		]);
		Expr(Fct("f", [Num(3.0)]));
		Expr(Fct("f", [Num(5.0)]));
	]

let%expect_test "p3" =
	runCode p3;
	[%expect {|
		3.
		8.
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
			Op1("x--", Var("i")),
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
			Op1("x++", Var("i")),
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

(* TODO: Not sure where after the 5 is printed the 0 is comming from *)
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
			[Return(Var("y"))]
		);
		FctDef(
			"g",
			["y"],
			[
				Assign("y", Op2("+", Num(1.), Num(4.)));
				Return(Fct("f", []))
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
