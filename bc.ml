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

type codeReturn =
	| Normal
	| Brake
	| Continue
	| Return of expr (* Maybe not expr *)

type block = statement list

type env = N of float (* complete *)

type envQueue = env list

(* Functions for running *)

let varEval (_v: string) (_q:envQueue): float  = 0.0

let evalExpr (_e: expr) (_q:envQueue): float  = 0.0

(* Test for expression *)
let%expect_test "evalNum" =
	evalExpr (Num 10.0) [] |>
	printf "%F";
	[%expect {| 10. |}]

let runCode (_code: block): unit =
	evalCode _code []

let evalCode (_code: block) (_q:envQueue): unit =
	(* crate new environment *)
	(* user fold_left  *)
	(* pop the local environment *)
	print_endline "Not implemented"

let evalStatement (s: statement) (q:envQueue): envQueue =
	match s with
		| Assign(_v, _e) -> (* eval e and store in v *) q
		| If(e, codeT, codeF) ->
			let cond = evalExpr e q in
				if(cond>0.0) then
					evalCode codeT q
				else
					evalCode codeF q
			;q
		| _ -> q (*ignore *)

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
			Expr(Op1("++a", Var("i"))),
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
				Op2("/", Num(12), Num(2))
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
		Expr(
			Op2("||",
				Op2("*",
					Assign("a", Num(.4)),
					2
				),
				Op2("/", Num(1.2), Num(.4))
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
			Expr(Num(0.)),
			[
				Expr(Num(2.))
			],
			[
				If(
					Expr(Num(1.)),
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
			Num(1),
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
					Expr(Var("y"))
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
			["x", "y"],
			[
				Return(Op2("*", Var("y"), Var("x")))
			]
		);
		Expr(Fct("a", [Num(2.), Num(2.)]))
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
								Op2("-", Var(x), Num(1.))
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
