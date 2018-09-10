type id = string
datatype binop = Plus | Minus | Times | Div
datatype stm = CompoundStm of stm * stm
             | AssignStm of id * exp
			 | PrintStm of exp list

    and exp = IdExp of id
	        | NumExp of int
			| OpExp of exp * binop * exp
			| EseqExp of stm * exp
val prog = 
    CompoundStm(AssignStm("a", OpExp(NumExp 5, Plus, NumExp 3)),
	    CompoundStm(AssignStm("b", 
		    EseqExp(PrintStm[IdExp"a", OpExp(IdExp"a", Minus, NumExp 1)],
			OpExp(NumExp 10, Times, IdExp"a"))),
		PrintStm[IdExp "b"]));

fun maxargs (CompoundStm (stm, stm2)) = Int.max(maxargs(stm), maxargs(stm2))
    | maxargs (AssignStm(_, exp)) = maxExpArgs(exp)
	| maxargs (PrintStm(lst)) = Int.max(length(lst), maxExpList(lst))

	and 
	maxExpArgs (EseqExp(stm, exp)) = Int.max(maxargs(stm), maxExpArgs(exp))
		| maxExpArgs (OpExp(left, _ , right)) = Int.max(maxExpArgs(left), maxExpArgs(right))
		| maxExpArgs (_) = 0

    and 
	maxExpList ([]) = 0
		| maxExpList (x::xs) = Int.max(maxExpArgs(x), maxExpList(xs))

type table = (id * int) list
(* datatype table = Table of (id * int) list *)

fun update (table, id, newVal) = (id, newVal) :: table

fun print_op (b:binop) : unit = 
    case b of 
	Plus => print "+"
      | Minus => print "-"
      | Times => print "*"
      | Div => print "/"

fun print_exp (e:exp) : unit =
    case e of
	IdExp id => print id
      | NumExp n => print (Int.toString n)
      | OpExp (e1,b,e2) => (print_exp e1; print_op b; print_exp e2)
      | EseqExp (_,e1) => print_exp e1

fun lookup (table, id) =  
    case table of 
	    [] => 0
		| (identifier, value) :: xs => 
		    if identifier = id then value else lookup (xs, id)

fun interpStm (stm, table) : table = 
    case stm of 
	    CompoundStm (stm1, stms) => interpStm(stms, (interpStm(stm1, table)))
		| AssignStm (id, exp) => 
		        let 
			        val (value, newTable) = interpExp(exp, table)
			    in 
				    update(newTable, id, value)
				end
		| PrintStm (exps) => 
		    case exps of 
			    [] => table
				| x::xs => 
					let 
						val (_, newTable) = (print_exp x; interpExp (x,table))
					in
						interpStm (PrintStm xs, newTable)
					end
and interpExp (exp, table) : int * table = 
    case exp of 
	    IdExp id => (lookup(table, id), table)
		| NumExp num => (num, table)
		| OpExp (leftExp, operator, rightExp) => 
		    let 
			    val (leftVal, leftTable) = interpExp(leftExp, table)
				val (rightVal, rightTable) = interpExp(rightExp, leftTable)
			in 
			    case operator of 
				    Plus => (leftVal + rightVal, rightTable)
					| Minus => (leftVal - rightVal, rightTable)
					| Times => (leftVal * rightVal, rightTable)
					| Div => (leftVal div rightVal, rightTable)
			end
		| EseqExp (stm, exp) => 
		    let 
			    val newTable = interpStm(stm, table)
			in 
			    interpExp(exp, newTable)
			end