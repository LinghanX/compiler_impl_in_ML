(* signature SEMANT =
sig
    type venv = Env.enventry Symbol.table 
    type tenv = ty Symbol.table 

    transVar: venv * tenv * Absyn.var -> expty 
    transExp: venv * tenv * Absyn.exp -> expty 
    transDec: venv * tenv * Absyn.dec -> { venv: venv, tenv: tenv }
    transTy:         tenv * Absyn.ty  -> Types.ty 
end *)

structure Semant = 
struct 
    structure A = Absyn
    structure S = Symbol
    val error = ErrorMsg.error 
    type expty = { exp: Translate.exp, ty: Types.ty }

    fun transExp(venv, tenv, exp) = 
        let 
            fun checkType(ty1, ty2, pos) = case ty1 of 
                Types.INT => case ty2 of Types.INT => {exp = (), ty = Types.INT} | _ => error pos ("integer required") 
                | Types.STRING => case ty2 of Types.STRING => {exp = (), ty = Types.STRING} | _ => error pos ("string required")
                | Types.ARRAY(ty) => case ty2 of Types.ARRAY(ty2) => {exp = (), ty = Types.ARRAY(ty2)} | _ => error pos ("array required")
            fun checkInt ({exp, ty}, pos) = case ty of 
                Types.INT => () | _ => error pos "integer required";
            fun findType ty = case ty of 
                Types.NAME(sym, tyop) => 
                    case !tyop of 
                        SOME(ty') => S.look(tenv, ty')
                        | NONE => error ("unknown type")
                | _ => ty 
            fun checkSameType({exp1, ty1}, {exp2, ty2}, pos) = 
                let 
                    val ty1' = findType(ty1)
                    val ty2' = findType(ty2)
                in 
                    checkType(ty1, ty2, pos)
                end 
            fun trexp (A.OpExp{left, oper, right, pos}) = 
                let in case oper of 
                    PlusOp => (checkInt(trexp left, pos) checkInt(trexp right, pos); {exp = (), ty = Types.INT})
                    | MinusOp => (checkInt(trexp left, pos) checkInt(trexp right, pos); {exp = (), ty = Types.INT})
                    | TimesOp => (checkInt(trexp left, pos) checkInt(trexp right, pos); {exp = (), ty = Types.INT})
                    | DivideOp => (checkInt(trexp left, pos) checkInt(trexp right, pos); {exp = (), ty = Types.INT})
                    | EqOp => (checkSameType(trexp left, trexp right, pos); {exp = (), ty = Types.INT}) 
                    | NeqOp => (checkSameType(trexp left, trexp right, pos); {exp = (), ty = Types.INT})
                    | LtOp => (checkSameType(trexp left, trexp right, pos); {exp = (), ty = Types.INT})
                    | LeOp => (checkSameType(trexp left, trexp right, pos); {exp = (), ty = Types.INT})
                    | GtOp => (checkSameType(trexp left, trexp right, pos); {exp = (), ty = Types.INT})
                    | GeOp => (checkSameType(trexp left, trexp right, pos); {exp = (), ty = Types.INT})
                    | _ => error pos ("error unknown")
                end 
            | trexp(A.LetExp{decs, body, pos}) = 
                let val {venv = venv', tenv = tenv'} = transDecs(venv, tenv, decs)
                in transExp(venv', tenv') body end
            | trexp (A.NilExp) = {exp = (), ty = Types.Unit}
            | trexp (A.IntExp) = {exp = (), ty = Types.INT}
            | trexp (A.StringExp) = {exp = (), ty = Types.STRING}
            | trexp (A.VarExp(var)) = trvar var
            | trexp (A.CallExp{func, args, pos}) = 
                let 
                    fun trargs (arg, args) = let val {exp = _, ty = ty'} = trexp arg in ty'::args end 
                    val args' = foldl trargs [] args 
                in 
                    {exp = (), ty = checkType(hd(args'), S.look(tenv, func))}
                end 
            | trexp (A.RecordExp{fields, typ, pos}) = 
                let 
                    val recordType = findType typ
                    fun getFieldType field{symbol, exp, pos} = 
                        let val {exp, ty'} = transExp(exp)
                        in ty' end
                    val fieldTypes = foldl getFieldType [] fields
                in
                    checkType(hd(fieldTypes), findType(typ))
                end 
            | trexp (A.SeqExp((exp, pos)::rst)) = (* to be implemented *)
            | trexp (A.AssignExp{var, exp, pos}) = {exp = (), ty = Types.UNIT}
            | trexp (A.IfExp(test, then', else', pos)) = 
                let 
                    val {_, ty = testType} = transExp test 
                    val {_, ty = thenType} = transExp then'
                    val {_, ty = elseType} = transExp else'
                in 
                    (checkType(testType, Types.INT, pos)(checkType(thenType, elseType, pos)))
                end
            | trexp (A.WhileExp{test, body, pos}) = 
                let 
                    val {_, ty = testType} = transExp test 
                    val {_, ty = bodyType} = transExp exp 
                in 
                    (checkType(test, Types.INT, pos)
                    {exp = (), ty = Types.UNIT})
                end
            | trexp (A.BreakExp(pos)) = {exp = (), ty = Types.UNIT}
            | trexp (A.ArrayExp{typ, size, init, pos}) = 
                let 
                    val sizeType = transExp size
                    val ty' = findType typ 
                    val initType = transExp init 
                in 
                    (checkType(sizeType, Types.INT, pos)
                    {exp = (), ty = ty'})
                end 
            and trvar (A.SimpleVar(id, pos)) = 
                (case Symbol.look(venv, id)
                    of SOME(E.VarEntry{ty}) => {exp=(), ty = actual_ty ty}
                    | NONE => (error pos ("undefined variable " ^ S.name id);
                    {exp = (), ty = Types.INT}))
                (* | trvar (A.FieldVar(v, id, pos)) = ... *)
        in 
            trexp exp
        end 
    
    fun transDec (venv, tenv, A.VarDec{name, typ = NONE, init, ...}) = 
            let 
                val {exp, ty} = transExp (venv, tenv, init)
            in 
                {tenv = tenv, venv = S.enter(venv, name, E.VarEntry{ty = ty})}
            end
        | transDec (venv, tenv, A.TypeDec[{name, ty}]) = 
            {venv = venv, tenv = S.enter(tenv, name, transTy(tenv, ty))}
        | transDec (venv, tenv, A.FunctionDec[{name, params, body, pos, result = SOME(rt, pos)}]) = 
            let 
                val SOME(result_ty) = S.look(tenv, rt)
                fun transparam{name, typ, pos} = 
                    case S.look(tenv, typ) of SOME t => {name=name, ty = t}
                val params' = map transparam params
                val venv' = S.enter(venv, name, E.FunEntry{formals = map #ty params', result = result_ty})
                fun enterparam ({name, ty}, venv) = S.enter(venv, name, E.VarEntry{access=(), ty=ty})
                val venv'' = fold enterparam params' venv'
            in 
                transExp(venv'', tenv) body;
                {venv=venv', tenv = tenv}
            end 
    fun transProg(exp: Absyn.exp) = transExp(Env.base_venv, Env.base_tenv, exp)
end 