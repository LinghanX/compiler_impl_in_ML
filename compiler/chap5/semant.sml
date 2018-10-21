signature SEMANT =
sig
    type venv = Env.enventry Symbol.table 
    type tenv = ty Symbol.table 

    transVar: venv * tenv * Absyn.var -> expty 
    transExp: venv * tenv * Absyn.exp -> expty 
    transDec: venv * tenv * Absyn.dec -> { venv: venv, tenv: tenv }
    transTy:         tenv * Absyn.ty -> Types.ty 
end

structure Semant : SEMANT = 
struct 
    structure A = Absyn
    val error = ErrorMsg.error 

    type expty = { exp: Translate.exp, ty: Types.ty }

    fun transProg(exp: Absyn.exp) = transExp(Env.base_venv, Env.base_tenv, exp)

    fun checkInt ({exp, ty}, pos) = 
        case ty of 
            Types.INT => ()
            | _ => error pos "integer required";

    fun transExp(venv, tenv, exp) = 
        let 
            fun trexp (A.OpExp{left, oper = A.PlusOp, right, pos}) = 
                (checkInt(trexp left, pos);
                checkInt(trexp right, pos);
                {exp = (), ty = Types.INT})
            and trvar (A.SimpleVar(id, pos)) = 
                (case Symbol.look(venv, id)
                    of SOME(E.VarEntry{ty}) => {exp=(), ty = actual_ty ty}
                    | NONE => (error pos ("undefined variable " ^ S.name id);
                    {exp = (), ty = Types.INT}))
                | trvar (A.FieldVar(v, id, pos)) = ...
        in 
            trexp exp
        end 
end 