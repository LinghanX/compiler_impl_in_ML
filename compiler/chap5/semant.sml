signature SEMANT =
sig
    type venv = Env.enventry Symbol.table 
    type tenv = ty Symbol.table 

    transVar: venv * tenv * Absyn.var -> expty 
    transExp: venv * tenv * Absyn.exp -> expty 
    transDec: venv * tenv * Absyn.dec -> { venv: venv, tenv: tenv }
    transTy:         tenv * Absyn.ty  -> Types.ty 
end

structure Semant : SEMANT = 
struct 
    structure A = Absyn
    structure S = Symbol
    val error = ErrorMsg.error 
    type expty = { exp: Translate.exp, ty: Types.ty }
    fun transProg(exp: Absyn.exp) = transExp(Env.base_venv, Env.base_tenv, exp)
    fun checkInt ({exp, ty}, pos) = case ty of 
            Types.INT => () | _ => error pos "integer required";
    fun checkSameType({exp1, ty1}, {exp2, ty2}, pos) = 
        let 
            fun checkType(ty1, ty2, pos) = 
                case ty1 of 
                    Types.INT => 
                        case ty2 of Types.INT => () | _ => error pos "integer required";
                    | Types.STRING => 
                        case ty2 of Types.STRING => () | _ => error pos "string required";
                    | Types.ARRAY => 
                        case ty2 of Types.ARRAY => () | _ => error pos "array required";
                    | Types.NAME(sym) => 
                        case ty2 of 
                            Types.NAME(sym2) => 
                                let 
                                    val sym' = S.look(tenv, sym)
                                    val sym2' = S.look(tenv, sym2)
                                in 
                                    checkType(sym', sym2', pos)
                                end 
                            | _ => error pos "type not the same";
        in 
            checkType(ty1, ty2, pos)
        end 
                    

    fun transExp(venv, tenv, exp) = 
        let 
            fun trexp (A.OpExp{left, oper, right, pos}) = 
                (* (checkInt(trexp right, pos) 
                checkInt(trexp left, pos);
                {exp = (), ty = Types.INT}) *)
                case oper of 
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
            | trexp(A.LetExp{decs, body, pos}) = 
                let val {venv = venv', tenv = tenv'} = transDecs(venv, tenv, decs)
                in transExp(venv', tenv') body end
            | trexp (A.NilExp) = {exp = (), ty = Types.Unit}
            | trexp (A.IntExp) = {exp = (), ty = Types.INT}
            | trexp (A.StringExp) = {exp = (), ty = Types.STRING}
            | trexp (VarExp(var)) = trvar var
            and trvar (A.SimpleVar(id, pos)) = 
                (case Symbol.look(venv, id)
                    of SOME(E.VarEntry{ty}) => {exp=(), ty = actual_ty ty}
                    | NONE => (error pos ("undefined variable " ^ S.name id);
                    {exp = (), ty = Types.INT}))
                | trvar (A.FieldVar(v, id, pos)) = ...
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
end 