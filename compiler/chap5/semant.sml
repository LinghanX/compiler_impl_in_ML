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
    structure E = Env 
    structure T = Types
    val error = ErrorMsg.error 
    type expty = { exp: Translate.exp, ty: Types.ty }

    fun checkInt ({exp, ty}, pos) = case ty of 
        Types.INT => () 
        | _ => (error pos "integer required")
    fun findType (ty) = case ty of 
        Types.NAME(sym, tyop) => 
            (case !tyop of 
                SOME(T.NAME(sym', typo')) => findType(T.NAME(sym', typo'))
                | SOME(ty'') => ty''
                | NONE => T.NIL)
        | _ => ty 
    fun findBaseType (t) = 
        (case t of 
            T.NAME(sym, tyop) => findType(T.NAME(sym, tyop))
            | T.ARRAY(ty, uniqRef) => findBaseType(ty)
            | _ => t)
    fun checkType(ty1, ty2) = 
        case (ty1, ty2) of 
            (T.INT, T.INT) => true 
            | (T.NIL, T.NIL) => true 
            | (T.UNIT, T.UNIT) => true 
            | (T.RECORD(lst1, uniq1), T.RECORD(lst2, uniq2)) => (uniq1 = uniq2)
            | (T.ARRAY(ty1, uniq1), T.ARRAY(ty2, uniq2)) => (uniq1 = uniq2)
            | (T.NAME(name1, tyop1), T.NAME(name2, tyop2)) => 
                checkType(findBaseType(T.NAME(name1, tyop1)), findBaseType(T.NAME(name2, tyop2)))
            | (T.NAME(name1, tyop1), _) => (checkType(findBaseType(T.NAME(name1, tyop1)), ty2))
            | (_, T.NAME(name2, tyop2)) => (checkType(findBaseType(T.NAME(name2, tyop2)), ty1))
            | _ => false
    fun checkSameType({exp, ty = ty1}, {exp, ty = ty2}) = 
        let 
            val ty1' = findType(ty1)
            val ty2' = findType(ty2)
        in 
            checkType(ty1', ty2')
        end 

    fun transExp(venv, tenv, exp) = 
        let 
            fun trexp (A.OpExp{left, oper, right, pos}) = 
                let in case oper of 
                    PlusOp => (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty = Types.INT})
                    | MinusOp => (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty = Types.INT})
                    | TimesOp => (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty = Types.INT})
                    | DivideOp => (checkInt(trexp left, pos); checkInt(trexp right, pos); {exp = (), ty = Types.INT})
                    | EqOp => (checkSameType(trexp left, trexp right); {exp = (), ty = Types.INT}) 
                    | NeqOp => (checkSameType(trexp left, trexp right); {exp = (), ty = Types.INT})
                    | LtOp => (checkSameType(trexp left, trexp right); {exp = (), ty = Types.INT})
                    | LeOp => (checkSameType(trexp left, trexp right); {exp = (), ty = Types.INT})
                    | GtOp => (checkSameType(trexp left, trexp right); {exp = (), ty = Types.INT})
                    | GeOp => (checkSameType(trexp left, trexp right); {exp = (), ty = Types.INT})
                end 
            | trexp(A.LetExp{decs, body, pos}) = 
                let val {venv = venv', tenv = tenv'} = transDecs(venv, tenv, decs)
                in transExp(venv', tenv') body end
            | trexp (A.NilExp) = {exp = (), ty = Types.UNIT}
            | trexp (A.IntExp(i)) = {exp = (), ty = Types.INT}
            | trexp (A.StringExp(str, pos)) = {exp = (), ty = Types.STRING}
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
                    val recordType = findType (typ)
                    fun getFieldType field{symbol, exp, pos} = 
                        let val {exp, ty'} = transExp(exp)
                        in ty' end
                    val fieldTypes = foldl getFieldType [] fields
                in
                    checkType(hd(fieldTypes), findType(typ))
                end 
            | trexp (A.SeqExp((exp, pos)::rst)) = {exp = (), ty = Types.UNIT}
            | trexp (A.AssignExp{var, exp, pos}) = {exp = (), ty = Types.UNIT}
            | trexp (A.IfExp(test, then', else', pos)) = 
                let 
                    val { ty = testType} = transExp test 
                    val { ty = thenType} = transExp then'
                    val { ty = elseType} = transExp else'
                in 
                    (checkType(testType, Types.INT, pos)(checkType(thenType, elseType, pos)))
                end
            | trexp (A.WhileExp{test, body, pos}) = 
                let 
                    val { ty = testType} = transExp test 
                    val { ty = bodyType} = transExp exp 
                in 
                    (checkType(test, Types.INT, pos)
                    {exp = (), ty = Types.UNIT})
                end
            | trexp (A.BreakExp(pos)) = {exp = (), ty = Types.UNIT}
            | trexp (A.ArrayExp{typ, size, init, pos}) = 
                let 
                    val sizeType = transExp size
                    val ty' = findType (typ) 
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
                | trvar (A.FieldVar(v, id, pos)) = {exp = (), ty = Types.UNIT}
                | trvar (A.SubscriptVar(var, exp, pos)) = 
                    let val {exp, ty} = transExp exp in 
                        (checkType(ty, Types.INT, pos)
                        {exp = (), ty = Types.UNIT})
                    end 
        in 
            trexp exp
        end 
    
    and transDecs(venv,tenv) =
        let
            fun trdec (venv,tenv,A.VarDec{name,escape,typ=NONE,init,pos}) =
                let 
                    val {exp,ty} = transExp(venv,tenv) init
                in 
                    {tenv=tenv,venv = S.enter(venv,name,E.VarEntry{ty=ty})}
                end
            | trdec (venv,tenv,A.VarDec{name,escape,typ=SOME(tysym, _),init,pos}) =
                (case S.look(tenv,tysym) of 
                    SOME(ty) => {tenv=tenv,venv=S.enter(venv,name,E.VarEntry{ty=ty})}
                    | NONE => (Err.error pos "Type not defined: " ^ S.name tysym); 
                        {tenv=tenv,venv=venv})
            | trdec(venv,tenv,A.TypeDec(tydecs)) =
                    let
                        fun newdec ({name,ty,pos}, env) = 
                            S.enter(env,name,transTy(env,ty))
                        val tenv' = foldr newdec tenv tydecs
                    in
                        { venv=venv,tenv=tenv' }
                    end
                    
            | trdec(venv,tenv,A.FunctionDec(fundecs)) =
                    let
                        val venv'   = foldr transFun (venv,tenv) venv fundecs
                    in {venv=venv',tenv=tenv} end
        in
            trdec
        end
    and transParam (venv,tenv) {name,typ,pos} =
          case S.look(tenv,typ) of 
               SOME t => {name=name,ty=t}
             | NONE => (Err.error 0 ("Parameter type not declared: " ^ S.name rt);
                       {name=name,ty=T.NIL})
    and transFun ((venv,tenv),{name,params,body,pos,result=SOME(rt,pos)}) =
        (case S.look(tenv,rt) of
            NONE => (Err.error 0 ("Return type not declared: " ^ S.name rt); NONE)
            | SOME(result_ty) =>
                let
                    fun enterparam ({name,ty},venv) = S.enter(venv,name,E.VarEntry{access=(),ty=ty})
                    val params' = map (transParam (venv,tenv)) params
                    val entry = E.FunEntry{formals= map #ty params',result=result_ty}
                    val venv'   = S.enter(venv,name,entry)
                    val venv''  = foldr enterparam params' venv'
                in
                    transExp (venv'',tenv=tenv) body;
                    SOME entry
                end )
      | transFun {name,params,body,pos,result=NONE} =
        let
          fun enterparam ({name,ty},venv) = S.enter(venv,name,E.VarEntry{access=(),ty=ty})
          val params' = map (transParam (venv,tenv)) params
          val entry   = E.FunEntry{formals= map #ty params',result=T.UNIT}
          val venv'   = S.enter(venv,name,entry)
          val venv''  = foldr enterparam params' venv'
        in
          transExp(venv'',tenv=tenv) body;
          SOME entry
        end
    fun transProg(exp: Absyn.exp) = transExp(Env.base_venv, Env.base_tenv, exp)
end 