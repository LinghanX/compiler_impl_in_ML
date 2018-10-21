signature ENV = 
sig 
    type access 
    type ty 
    datatype enventry = 
        VarEntry of { ty: ty }
        | FunEntry of { formals: ty list, result: ty }
    val base_tenv : ty Symbol.table 
    val base_venv : enventry Symbol.table 
end 

structure Env : ENV =
struct
    type access = unit 
    type ty = Types.ty
    datatype enventry = 
        VarEntry of { ty: Types.ty }
        | FunEntry of { formals: Types.ty list, result: Types.ty }

    type tenv = Types.ty Symbol.table 
    type venv = enventry Symbol.table 

    fun addType((typeName, type'), tenv) 
        = Symbol.enter (tenv, Symbol.symbol typeName, type')
    
    fun addVal((name, enventry), venv) 
        = Symbol.enter (venv, Symbol.symbol name, enventry)
    
    val primeTypes = [
        ("int", Types.INT), 
        ("string", Types.STRING), 
        ("unit", Types.UNIT)]
    val stdFunctions = [
        ("print", FunEntry {
            formals = [Types.STRING],
            result = Types.UNIT
            }),
        ("flush", FunEntry {
            formals = [],
            result = Types.UNIT
        }),
        ("getchar", FunEntry {
            formals = [],
            result = Types.UNIT
        }),
        ("ord", FunEntry {
            formals = [Types.STRING],
            result = Types.INT
        }),
        ("chr", FunEntry {
            formals = [Types.INT],
            result = Types.STRING
        }),
        ("size", FunEntry {
            formals = [Types.STRING],
            result = Types.INT
        }), 
        ("substring", FunEntry {
            formals = [Types.STRING, Types.INT, Types.INT],
            result = Types.STRING
        }),
        ("concat", FunEntry {
            formals = [Types.STRING, Types.STRING],
            result = Types.STRING
        }),
        ("not", FunEntry {
            formals = [Types.INT],
            result = Types.INT
        }),
        ("exit", FunEntry {
            formals = [Types.INT],
            result = Types.UNIT
        })
    ]

    val base_tenv = List.foldr addType Symbol.empty primeTypes
    val base_venv = List.foldr addVal Symbol.empty stdFunctions
end