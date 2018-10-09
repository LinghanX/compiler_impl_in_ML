functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\214\000\005\000\214\000\007\000\214\000\009\000\214\000\
\\011\000\214\000\013\000\214\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\025\000\027\000\026\000\026\000\
\\030\000\214\000\031\000\214\000\034\000\214\000\035\000\214\000\
\\037\000\214\000\038\000\214\000\042\000\214\000\043\000\214\000\
\\044\000\214\000\000\000\
\\001\000\001\000\215\000\005\000\215\000\007\000\215\000\009\000\215\000\
\\011\000\215\000\013\000\215\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\025\000\027\000\026\000\026\000\
\\030\000\215\000\031\000\215\000\034\000\215\000\035\000\215\000\
\\037\000\215\000\038\000\215\000\042\000\215\000\043\000\215\000\
\\044\000\215\000\000\000\
\\001\000\001\000\216\000\005\000\216\000\007\000\216\000\009\000\216\000\
\\011\000\216\000\013\000\216\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\025\000\027\000\026\000\026\000\
\\030\000\216\000\031\000\216\000\034\000\216\000\035\000\216\000\
\\037\000\216\000\038\000\216\000\042\000\216\000\043\000\216\000\
\\044\000\216\000\000\000\
\\001\000\001\000\217\000\005\000\217\000\007\000\217\000\009\000\217\000\
\\011\000\217\000\013\000\217\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\025\000\027\000\026\000\026\000\
\\030\000\217\000\031\000\217\000\034\000\217\000\035\000\217\000\
\\037\000\217\000\038\000\217\000\042\000\217\000\043\000\217\000\
\\044\000\217\000\000\000\
\\001\000\001\000\218\000\005\000\218\000\007\000\218\000\009\000\218\000\
\\011\000\218\000\013\000\218\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\025\000\027\000\026\000\026\000\
\\030\000\218\000\031\000\218\000\034\000\218\000\035\000\218\000\
\\037\000\218\000\038\000\218\000\042\000\218\000\043\000\218\000\
\\044\000\218\000\000\000\
\\001\000\001\000\219\000\005\000\219\000\007\000\219\000\009\000\219\000\
\\011\000\219\000\013\000\219\000\015\000\037\000\016\000\036\000\
\\017\000\035\000\018\000\034\000\025\000\027\000\026\000\026\000\
\\030\000\219\000\031\000\219\000\034\000\219\000\035\000\219\000\
\\037\000\219\000\038\000\219\000\042\000\219\000\043\000\219\000\
\\044\000\219\000\000\000\
\\001\000\002\000\022\000\003\000\021\000\004\000\020\000\008\000\019\000\
\\009\000\045\000\016\000\018\000\029\000\017\000\032\000\016\000\
\\033\000\015\000\036\000\014\000\040\000\013\000\041\000\012\000\000\000\
\\001\000\002\000\022\000\003\000\021\000\004\000\020\000\008\000\019\000\
\\009\000\085\000\016\000\018\000\029\000\017\000\032\000\016\000\
\\033\000\015\000\036\000\014\000\040\000\013\000\041\000\012\000\000\000\
\\001\000\002\000\022\000\003\000\021\000\004\000\020\000\008\000\019\000\
\\016\000\018\000\029\000\017\000\032\000\016\000\033\000\015\000\
\\036\000\014\000\040\000\013\000\041\000\012\000\000\000\
\\001\000\002\000\039\000\000\000\
\\001\000\002\000\050\000\000\000\
\\001\000\002\000\081\000\000\000\
\\001\000\002\000\089\000\000\000\
\\001\000\002\000\090\000\000\000\
\\001\000\002\000\091\000\000\000\
\\001\000\002\000\119\000\012\000\118\000\028\000\117\000\000\000\
\\001\000\002\000\121\000\000\000\
\\001\000\002\000\135\000\000\000\
\\001\000\002\000\140\000\000\000\
\\001\000\002\000\143\000\000\000\
\\001\000\002\000\147\000\000\000\
\\001\000\002\000\150\000\000\000\
\\001\000\006\000\107\000\027\000\106\000\000\000\
\\001\000\006\000\133\000\000\000\
\\001\000\006\000\139\000\019\000\138\000\000\000\
\\001\000\006\000\149\000\000\000\
\\001\000\008\000\108\000\000\000\
\\001\000\009\000\077\000\000\000\
\\001\000\009\000\102\000\000\000\
\\001\000\009\000\132\000\000\000\
\\001\000\011\000\086\000\015\000\037\000\016\000\036\000\017\000\035\000\
\\018\000\034\000\019\000\033\000\020\000\032\000\021\000\031\000\
\\022\000\030\000\023\000\029\000\024\000\028\000\025\000\027\000\
\\026\000\026\000\000\000\
\\001\000\011\000\101\000\015\000\037\000\016\000\036\000\017\000\035\000\
\\018\000\034\000\019\000\033\000\020\000\032\000\021\000\031\000\
\\022\000\030\000\023\000\029\000\024\000\028\000\025\000\027\000\
\\026\000\026\000\000\000\
\\001\000\013\000\097\000\000\000\
\\001\000\013\000\136\000\000\000\
\\001\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\030\000\076\000\000\000\
\\001\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\034\000\110\000\000\000\
\\001\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\035\000\075\000\000\000\
\\001\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\035\000\134\000\000\000\
\\001\000\019\000\100\000\000\000\
\\001\000\019\000\105\000\000\000\
\\001\000\019\000\146\000\000\000\
\\001\000\027\000\074\000\000\000\
\\001\000\027\000\131\000\000\000\
\\001\000\037\000\073\000\042\000\072\000\043\000\071\000\044\000\070\000\000\000\
\\001\000\038\000\109\000\000\000\
\\001\000\039\000\114\000\000\000\
\\001\000\039\000\129\000\000\000\
\\153\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\000\000\
\\154\000\010\000\025\000\014\000\024\000\027\000\023\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\159\000\000\000\
\\160\000\000\000\
\\161\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\000\000\
\\162\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\000\000\
\\163\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\
\\031\000\111\000\000\000\
\\164\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\000\000\
\\165\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\000\000\
\\173\000\025\000\027\000\026\000\026\000\000\000\
\\174\000\000\000\
\\175\000\000\000\
\\176\000\000\000\
\\177\000\000\000\
\\178\000\005\000\104\000\015\000\037\000\016\000\036\000\017\000\035\000\
\\018\000\034\000\019\000\033\000\020\000\032\000\021\000\031\000\
\\022\000\030\000\023\000\029\000\024\000\028\000\025\000\027\000\
\\026\000\026\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\042\000\072\000\000\000\
\\185\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\000\000\
\\186\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\000\000\
\\190\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\000\000\
\\191\000\000\000\
\\192\000\002\000\081\000\000\000\
\\193\000\000\000\
\\194\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\000\000\
\\195\000\005\000\099\000\000\000\
\\196\000\000\000\
\\197\000\015\000\037\000\016\000\036\000\017\000\035\000\018\000\034\000\
\\019\000\033\000\020\000\032\000\021\000\031\000\022\000\030\000\
\\023\000\029\000\024\000\028\000\025\000\027\000\026\000\026\000\000\000\
\\198\000\007\000\078\000\015\000\037\000\016\000\036\000\017\000\035\000\
\\018\000\034\000\019\000\033\000\020\000\032\000\021\000\031\000\
\\022\000\030\000\023\000\029\000\024\000\028\000\025\000\027\000\
\\026\000\026\000\000\000\
\\199\000\000\000\
\\200\000\044\000\070\000\000\000\
\\201\000\000\000\
\\202\000\000\000\
\\203\000\000\000\
\\204\000\000\000\
\\205\000\000\000\
\\206\000\002\000\123\000\000\000\
\\207\000\000\000\
\\208\000\005\000\145\000\000\000\
\\209\000\000\000\
\\210\000\017\000\035\000\018\000\034\000\025\000\027\000\026\000\026\000\000\000\
\\211\000\017\000\035\000\018\000\034\000\025\000\027\000\026\000\026\000\000\000\
\\212\000\025\000\027\000\026\000\026\000\000\000\
\\213\000\025\000\027\000\026\000\026\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\008\000\048\000\010\000\047\000\012\000\046\000\000\000\
\\223\000\000\000\
\\224\000\000\000\
\"
val actionRowNumbers =
"\009\000\067\000\066\000\065\000\
\\064\000\055\000\054\000\053\000\
\\049\000\048\000\052\000\061\000\
\\075\000\010\000\009\000\009\000\
\\009\000\007\000\051\000\050\000\
\\111\000\009\000\011\000\009\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\009\000\009\000\
\\009\000\009\000\009\000\009\000\
\\044\000\042\000\037\000\035\000\
\\068\000\028\000\093\000\069\000\
\\087\000\009\000\008\000\056\000\
\\112\000\031\000\110\000\109\000\
\\006\000\005\000\004\000\003\000\
\\002\000\001\000\108\000\107\000\
\\106\000\105\000\082\000\095\000\
\\079\000\078\000\077\000\076\000\
\\013\000\014\000\015\000\009\000\
\\009\000\009\000\009\000\063\000\
\\009\000\033\000\090\000\039\000\
\\032\000\029\000\073\000\070\000\
\\113\000\096\000\083\000\040\000\
\\023\000\027\000\045\000\036\000\
\\059\000\058\000\094\000\086\000\
\\088\000\012\000\009\000\046\000\
\\071\000\072\000\009\000\016\000\
\\009\000\017\000\101\000\062\000\
\\009\000\009\000\090\000\089\000\
\\009\000\073\000\097\000\047\000\
\\101\000\098\000\080\000\043\000\
\\030\000\024\000\038\000\057\000\
\\091\000\092\000\074\000\018\000\
\\034\000\009\000\025\000\019\000\
\\009\000\100\000\099\000\081\000\
\\009\000\020\000\103\000\060\000\
\\084\000\041\000\102\000\021\000\
\\009\000\026\000\085\000\022\000\
\\103\000\104\000\000\000"
val gotoT =
"\
\\001\000\009\000\002\000\150\000\003\000\008\000\013\000\007\000\
\\014\000\006\000\015\000\005\000\020\000\004\000\021\000\003\000\
\\022\000\002\000\023\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\036\000\000\000\
\\000\000\
\\001\000\038\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\039\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\040\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\042\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\016\000\041\000\020\000\004\000\021\000\003\000\
\\022\000\002\000\023\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\047\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\000\000\
\\001\000\049\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\050\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\051\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\052\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\053\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\054\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\055\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\056\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\057\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\058\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\059\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\060\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\061\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\005\000\067\000\006\000\066\000\007\000\065\000\008\000\064\000\
\\009\000\063\000\024\000\062\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\078\000\019\000\077\000\000\000\
\\001\000\080\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\082\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\026\000\081\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\006\000\085\000\009\000\063\000\000\000\
\\024\000\086\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\042\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\016\000\090\000\020\000\004\000\021\000\003\000\
\\022\000\002\000\023\000\001\000\000\000\
\\001\000\091\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\092\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\093\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\000\000\
\\001\000\042\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\016\000\094\000\020\000\004\000\021\000\003\000\
\\022\000\002\000\023\000\001\000\000\000\
\\000\000\
\\018\000\096\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\025\000\101\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\110\000\000\000\
\\001\000\111\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\113\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\010\000\114\000\000\000\
\\001\000\118\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\000\000\
\\011\000\120\000\000\000\
\\000\000\
\\001\000\122\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\001\000\123\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\018\000\124\000\000\000\
\\000\000\
\\001\000\125\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\025\000\126\000\000\000\
\\000\000\
\\000\000\
\\011\000\128\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\135\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\139\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\140\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\000\000\
\\012\000\142\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\146\000\003\000\008\000\013\000\007\000\014\000\006\000\
\\015\000\005\000\020\000\004\000\021\000\003\000\022\000\002\000\
\\023\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\149\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 151
val numrules = 72
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | (T 44) => "UMINUS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.ntVOID l_value1, l_value1left, l_value1right
)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  l_value1 = l_value1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, l_value1left, l_value1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  INT1 = INT1
 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 STRING1 = STRING1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 4, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ntVOID unit_exp1, unit_exp1left, 
unit_exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  unit_exp1 = unit_exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, unit_exp1left, unit_exp1right), rest671)

end
|  ( 6, ( ( _, ( MlyValue.ntVOID record_creation1, 
record_creation1left, record_creation1right)) :: rest671)) => let val 
 result = MlyValue.ntVOID (fn _ => ( let val  record_creation1 = 
record_creation1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, record_creation1left, 
record_creation1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.ntVOID array_creation1, array_creation1left,
 array_creation1right)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  array_creation1 = array_creation1
 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, array_creation1left, 
array_creation1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID l_value1, l_value1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  l_value1 = l_value1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, l_value1left, exp1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _
)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, IF1left, exp3right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, IF1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1
 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, WHILE1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _
)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FOR1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, FOR1left, exp3right), rest671)
end
|  ( 13, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 14, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID expseq1,
 _, _)) :: _ :: ( _, ( MlyValue.ntVOID decs1, _, _)) :: ( _, ( _, 
LET1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  decs1 = decs1 ()
 val  expseq1 = expseq1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, LET1left, END1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
expseq1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val 
 result = MlyValue.ntVOID (fn _ => ( let val  expseq1 = expseq1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ntVOID func_call1, func_call1left, 
func_call1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  func_call1 = func_call1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, func_call1left, func_call1right), 
rest671)
end
|  ( 17, ( ( _, ( MlyValue.ntVOID arith_exp1, arith_exp1left, 
arith_exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  arith_exp1 = arith_exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, arith_exp1left, arith_exp1right), 
rest671)
end
|  ( 18, ( ( _, ( MlyValue.ntVOID com_exp1, com_exp1left, 
com_exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  com_exp1 = com_exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, com_exp1left, com_exp1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.ntVOID bool_exp1, bool_exp1left, 
bool_exp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  bool_exp1 = bool_exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, bool_exp1left, bool_exp1right), rest671)

end
|  ( 20, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, MINUS1left, exp1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 12, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 22, ( ( _, ( _, _, RPAREN1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 19, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 23, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID 
explist1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1
 = ID1 ()
 val  explist1 = explist1 ()
 in ()
end; ()))
 in ( LrTable.NT 19, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ntVOID more_exp1, _, more_exp1right)) :: (
 _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  more_exp1 = more_exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 25, ( result, exp1left, more_exp1right), rest671)
end
|  ( 25, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 24, ( result, defaultPos, defaultPos), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID more_exp1, _, more_exp1right)) :: (
 _, ( MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, COMMA1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 val  more_exp1 = more_exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 24, ( result, COMMA1left, more_exp1right), rest671)

end
|  ( 27, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ntVOID dec1, _, dec1right)) :: ( _, ( 
MlyValue.ntVOID decs1, decs1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  decs1 = decs1 ()
 val  dec1 = dec1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, decs1left, dec1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.ntVOID typedecs1, typedecs1left, 
typedecs1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  typedecs1 = typedecs1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, typedecs1left, typedecs1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.ntVOID vardec1, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 vardec1 = vardec1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, vardec1left, vardec1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.ntVOID fundecs1, fundecs1left, 
fundecs1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  fundecs1 = fundecs1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, fundecs1left, fundecs1right), rest671)

end
|  ( 32, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, VAR1left, exp1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _,
 ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, VAR1left, exp1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ntVOID fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 fundec1 = fundec1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, fundec1left, fundec1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.ntVOID fundec1, _, fundec1right)) :: ( _, (
 MlyValue.ntVOID fundecs1, fundecs1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  fundecs1 = fundecs1 ()
 val  fundec1 = fundec1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, fundecs1left, fundec1right), rest671)

end
|  ( 36, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: _ :: (
 _, ( MlyValue.ntVOID ty_fields1, _, _)) :: _ :: ( _, ( MlyValue.ID 
ID1, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ty_fields1 = ty_fields1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 23, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID ty_fields1
, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ID1 = ID1 ()
 val  ty_fields1 = ty_fields1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 23, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 38, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
record_creation_list1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left,
 _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  ID1 = ID1 ()
 val  record_creation_list1 = record_creation_list1 ()
 in ()
end; ()))
 in ( LrTable.NT 13, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 39, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 18, ( result, defaultPos, defaultPos), rest671)
end
|  ( 40, ( ( _, ( MlyValue.ntVOID more_records1, _, more_records1right
)) :: ( _, ( MlyValue.ntVOID record1, record1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  record1 = 
record1 ()
 val  more_records1 = more_records1 ()
 in ()
end; ()))
 in ( LrTable.NT 18, ( result, record1left, more_records1right), 
rest671)
end
|  ( 41, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 16, ( result, ID1left, exp1right), rest671)
end
|  ( 42, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 43, ( ( _, ( MlyValue.ntVOID more_records1, _, more_records1right
)) :: ( _, ( MlyValue.ntVOID record1, _, _)) :: ( _, ( _, COMMA1left,
 _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  record1 = record1 ()
 val  more_records1 = more_records1 ()
 in ()
end; ()))
 in ( LrTable.NT 17, ( result, COMMA1left, more_records1right), 
rest671)
end
|  ( 44, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: _ :: (
 _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 14, ( result, ID1left, exp2right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 15, ( result, exp1left, exp1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.ntVOID expseq1, _, expseq1right)) :: _ :: (
 _, ( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  expseq1 = expseq1 ()
 in ()
end; ()))
 in ( LrTable.NT 15, ( result, exp1left, expseq1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.ntVOID typedec1, typedec1left, 
typedec1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  typedec1 = typedec1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, typedec1left, typedec1right), rest671)

end
|  ( 48, ( ( _, ( MlyValue.ntVOID typedecs1, _, typedecs1right)) :: (
 _, ( MlyValue.ntVOID typedec1, typedec1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  typedec1 = typedec1
 ()
 val  typedecs1 = typedecs1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, typedec1left, typedecs1right), rest671)

end
|  ( 49, ( ( _, ( MlyValue.ntVOID ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ty1 = ty1 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, TYPE1left, ty1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, ID1left, ID1right), rest671)
end
|  ( 51, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
ty_fields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  ty_fields1 = 
ty_fields1 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 53, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 10, ( result, defaultPos, defaultPos), rest671)
end
|  ( 54, ( ( _, ( MlyValue.ntVOID more_ty_fields1, _, 
more_ty_fields1right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, 
( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  more_ty_fields1 = more_ty_fields1 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, ID1left, more_ty_fields1right), rest671
)
end
|  ( 55, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 11, ( result, defaultPos, defaultPos), rest671)
end
|  ( 56, ( ( _, ( MlyValue.ntVOID more_ty_fields1, _, 
more_ty_fields1right)) :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, 
( MlyValue.ID ID1, _, _)) :: ( _, ( _, COMMA1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  more_ty_fields1 = more_ty_fields1 ()
 in ()
end; ()))
 in ( LrTable.NT 11, ( result, COMMA1left, more_ty_fields1right), 
rest671)
end
|  ( 57, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 20, ( result, exp1left, exp2right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 20, ( result, exp1left, exp2right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 20, ( result, exp1left, exp2right), rest671)
end
|  ( 60, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 20, ( result, exp1left, exp2right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 21, ( result, exp1left, exp2right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 21, ( result, exp1left, exp2right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 21, ( result, exp1left, exp2right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 21, ( result, exp1left, exp2right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 21, ( result, exp1left, exp2right), rest671)
end
|  ( 66, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 21, ( result, exp1left, exp2right), rest671)
end
|  ( 67, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 22, ( result, exp1left, exp2right), rest671)
end
|  ( 68, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 22, ( result, exp1left, exp2right), rest671)
end
|  ( 69, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
|  ( 70, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.ntVOID l_value1, l_value1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  l_value1 = l_value1 ()
 val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, l_value1left, ID1right), rest671)
end
|  ( 71, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.ntVOID exp1,
 _, _)) :: _ :: ( _, ( MlyValue.ntVOID l_value1, l_value1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
l_value1 = l_value1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, l_value1left, RBRACK1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
