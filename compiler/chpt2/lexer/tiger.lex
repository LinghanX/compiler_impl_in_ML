type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

%% 
digits=[0-9]+;
%s COMMENT;
%%

"\n" => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
[/][*] => (YYBEGIN COMMENT; continue());
<COMMENT> [*][/] => (YYBEGIN INITIAL; continue());
<COMMENT> . => (continue()); 
type => (Tokens.TYPE(yypos, yypos + 4));
var => (Tokens.VAR(yypos,yypos + size "var"));
function => (Tokens.FUNCTION(yypos, yypos + size "function"));
break => (Tokens.BREAK(yypos, yypos + size "break"));
of => (Tokens.OR(yypos, yypos + size "or"));
end => (Tokens.END(yypos, yypos + size "end"));
in => (Tokens.IN(yypos, yypos + size "in"));
nil => (Tokens.NIL(yypos, yypos + size "nil"));
let => (Tokens.LET(yypos, yypos + size "let"));
do => (Tokens.DO(yypos, yypos + size "do"));
to => (Tokens.TO(yypos, yypos + size "to"));
for => (Tokens.FOR(yypos, yypos + size "for"));
while => (Tokens.WHILE(yypos, yypos + size "while"));
else => (Tokens.ELSE(yypos, yypos + size "else"));
then => (Tokens.THEN(yypos, yypos + size "then"));
if => (Tokens.IF(yypos, yypos + 2));
array => (Tokens.ARRAY(yypos, yypos + size "array"));
":=" => (Tokens.ASSIGN(yypos, yypos + 2));  
or => (Tokens.OR(yypos, yypos + 2));
and => (Tokens.AND(yypos, yypos + 3));
">" => (Tokens.GT(yypos, yypos + 1));
">=" => (Tokens.GE(yypos, yypos + 2));
"<" => (Tokens.LT(yypos, yypos + 1));
"<=" => (Tokens.LE(yypos, yypos + 2));
"<>" => (Tokens.NEQ(yypos, yypos + 2));
"=" => (Tokens.EQ(yypos, yypos + 1));
"/" => (Tokens.DIVIDE(yypos, yypos + 1));
"*" => (Tokens.TIMES(yypos, yypos + 1));
"-" => (Tokens.MINUS(yypos, yypos + 1));
"+" => (Tokens.PLUS(yypos, yypos + 1));
"." => (Tokens.DOT(yypos, yypos + 1));
"[" => (Tokens.RBRACE(yypos, yypos + 1));
"]" => (Tokens.LBRACE(yypos, yypos + 1));
"}" => (Tokens.RBRACK(yypos, yypos + 1));
"{" => (Tokens.LBRACK(yypos, yypos + 1));
"(" => (Tokens.LPAREN(yypos, yypos + 1));
")" => (Tokens.RPAREN(yypos, yypos + 1));
";" => (Tokens.SEMICOLON(yypos, yypos + 1));
":" => (Tokens.COLON(yypos, yypos + 1));
"," => (Tokens.COMMA(yypos, yypos + 1));
["](\"|[^"])*["] => (Tokens.STRING(yytext, yypos, yypos + size yytext));
{digits} => (Tokens.INT(valOf(Int.fromString yytext), yypos, yypos + size yytext));
[a-z][a-z0-9]* => (Tokens.ID(yytext, yypos, yypos + size yytext));
\0 => (Tokens.EOF(yypos, yypos));