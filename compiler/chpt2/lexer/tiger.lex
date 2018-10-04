type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

(* Here we initiate a box for `comment level`, in that
   each time we encounter a `/*`, we ask: am i in the 
   `comment` state, if so, we increment commentLvl by
   1, otherwise we begin `comment` state.

   When we see a `*/`, we consult the value of commentLvl,
   if its 0, then we end `comment` state, otherwise we 
   decrement commentLvl by 1
 *)
val commentLvl = ref 0;
val stringBuf = "";
fun eof() = 
    let 
        val pos = hd(!linePos) 
    in 
        if (!commentLvl = 1) then (print "error: comment"; Tokens.EOF(pos, pos))
        else (Tokens.EOF(pos,pos))
    end
%% 
digits=[0-9]+;
%s COMMENT;
%%

<INITIAL> ["]((\\.)|([^\\"]))*["] => (Tokens.STRING(yytext, yypos, yypos + size yytext));
<INITIAL> \n|\r|\r\n => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> " " => (linePos := yypos :: !linePos; continue());
<INITIAL> \t => (continue());
<INITIAL> \/\* => (YYBEGIN COMMENT; commentLvl := !commentLvl + 1; continue());
<COMMENT> \*\/ => (
    if (!commentLvl = 1) then (commentLvl := 0; YYBEGIN INITIAL; continue())
    else (commentLvl := !commentLvl - 1; continue()));
<COMMENT> \/\* => (commentLvl := !commentLvl + 1; continue());
<COMMENT> . => (continue()); 
<INITIAL> type => (Tokens.TYPE(yypos, yypos + 4));
<INITIAL> var => (Tokens.VAR(yypos,yypos + size "var"));
<INITIAL> function => (Tokens.FUNCTION(yypos, yypos + size "function"));
<INITIAL> break => (Tokens.BREAK(yypos, yypos + size "break"));
<INITIAL> of => (Tokens.OF(yypos, yypos + size "of"));
<INITIAL> end => (Tokens.END(yypos, yypos + size "end"));
<INITIAL> in => (Tokens.IN(yypos, yypos + size "in"));
<INITIAL> nil => (Tokens.NIL(yypos, yypos + size "nil"));
<INITIAL> let => (Tokens.LET(yypos, yypos + 3));
<INITIAL> do => (Tokens.DO(yypos, yypos + size "do"));
<INITIAL> to => (Tokens.TO(yypos, yypos + size "to"));
<INITIAL> for => (Tokens.FOR(yypos, yypos + size "for"));
<INITIAL> while => (Tokens.WHILE(yypos, yypos + size "while"));
<INITIAL> else => (Tokens.ELSE(yypos, yypos + size "else"));
<INITIAL> then => (Tokens.THEN(yypos, yypos + size "then"));
<INITIAL> if => (Tokens.IF(yypos, yypos + 2));
<INITIAL> array => (Tokens.ARRAY(yypos, yypos + size "array"));
<INITIAL> ":=" => (Tokens.ASSIGN(yypos, yypos + 2));  
<INITIAL> \| => (Tokens.OR(yypos, yypos + 2));
<INITIAL> "&" => (Tokens.AND(yypos, yypos + 1));
<INITIAL> ">" => (Tokens.GT(yypos, yypos + 1));
<INITIAL> ">=" => (Tokens.GE(yypos, yypos + 2));
<INITIAL> "<" => (Tokens.LT(yypos, yypos + 1));
<INITIAL> "<=" => (Tokens.LE(yypos, yypos + 2));
<INITIAL> "<>" => (Tokens.NEQ(yypos, yypos + 2));
<INITIAL> "=" => (Tokens.EQ(yypos, yypos + 1));
<INITIAL> "/" => (Tokens.DIVIDE(yypos, yypos + 1));
<INITIAL> "*" => (Tokens.TIMES(yypos, yypos + 1));
<INITIAL> "-" => (Tokens.MINUS(yypos, yypos + 1));
<INITIAL> "+" => (Tokens.PLUS(yypos, yypos + 1));
<INITIAL> "." => (Tokens.DOT(yypos, yypos + 1));
<INITIAL> "[" => (Tokens.LBRACE(yypos, yypos + 1));
<INITIAL> "]" => (Tokens.RBRACE(yypos, yypos + 1));
<INITIAL> "}" => (Tokens.RBRACK(yypos, yypos + 1));
<INITIAL> "{" => (Tokens.LBRACK(yypos, yypos + 1));
<INITIAL> "(" => (Tokens.LPAREN(yypos, yypos + 1));
<INITIAL> ")" => (Tokens.RPAREN(yypos, yypos + 1));
<INITIAL> ";" => (Tokens.SEMICOLON(yypos, yypos + 1));
<INITIAL> ":" => (Tokens.COLON(yypos, yypos + 1));
<INITIAL> "," => (Tokens.COMMA(yypos, yypos + 1));
<INITIAL> {digits} => (Tokens.INT(valOf(Int.fromString yytext), yypos, yypos + size yytext));
<INITIAL> [a-zA-Z][a-zA-Z0-9]* => (Tokens.ID(yytext, yypos, yypos + size yytext));