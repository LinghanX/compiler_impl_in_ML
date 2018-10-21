structure T = Tokens
type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue,pos) T.token


val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
val commentLevel = ref 0
val stringPartial = ref false
val stringText : string ref = ref ""
val stringT = {text= ref "", partial= ref false, start= ref 0}
val stringStart = ref 0
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = 
    let val pos = hd(!linePos) 
    in if (!commentLevel = 0) then ()
        else ErrorMsg.error pos ("reached EOF inside comment");
      if (!stringPartial) then (ErrorMsg.error pos ("reached EOF inside string"))
        else ();
        T.EOF(pos,pos) 
    end

fun toASCII(pos, inText) =
    let val decChars = String.substring(inText, 1, 3);
        val ordVal = valOf( Int.fromString( decChars));
    in  if  (ordVal < 256) then ()
          else ErrorMsg.error pos ("invalid ASCII code: " ^ inText);
        if (ordVal < 255) then Char.toString( chr(ordVal))
          else ""
    end

fun fromCtrlChar inText =
    let val str = String.substring(inText, 0, 3);
        val ret = valOf( String.fromString str );
    in ret end

fun newLines inText =
    let val strList = String.explode(inText);
    in
        length( List.filter (fn x => x = #"\n") strList)
    end   

structure KeyWord :
    sig
        val find:string->(int*int->(svalue,int) T.token) option
    end =
struct
    val TableSize = 422 (* 211 *)
    val HashFactor = 5
    val hash = fn s => 
        List.foldr (fn (c,v) => (v*HashFactor+(ord c)) mod TableSize) 0 (explode s)
    val HashTable = Array.array(TableSize,nil) :
        (string * (int * int -> (svalue,int) T.token)) list Array.array
    val add = fn (s,v) => 
        let val i = hash s
        in Array.update(HashTable,i,(s,v) :: (Array.sub(HashTable, i)))
        end
    val find = fn s => 
        let val i = hash s
            fun f ((key,v)::r) = if s=key then SOME v else f r
                | f nil = NONE
        in f (Array.sub(HashTable, i))
        end
val _ = (List.app add [
    ("array", T.ARRAY),
    ("break", T.BREAK),
    ("do", T.DO),
    ("else", T.ELSE),
    ("end", T.END),
    ("for", T.FOR),
    ("function", T.FUNCTION),
    ("if", T.IF),
    ("in", T.IN),
    ("let", T.LET),
    ("nil", T.NIL),
    ("of", T.OF),
    ("then", T.THEN),
    ("to", T.TO),
    ("type", T.TYPE),
    ("var", T.VAR),
    ("while", T.WHILE)
])
end

open KeyWord;

%% 
%header (functor TigerLexFun (structure Tokens: Tiger_TOKENS));

%s COMMENT STRING;

DIGITS=[0-9]+;
WHITESPC=[\ \t]+;
ID=[a-zA-Z][a-zA-Z0-9_]*;
PRINT=[#-\[\]-~ ]+;

%%
<INITIAL>"/*" => (commentLevel := 1; YYBEGIN COMMENT; continue());
<INITIAL>\"   => (stringPartial := true; stringText := ""; stringStart := yypos; YYBEGIN STRING; continue());
<INITIAL>"*/" => (ErrorMsg.error yypos ("invalid comment"); continue());

<COMMENT>"*/" => (if (!commentLevel = 1) then YYBEGIN INITIAL
                  else ();
                  commentLevel := !commentLevel-1;
                  continue());
<COMMENT>"/*" => (commentLevel := !commentLevel+1; continue());
<COMMENT>.    => (continue());

<STRING>\\n   => (stringText := !stringText ^ "\n"; continue());
<STRING>\\t   => (stringText := !stringText ^ "\t"; continue());
<STRING>\\\"  => (stringText := !stringText ^ "\""; continue());
<STRING>\\\\  => (stringText := !stringText ^ "\\"; continue());
<STRING>\\\^[@A-Z\[\]\\^_ ]  => (stringText := !stringText ^ fromCtrlChar yytext; continue());
<STRING>\\[0-9]{3}  => (stringText := !stringText ^ toASCII(yypos, yytext); 
                 continue());
<STRING>\\[\ \t\n\r]+\\  => (lineNum := !lineNum+(newLines yytext); linePos := yypos :: !linePos;continue());
<STRING>{PRINT} => (stringText := !stringText ^ yytext; continue());
<STRING>\n    => (lineNum := !lineNum+1; linePos := yypos :: !linePos;
                  ErrorMsg.error yypos ("illegal string character: '" ^ (String.toString yytext) ^ "'");
                  continue());
<STRING>\"    => (YYBEGIN INITIAL; stringPartial := false; T.STRING( !stringText, !stringStart, yypos ));
<STRING>\\    => (ErrorMsg.error yypos ("illegal use of '\\' in string."); continue());
<STRING>.     => (ErrorMsg.error yypos ("illegal string character: " ^ yytext); continue());
<INITIAL,COMMENT>\n => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL>","  => (T.COMMA(yypos,yypos+1));
<INITIAL>":"  => (T.COLON(yypos,yypos+1));
<INITIAL>";"  => (T.SEMICOLON(yypos,yypos+1));
<INITIAL>"("  => (T.LPAREN(yypos,yypos+1));
<INITIAL>")"  => (T.RPAREN(yypos,yypos+1));
<INITIAL>"["  => (T.LBRACK(yypos,yypos+1));
<INITIAL>"]"  => (T.RBRACK(yypos,yypos+1));
<INITIAL>"{"  => (T.LBRACE(yypos,yypos+1));
<INITIAL>"}"  => (T.RBRACE(yypos,yypos+1));
<INITIAL>"."  => (T.DOT(yypos,yypos+1));
<INITIAL>"+"  => (T.PLUS(yypos,yypos+1));
<INITIAL>"-"  => (T.MINUS(yypos,yypos+1));
<INITIAL>"*"  => (T.TIMES(yypos,yypos+1));
<INITIAL>"/"  => (T.DIVIDE(yypos,yypos+1));
<INITIAL>"="  => (T.EQ(yypos,yypos+1));
<INITIAL>"<>" => (T.NEQ(yypos,yypos+2));
<INITIAL>"<"  => (T.LT(yypos,yypos+1));
<INITIAL>"<=" => (T.LE(yypos,yypos+2));
<INITIAL>">"  => (T.GT(yypos,yypos+1));
<INITIAL>">=" => (T.GE(yypos,yypos+2));
<INITIAL>"&"  => (T.AND(yypos,yypos+1));
<INITIAL>"|"  => (T.OR(yypos,yypos+1));
<INITIAL>":=" => (T.ASSIGN(yypos,yypos+2));

<INITIAL>{ID} => (case find yytext of
                    SOME v => v(yypos,yypos+size yytext)
                  | _      => (T.ID(yytext, yypos, yypos+size yytext)));
<INITIAL>{DIGITS} => (T.INT( valOf(Int.fromString yytext), yypos, yypos + size yytext));
<INITIAL>{WHITESPC} => (continue());
<INITIAL>.        => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
