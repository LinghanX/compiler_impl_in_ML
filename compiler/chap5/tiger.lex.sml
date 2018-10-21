functor TigerLexFun (structure Tokens: Tiger_TOKENS)  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
STRING | COMMENT | INITIAL
    structure UserDeclarations = 
      struct

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



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (commentLevel := 1; YYBEGIN COMMENT; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm;
      (stringPartial := true; stringText := ""; stringStart := yypos; YYBEGIN STRING; continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm;
      (ErrorMsg.error yypos ("invalid comment"); continue()))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm;
      (if (!commentLevel = 1) then YYBEGIN INITIAL
                  else ();
                  commentLevel := !commentLevel-1;
                  continue()))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm;
      (commentLevel := !commentLevel+1; continue()))
fun yyAction5 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction6 (strm, lastMatch : yymatch) = (yystrm := strm;
      (stringText := !stringText ^ "\n"; continue()))
fun yyAction7 (strm, lastMatch : yymatch) = (yystrm := strm;
      (stringText := !stringText ^ "\t"; continue()))
fun yyAction8 (strm, lastMatch : yymatch) = (yystrm := strm;
      (stringText := !stringText ^ "\""; continue()))
fun yyAction9 (strm, lastMatch : yymatch) = (yystrm := strm;
      (stringText := !stringText ^ "\\"; continue()))
fun yyAction10 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (stringText := !stringText ^ fromCtrlChar yytext; continue())
      end
fun yyAction11 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (stringText := !stringText ^ toASCII(yypos, yytext); 
                 continue())
      end
fun yyAction12 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (lineNum := !lineNum+(newLines yytext); linePos := yypos :: !linePos;continue())
      end
fun yyAction13 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm; (stringText := !stringText ^ yytext; continue())
      end
fun yyAction14 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (lineNum := !lineNum+1; linePos := yypos :: !linePos;
                  ErrorMsg.error yypos ("illegal string character: '" ^ (String.toString yytext) ^ "'");
                  continue())
      end
fun yyAction15 (strm, lastMatch : yymatch) = (yystrm := strm;
      (YYBEGIN INITIAL; stringPartial := false; T.STRING( !stringText, !stringStart, yypos )))
fun yyAction16 (strm, lastMatch : yymatch) = (yystrm := strm;
      (ErrorMsg.error yypos ("illegal use of '\\' in string."); continue()))
fun yyAction17 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.error yypos ("illegal string character: " ^ yytext); continue())
      end
fun yyAction18 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue()))
fun yyAction19 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.COMMA(yypos,yypos+1)))
fun yyAction20 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.COLON(yypos,yypos+1)))
fun yyAction21 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.SEMICOLON(yypos,yypos+1)))
fun yyAction22 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.LPAREN(yypos,yypos+1)))
fun yyAction23 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RPAREN(yypos,yypos+1)))
fun yyAction24 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.LBRACK(yypos,yypos+1)))
fun yyAction25 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RBRACK(yypos,yypos+1)))
fun yyAction26 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.LBRACE(yypos,yypos+1)))
fun yyAction27 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.RBRACE(yypos,yypos+1)))
fun yyAction28 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.DOT(yypos,yypos+1)))
fun yyAction29 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.PLUS(yypos,yypos+1)))
fun yyAction30 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.MINUS(yypos,yypos+1)))
fun yyAction31 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.TIMES(yypos,yypos+1)))
fun yyAction32 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.DIVIDE(yypos,yypos+1)))
fun yyAction33 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.EQ(yypos,yypos+1)))
fun yyAction34 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.NEQ(yypos,yypos+2)))
fun yyAction35 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.LT(yypos,yypos+1)))
fun yyAction36 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.LE(yypos,yypos+2)))
fun yyAction37 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.GT(yypos,yypos+1)))
fun yyAction38 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.GE(yypos,yypos+2)))
fun yyAction39 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.AND(yypos,yypos+1)))
fun yyAction40 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.OR(yypos,yypos+1)))
fun yyAction41 (strm, lastMatch : yymatch) = (yystrm := strm;
      (T.ASSIGN(yypos,yypos+2)))
fun yyAction42 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (case find yytext of
                    SOME v => v(yypos,yypos+size yytext)
                  | _      => (T.ID(yytext, yypos, yypos+size yytext)))
      end
fun yyAction43 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (T.INT( valOf(Int.fromString yytext), yypos, yypos + size yytext))
      end
fun yyAction44 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction45 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.error yypos ("illegal character " ^ yytext); continue())
      end
fun yyQ49 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction27(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction27(strm, yyNO_MATCH)
      (* end case *))
fun yyQ48 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction40(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction40(strm, yyNO_MATCH)
      (* end case *))
fun yyQ47 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction26(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction26(strm, yyNO_MATCH)
      (* end case *))
fun yyQ46 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction25(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction25(strm, yyNO_MATCH)
      (* end case *))
fun yyQ45 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction24(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction24(strm, yyNO_MATCH)
      (* end case *))
fun yyQ50 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ50(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"`"
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ44 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction42(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"["
              then if inp = #":"
                  then yyAction42(strm, yyNO_MATCH)
                else if inp < #":"
                  then if inp <= #"/"
                      then yyAction42(strm, yyNO_MATCH)
                      else yyQ50(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                else if inp <= #"@"
                  then yyAction42(strm, yyNO_MATCH)
                  else yyQ50(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
            else if inp = #"`"
              then yyAction42(strm, yyNO_MATCH)
            else if inp < #"`"
              then if inp = #"_"
                  then yyQ50(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
                  else yyAction42(strm, yyNO_MATCH)
            else if inp <= #"z"
              then yyQ50(strm', yyMATCH(strm, yyAction42, yyNO_MATCH))
              else yyAction42(strm, yyNO_MATCH)
      (* end case *))
fun yyQ51 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction38(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction38(strm, yyNO_MATCH)
      (* end case *))
fun yyQ43 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction37(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ51(strm', yyMATCH(strm, yyAction37, yyNO_MATCH))
              else yyAction37(strm, yyNO_MATCH)
      (* end case *))
fun yyQ42 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction33(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction33(strm, yyNO_MATCH)
      (* end case *))
fun yyQ53 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction34(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction34(strm, yyNO_MATCH)
      (* end case *))
fun yyQ52 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction36(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction36(strm, yyNO_MATCH)
      (* end case *))
fun yyQ41 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction35(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #">"
              then yyQ53(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
            else if inp < #">"
              then if inp = #"="
                  then yyQ52(strm', yyMATCH(strm, yyAction35, yyNO_MATCH))
                  else yyAction35(strm, yyNO_MATCH)
              else yyAction35(strm, yyNO_MATCH)
      (* end case *))
fun yyQ40 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction21(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction21(strm, yyNO_MATCH)
      (* end case *))
fun yyQ54 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction41(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction41(strm, yyNO_MATCH)
      (* end case *))
fun yyQ39 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction20(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"="
              then yyQ54(strm', yyMATCH(strm, yyAction20, yyNO_MATCH))
              else yyAction20(strm, yyNO_MATCH)
      (* end case *))
fun yyQ55 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ55(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < #"0"
              then yyAction43(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ55(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ38 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction43(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ55(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
            else if inp < #"0"
              then yyAction43(strm, yyNO_MATCH)
            else if inp <= #"9"
              then yyQ55(strm', yyMATCH(strm, yyAction43, yyNO_MATCH))
              else yyAction43(strm, yyNO_MATCH)
      (* end case *))
fun yyQ56 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ37 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction32(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ56(strm', yyMATCH(strm, yyAction32, yyNO_MATCH))
              else yyAction32(strm, yyNO_MATCH)
      (* end case *))
fun yyQ36 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction28(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction28(strm, yyNO_MATCH)
      (* end case *))
fun yyQ35 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction30(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction30(strm, yyNO_MATCH)
      (* end case *))
fun yyQ34 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction19(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction19(strm, yyNO_MATCH)
      (* end case *))
fun yyQ33 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction29(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction29(strm, yyNO_MATCH)
      (* end case *))
fun yyQ57 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ32 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction31(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ57(strm', yyMATCH(strm, yyAction31, yyNO_MATCH))
              else yyAction31(strm, yyNO_MATCH)
      (* end case *))
fun yyQ31 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction23(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction23(strm, yyNO_MATCH)
      (* end case *))
fun yyQ30 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction22(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction22(strm, yyNO_MATCH)
      (* end case *))
fun yyQ29 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction39(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction39(strm, yyNO_MATCH)
      (* end case *))
fun yyQ28 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction18(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction18(strm, yyNO_MATCH)
      (* end case *))
fun yyQ58 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction44(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ58(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                  else yyAction44(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ58(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ27 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction44(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction44(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ58(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
                  else yyAction44(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ58(strm', yyMATCH(strm, yyAction44, yyNO_MATCH))
              else yyAction44(strm, yyNO_MATCH)
      (* end case *))
fun yyQ26 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction45(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction45(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ37(strm', lastMatch)
            else if inp < #"/"
              then if inp = #"&"
                  then yyQ29(strm', lastMatch)
                else if inp < #"&"
                  then if inp = #" "
                      then yyQ27(strm', lastMatch)
                    else if inp < #" "
                      then if inp = #"\n"
                          then yyQ21(strm', lastMatch)
                        else if inp < #"\n"
                          then if inp = #"\t"
                              then yyQ27(strm', lastMatch)
                              else yyQ26(strm', lastMatch)
                          else yyQ26(strm', lastMatch)
                    else if inp = #"\""
                      then yyQ28(strm', lastMatch)
                      else yyQ26(strm', lastMatch)
                else if inp = #"+"
                  then yyQ33(strm', lastMatch)
                else if inp < #"+"
                  then if inp = #")"
                      then yyQ31(strm', lastMatch)
                    else if inp < #")"
                      then if inp = #"'"
                          then yyQ26(strm', lastMatch)
                          else yyQ30(strm', lastMatch)
                      else yyQ32(strm', lastMatch)
                else if inp = #"-"
                  then yyQ35(strm', lastMatch)
                else if inp = #","
                  then yyQ34(strm', lastMatch)
                  else yyQ36(strm', lastMatch)
            else if inp = #"["
              then yyQ45(strm', lastMatch)
            else if inp < #"["
              then if inp = #"="
                  then yyQ42(strm', lastMatch)
                else if inp < #"="
                  then if inp = #";"
                      then yyQ40(strm', lastMatch)
                    else if inp < #";"
                      then if inp = #":"
                          then yyQ39(strm', lastMatch)
                          else yyQ38(strm', lastMatch)
                      else yyQ41(strm', lastMatch)
                else if inp = #"?"
                  then yyQ26(strm', lastMatch)
                else if inp < #"?"
                  then yyQ43(strm', lastMatch)
                else if inp <= #"@"
                  then yyQ26(strm', lastMatch)
                  else yyQ44(strm', lastMatch)
            else if inp = #"{"
              then yyQ47(strm', lastMatch)
            else if inp < #"{"
              then if inp = #"^"
                  then yyQ26(strm', lastMatch)
                else if inp < #"^"
                  then if inp = #"\\"
                      then yyQ26(strm', lastMatch)
                      else yyQ46(strm', lastMatch)
                else if inp <= #"`"
                  then yyQ26(strm', lastMatch)
                  else yyQ44(strm', lastMatch)
            else if inp = #"}"
              then yyQ49(strm', lastMatch)
            else if inp = #"|"
              then yyQ48(strm', lastMatch)
              else yyQ26(strm', lastMatch)
      (* end case *))
fun yyQ24 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ24(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ25 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"/"
              then yyQ25(strm', yyMATCH(strm, yyAction5, yyNO_MATCH))
              else yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"*"
              then yyQ22(strm', lastMatch)
            else if inp < #"*"
              then if inp = #"\n"
                  then yyQ21(strm', lastMatch)
                  else yyQ20(strm', lastMatch)
            else if inp = #"/"
              then yyQ23(strm', lastMatch)
              else yyQ20(strm', lastMatch)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction7(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction7(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction6(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction6(strm, yyNO_MATCH)
      (* end case *))
fun yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction10(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction10(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ15(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"@"
              then yyQ15(strm', lastMatch)
            else if inp < #"@"
              then yystuck(lastMatch)
            else if inp <= #"_"
              then yyQ15(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction9(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction9(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction11(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction11(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ17(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ17(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ16(strm', lastMatch)
            else if inp < #"0"
              then yystuck(lastMatch)
            else if inp <= #"9"
              then yyQ16(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction8(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction8(strm, yyNO_MATCH)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction12(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction12(strm, yyNO_MATCH)
      (* end case *))
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\^N"
              then yystuck(lastMatch)
            else if inp < #"\^N"
              then if inp = #"\v"
                  then yystuck(lastMatch)
                else if inp < #"\v"
                  then if inp <= #"\b"
                      then yystuck(lastMatch)
                      else yyQ8(strm', lastMatch)
                else if inp = #"\r"
                  then yyQ8(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"!"
              then yystuck(lastMatch)
            else if inp < #"!"
              then if inp = #" "
                  then yyQ8(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"\\"
              then yyQ18(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction16(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ10(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #"\^N"
                  then yyAction16(strm, yyNO_MATCH)
                else if inp < #"\^N"
                  then if inp = #"\v"
                      then yyAction16(strm, yyNO_MATCH)
                    else if inp < #"\v"
                      then if inp <= #"\b"
                          then yyAction16(strm, yyNO_MATCH)
                          else yyQ8(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                    else if inp = #"\r"
                      then yyQ8(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                      else yyAction16(strm, yyNO_MATCH)
                else if inp = #"!"
                  then yyAction16(strm, yyNO_MATCH)
                else if inp < #"!"
                  then if inp = #" "
                      then yyQ8(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                      else yyAction16(strm, yyNO_MATCH)
                else if inp = #"\""
                  then yyQ9(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp = #"_"
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"_"
              then if inp = #"\\"
                  then yyQ11(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                else if inp < #"\\"
                  then if inp <= #"9"
                      then yyQ10(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                      else yyAction16(strm, yyNO_MATCH)
                else if inp = #"]"
                  then yyAction16(strm, yyNO_MATCH)
                  else yyQ12(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
            else if inp = #"o"
              then yyAction16(strm, yyNO_MATCH)
            else if inp < #"o"
              then if inp = #"n"
                  then yyQ13(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
                  else yyAction16(strm, yyNO_MATCH)
            else if inp = #"t"
              then yyQ14(strm', yyMATCH(strm, yyAction16, yyNO_MATCH))
              else yyAction16(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction15(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction15(strm, yyNO_MATCH)
      (* end case *))
fun yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ19(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #" "
                  then yyQ19(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp = #"]"
              then yyQ19(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"]"
              then if inp = #"\\"
                  then yyAction13(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp <= #"~"
              then yyQ19(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction13(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"#"
              then yyQ19(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"#"
              then if inp = #" "
                  then yyQ19(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
                  else yyAction13(strm, yyNO_MATCH)
            else if inp = #"]"
              then yyQ19(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp < #"]"
              then if inp = #"\\"
                  then yyAction13(strm, yyNO_MATCH)
                  else yyQ19(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
            else if inp <= #"~"
              then yyQ19(strm', yyMATCH(strm, yyAction13, yyNO_MATCH))
              else yyAction13(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction14(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction14(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction17(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction17(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\""
              then yyQ6(strm', lastMatch)
            else if inp < #"\""
              then if inp = #"\v"
                  then yyQ3(strm', lastMatch)
                else if inp < #"\v"
                  then if inp = #"\n"
                      then yyQ4(strm', lastMatch)
                      else yyQ3(strm', lastMatch)
                else if inp = #" "
                  then yyQ5(strm', lastMatch)
                  else yyQ3(strm', lastMatch)
            else if inp = #"]"
              then yyQ5(strm', lastMatch)
            else if inp < #"]"
              then if inp = #"\\"
                  then yyQ7(strm', lastMatch)
                  else yyQ5(strm', lastMatch)
            else if inp <= #"~"
              then yyQ5(strm', lastMatch)
              else yyQ3(strm', lastMatch)
      (* end case *))
in
  (case (!(yyss))
   of STRING => yyQ0(!(yystrm), yyNO_MATCH)
    | COMMENT => yyQ1(!(yystrm), yyNO_MATCH)
    | INITIAL => yyQ2(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
