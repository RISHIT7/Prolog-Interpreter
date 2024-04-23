{
    open Parser;;
    exception InvalidToken of string;;
}

let alpha_num = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']
let var = ['A'-'Z'](alpha_num*)
let cons = ['a'-'z'](alpha_num*) | ("\"" [^ '\"']+ "\"" )
(* the latter part matches a string [^'\"'] matches one or more characters that are not double quotes *)
let skip = [' ' '\t' '\n' '\r']+
let number = '0'|['1'-'9']['0'-'9']*

rule token = parse
    | skip
        { token lexbuf }
    | ['+']
        { PLUS }
    | ['-']
        { MINUS }
    | ['*']
        { ASTERISK }
    | ['/']
        { SLASH }
    | var as lxm
        { IDENT(lxm) }
    | cons as lxm
        { CONS(lxm) }
    | number as lxm
        { INTEGER(int_of_string lxm) }
    | ['(']
        { LEFTBRACKET }
    | [')']
        { RIGHTBRACKET }
    | ['[']
        { LEFTSQUARE }
    | [']']
        { RIGHTSQUARE }
    | [',']
        { COMMA }
    | ['=']
        { ASSIGN }
    | ['<']
        { LESSTHAN }
    | ['>']
        { GREATERTHAN }
    | "!="
        { NOTEQUAL }
    | ['|']
        { PIPE }
    | ['!']
        { BANG }
    | ['.']
        { STOP }
    | ":-"
        { COND }
    | ['%']
        { oneLineComment lexbuf }
    | ['_']
        { UNDERSCORE }
    | "/*"
        {multiLineComment 0 lexbuf }
    | _ as lxm
        {raise (InvalidToken (String.make 1 lxm))}
    | eof
        { EOF }

and oneLineComment = parse
      eof
        { EOF }
    | ['\n']
        {token lexbuf}
    | _
        {oneLineComment lexbuf}

and multiLineComment depth = parse
      eof
        { failwith "Syntax error" }
    | "*/"
        { if depth = 0 then token lexbuf 
          else multiLineComment (depth-1) lexbuf }
    | "/*"
        { multiLineComment (depth+1) lexbuf }
    | _
        { multiLineComment depth lexbuf }