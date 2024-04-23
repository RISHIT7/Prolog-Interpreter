%{
    open Interpreter;;
%}

%token LEFTBRACKET RIGHTBRACKET LEFTSQUARE RIGHTSQUARE
%token <string> IDENT CONS
%token <int> INTEGER
%token COMMA ASSIGN NOTEQUAL STOP BANG PIPE PLUS MINUS ASTERISK SLASH LESSTHAN GREATERTHAN COND PERCENTAGE COMMENTOPEN COMMENCLOSE UNDERSCORE
%token EOF

%left COMMA
%nonassoc EQUAL PIPE LESSTHAN GREATERTHAN NOTEQUAL
%left PLUS MINUS
%left ASTERISK SLASH
%nonassoc STOP

%start program goal
%type <Interpreter.program> program
%type <Interpreter.goal> goal
%%

program:
      EOF
        {[]}
    | clause_list EOF
        {$1}
;

clause_list:
      clause
        {[$1]}
    | clause clause_list
        {($1)::$2}
;

clause:
      atom STOP
        {Fact(Head($1))}
    | atom COND atom_list STOP
        {Rule(Head($1), Body($3))}
;

goal:
      atom_list STOP
        {Goal($1)}
;

atom_list:
      atom                                
        {[$1]}
    | atom COMMA atom_list                
        {($1)::$3}
;

atom:
    | CONS                                
        {Atom($1, [])}
    | CONS LEFTBRACKET term_list RIGHTBRACKET                
        {Atom($1, $3)}
    | term ASSIGN term                        
        {Atom("_equal", [$1; $3])}
    | term NOTEQUAL term                    
        {Atom("_not_equal", [$1; $3])}
    | term LESSTHAN term                        
        {Atom("<", [$1; $3])}
    | term GREATERTHAN term                        
        {Atom(">", [$1; $3])}
    | BANG                                 
        {Atom("_ofcourse", [])}
;


term_list:
      term                                
        {[$1]}
    | term COMMA term_list                
        {($1)::$3}
;


term:
      LEFTBRACKET term RIGHTBRACKET                          
        {$2}
    | IDENT                                 
        {V($1)}
    | CONS                                
        {Node($1, [])}
    | INTEGER                                 
        {N($1)}
    | UNDERSCORE
        {Underscore}
    | CONS LEFTBRACKET term_list RIGHTBRACKET                
        {Node($1, $3)}
    | term PLUS term                      
        {Node("+", [$1; $3])}
    | term MINUS term                     
        {Node("-", [$1; $3])}
    | term ASTERISK term                      
        {Node("*", [$1; $3])}
    | term SLASH term                       
        {Node("/", [$1; $3])}
    | list                                
        {$1}
;


list:
      LEFTSQUARE RIGHTSQUARE                               
        {Node("_empty_list", [])}
    | LEFTSQUARE list_body RIGHTSQUARE                     
        {$2}
;


list_body:
      term                                 
        {Node("_list", [$1; Node("_empty_list", [])])}
    | term COMMA list_body                 
        {Node("_list", [$1; $3])}
    | term PIPE term                       
        {Node("_list", [$1; $3])}
;