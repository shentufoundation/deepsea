%{ open Ast
%}

%token SIGNATURE STROAGE EVENT OF METHOD CONSTRUCTOR GUARD EFFECTS LOGS RETURNS MAP UINTType STORAGE
%token ASSIGN ARROW MAPASSIGN COLON SEMI PASSIGN COMMA POINT
%token LBRACE RBRACE LPAREN RPAREN LBRACK RBRACK 
%token EQ NEQ LGT ADD SUB MUL DIVIDE AND OR BOOL LGTEQ RGTEQ RGT
%token INT
%token <int> NUMLITERAL 
%token <string> ID ADDRESSTYPE END STRLIT UINTTYPE 
%token <string> UNIT ENVIRONMENT VOID
%token <bool> BooLit
%token EOF

%start program
%type <Ast.program> program



/* %left OR
%left AND

%left LT  */
%right PASSIGN
%left EQ NEQ LGT LGTEQ RGTEQ RGT 
%left OR AND

%left ADD SUB
%left MUL DIVIDE

%%

program:
	 defs EOF {$1}

defs:
   /* nothing */ 
	| interfacedecl implementationdecl { $1, $2 }


interfacedecl:
	SIGNATURE id_ok LBRACE interfaceBody_list RBRACE
	{
		{
			signaturename = $2;
			interfacebody =  $4
		}
	}

interfaceBody_list:
		{ [] }
	|interfaceBody interfaceBody_list { $1::$2 }


interfaceBody:
	| STORAGE ID COLON type_ok SEMI {TypeAssigndecl (Id($2), $4)}
	| MAP ID COLON LPAREN type_list RPAREN MAPASSIGN type_ok SEMI{MapAssigndecl (Id($2), Mapstruct($5, $8))}
	| EVENT ID ASSIGN ID OF LPAREN type_list RPAREN SEMI {Eventdecl (Id($2), $7)}
	| CONSTRUCTOR ID COLON LPAREN type_list RPAREN ARROW type_ok SEMI{Constructordecl (Id($2), $5, $8)}
	| METHOD ID COLON LPAREN type_list RPAREN ARROW type_ok SEMI{Methodecls (Id($2), $5, $8)} 


arg_list:
  /*nothing*/ { [] }
	| argument { [$1] }
  | argument COMMA arg_list  { $1 :: $3 }

argument:
	| ID {Id($1)}
	| ENVIRONMENT POINT ID {EnvLit($1, $3)}

/* (owner : Address, spender : Address) */
param_list:
  /*nothing*/ { [] }
	| param { [$1] }
  | param COMMA param_list  { $1 :: $3 }


param:
  ID COLON type_ok { Var(Id($1), $3) }

id_ok:
	| ID {Id($1)}

expr_list:
  /*nothing*/ { [] }
	| expr { [$1] }
  | expr SEMI expr_list  { $1 :: $3 }

expr:
	| NUMLITERAL { NumLit($1) }
  | BooLit { BoolLit($1) }
	| ID LBRACK arg_list RBRACK {Mapexpr(Id($1), $3)}
	| ID {Id($1)}
	| VOID {Voidlit($1) }
	| ENVIRONMENT POINT ID {EnvLit($1, $3)}
	| expr ADD expr   {Binop ($1, Add, $3) }
	| expr SUB expr   {Binop ($1, Sub, $3) }
	| expr MUL expr   {Binop ($1, Times, $3) }
	| expr DIVIDE expr   {Binop ($1, Divide, $3) }
	| expr OR expr   {Binop ($1, Or, $3) }
	| expr AND expr   {Binop ($1, And, $3) }
	| expr LGT expr  {Comparsion ($1, LGT, $3)}
	| expr EQ expr  {Comparsion ($1, Equal, $3)}
	| expr NEQ expr  {Comparsion ($1, Neq, $3)}
	| expr RGT expr  {Comparsion ($1, RGT, $3)}
	| expr LGTEQ expr  {Comparsion ($1, LGTEQ, $3)}
	| expr RGTEQ expr  {Comparsion ($1, RGTEQ, $3)}
	| expr PASSIGN expr { Storageassign($1, $3) }
	| LPAREN expr RPAREN {$2}



type_list:
  /*nothing*/ { [] }
	|	type_ok {[$1]}
	| type_ok COMMA type_list { $1 :: $3 }

type_ok:
    INT   { Int  }
   | UINTTYPE { Uint($1) }
   | BOOL  { Bool }
   | ADDRESSTYPE {Address($1)}
   | UNIT { Void($1) }



implementationdecl:
	constructordecl methoddecls
	{
		{
			consturctor = $1;
			methods =  $2
		}
	}

constructordecl:
	CONSTRUCTOR id_ok LPAREN param_list RPAREN LBRACE STORAGE expr_list RETURNS type_ok SEMI RBRACE
	{
		{
			name = $2;
			params = $4;
			consturctor_body = $8;
			return_type = $10;
		}
	}

methoddecls:
		{ [] }
	|	methoddecl methoddecls {$1 :: $2 }

methoddecl:
	METHOD id_ok LPAREN param_list RPAREN LBRACE 
	GUARD LBRACE expr_list RBRACE 
	STORAGE LBRACE expr_list RBRACE
	EFFECTS LBRACE effects_bodylist RBRACE
	RETURNS expr SEMI RBRACE
	{
		{
			methodname = $2;
			params = $4;
			guard_body = $9;
			storage_body = $13;
			effects_body = $17;
			returns = $20;
		}
	}


effects_bodylist:
		{ [] }
	|effects_body effects_bodylist { $1::$2 }

effects_body:
	| LOGS id_ok LPAREN arg_list RPAREN SEMI { Logexpr($2, $4) } 

