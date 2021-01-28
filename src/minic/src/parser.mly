%{
open Ast
open Printf

open Backend
  open AST
  open BinNumsExt
  open Ctypes
  open Cop
  module D = Datatypes
  open Integers
  open Language

%}

%token<string> NUM
%token SEMICOLON
%token COMMA
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token LPAREN
%token RPAREN
%token HASH
%token EQUAL
%token LARROW

%token ADD
%token SUB
%token MUL
%token DIV
%token MOD
%token EXP
%token AND
%token OR
%token XOR
%token SHL
%token SHR
%token EQ
%token NE
%token LT
%token GT
%token LE
%token GE

%token NOTBOOL
%token NOTINT

%token DOT

%token IF
%token ELSE
%token VOID
%token<string> INT
%token BOOL
%token PUBLIC
%token PRIVATE
%token WHILE
%token BREAK
%token RETURN

%token ADDRESS
%token ORIGIN
%token CALLER
%token CALLVALUE
%token COINBASE
%token TIMESTAMP
%token NUMBER

%token SHA1
%token SHA2

%token BALANCE
%token BLOCKHASH

%token TRANSFER
%token REVERT

%token UNION
%token STRUCT

%token EMIT
%token CALLMETHOD
%token MEMORY

%token<string> IDENT
%token EOF

%token CHAINID
%token SELFBALANCE

%token STRUCT_DEF
%token STRUCT_INSTANCE
%nonassoc STRUCT_INSTANCE
%nonassoc STRUCT_DEF

%nonassoc EQ NE LT LE GT GE
%left SHA2
%left OR
%left XOR
%left AND
%left MOD
%left SHL SHR
%left ADD SUB
%left MUL DIV
%left EXP
%right NOTBOOL

%start<Ast.p_file> file

%%

file:
  d=declaration* EOF { d }

declaration:
  | t = type_declaration { Ptype_decl t }
  | v = variable_declaration { Pvar_decl v }
  | f = function_definition { Pfunc f }

type_declaration:
  | t=struct_type_expression SEMICOLON { t }

variable_declaration:
  | v=variable SEMICOLON { v }

function_definition:
  | v=ioption(visibility) t=type_expression i=IDENT
    LPAREN p=separated_list(COMMA, variable) RPAREN
    LBRACE tmps=variable_declaration* body=statement* RBRACE {
      {
        p_fn_visibility = v;
        p_fn_name = i;
        p_fn_return = t;
        p_fn_params = p;
        p_fn_temps = tmps;
        p_fn_body = body;
      }}

visibility:
  | PUBLIC { Public }
  | PRIVATE { Private }

statement:
  | s=semicolon_statement SEMICOLON { s }
  | LBRACE s=statement RBRACE { s }
  | IF LPAREN e=expression RPAREN s1=then_clause s2=else_clause? { Pifthenelse (e, s1, s2) }
  | WHILE s=then_clause { Ploop s }


then_clause:
  | s=statement { [s] }
  | LBRACE s=statement* RBRACE { s }

else_clause:
  | ELSE s=then_clause { s }

semicolon_statement:
  | e1=expression EQUAL e2=expression { Passign (e1, e2) }
  | BREAK { Pbreak }
  | RETURN e=expression? { Preturn e }
  | TRANSFER LPAREN e1=expression COMMA e2=expression RPAREN { Ptransfer (e1, e2) }
  | REVERT { Prevert }
  | EMIT LPAREN e1=separated_list(COMMA, expression) SEMICOLON e2=separated_list(COMMA, expression) RPAREN { Plog (e1, e2) }
  | i=ioption(terminated(IDENT, LARROW)) func=IDENT LPAREN e=separated_list(COMMA, expression) RPAREN
    { Pcall (i, func, e) }
  | CALLMETHOD LPAREN
    addr=expression SEMICOLON
    ret=separated_list(COMMA, expression) SEMICOLON
    signature=expression SEMICOLON
    value=expression SEMICOLON
    gas=expression? SEMICOLON
    args=separated_list(COMMA, expression)
    RPAREN
    { Pcallmethod (addr, ret, signature, value, gas, args) }

expression:
  | LPAREN e=expression RPAREN { e }
  | i=IDENT { Pvar i }
  | n=NUM { Pconst n }
  | MUL e=expression { Pderef e } %prec NOTBOOL
  | u=unop e=expression { Punop (u, e) } %prec NOTBOOL
  | e1=expression b=binop e2=expression { Pbinop (e1, b, e2) }
  | e1=expression LBRACKET e2=expression RBRACKET { Parrayderef (e1, e2) }
  | b=builtin0 { Pcall0 b }
  | b=builtin1 LPAREN e=expression RPAREN { Pcall1 (b, e) }
  | e=expression DOT i=IDENT { Pfield (e, i) }

%inline binop:
  | ADD { Oadd }
  | SUB { Osub }
  | MUL { Omul }
  | DIV { Odiv }
  | MOD { Omod }
  | EXP { Oexp }
  | AND { Oand }
  | OR { Oor }
  | XOR { Oxor }
  | SHL { Oshl }
  | SHR { Oshr }
  | EQ { Oeq }
  | NE { One }
  | LT { Olt }
  | GT { Ogt }
  | LE { Ole }
  | GE { Oge }
  | SHA2 { Osha_2 }

type_expression:
  | VOID { Pvoid }
  | BOOL { Pbool }
  | i=INT { Pint i }
  | t=type_expression LBRACKET n=NUM RBRACKET { Parray (t, int_of_string n) }
  | t1=type_expression HASH t2=delimited(LBRACKET, type_expression, RBRACKET)
      { Pmapping (t1, t2) }
  | t=type_expression MUL { Ppointer t }
  | t=struct_type_expression { t } %prec STRUCT_DEF
  | STRUCT i=IDENT { Puserstruct i } %prec STRUCT_INSTANCE
  | UNION i=IDENT { Puserstruct i } %prec STRUCT_INSTANCE


struct_type_expression:
  | STRUCT i=IDENT LBRACE v=variable_declaration* RBRACE { Pstruct (i, v) }
  | UNION i=IDENT LBRACE v=variable_declaration* RBRACE { Punion (i, v) }

variable:
  | m=ioption(MEMORY) t=type_expression i=IDENT { Option.is_some m, t, i }

unop:
  | NOTBOOL { Onotbool }
  | NOTINT { Onotint }
  | SUB { Oneg }
  | SHA1 { Osha_1 }

builtin0:
  | ADDRESS { Baddress }
  | ORIGIN { Borigin }
  | CALLER { Bcaller }
  | CALLVALUE { Bcallvalue }
  | COINBASE { Bcoinbase }
  | TIMESTAMP { Btimestamp }
  | NUMBER { Bnumber }
  | CHAINID { Bchainid }
  | SELFBALANCE { Bselfbalance }

builtin1:
  | BALANCE { Bbalance }
  | BLOCKHASH { Bblockhash }
