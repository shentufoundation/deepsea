#include "config.h"

%{
open Astcommon
open Parsetree

let make_loc (startpos, endpos) = {
  loc_start = startpos;
  loc_end = endpos;
}

let mkexp_ ~loc d =  
  { p_expression_desc = d; p_expression_loc = make_loc loc; }
let mkcmd ~loc d = 
  { p_command_desc = d; p_command_loc = make_loc loc; }
let mkfotyp ~loc d = 
  { p_type_FO_desc = d; p_type_FO_loc = make_loc loc; }
let mksig ~loc d = 
  { p_signature_desc = d; p_signature_loc = make_loc loc; }
let mklayersign ~loc d = 
  { p_layer_signature_desc = d; p_layer_signature_loc = make_loc loc; }
let mkobjtyp ~loc objb objs = 
  { pObjectBase = objb; pObjectSignature = objs; pObjectTypLoc = make_loc loc; }
let mklayertyp ~loc plbase plsig = 
  { pLayerBase = plbase; pLayerSignature = plsig; pLayerLoc = make_loc loc }
let mkmethdef ~loc args rtyp kind body annos =
  { pMethodArguments = args; pMethodReturnType = rtyp; pMethodKind = kind; pMethodBody = body; pMethodAnnotations = annos; pMethodLoc = make_loc loc; }
let mkobjconstr ~loc objtyp objk objfs objms = 
  { pObjType = objtyp; pObjKind = objk; pObjFields = objfs; pObjMethods = objms; pObjLoc = make_loc loc; }
let mkobj ~loc d = 
  { p_object_desc = d; p_object_loc = make_loc loc; }
let mkobjdef ~loc objtyp obj = 
  { pObjectType = objtyp; pObjectDesc = obj; pObjectLoc = make_loc loc; }
let mkobjinst ~loc objinstdesc =
  { p_object_inst_desc = objinstdesc; p_object_inst_loc = make_loc loc; }
let mkprop ~loc d = 
  { p_proposition_desc = d; p_proposition_loc = make_loc loc;}
let mklayer ~loc d = 
  { p_layer_desc = d; p_layer_loc = make_loc loc; }
let mklayerdef ~loc layer invar annos = 
  { pLayerDesc = layer; pLayerInvariant = invar; pLayerAnnotations = annos; pLayerLoc = make_loc loc}
let mkdecl ~loc d = 
  { p_declaration_desc = d; p_declaration_loc = make_loc loc; }

(* let mkanno ~loc d =
  { p_annotation_desc = d; p_annotation_loc = make_loc loc; } *)

let builtin_type_table =
  let tbl = Hashtbl.create 20 in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) [
    "unit", (PTbuiltin Tunit);
    "bool", (PTbuiltin Tbool);
    "int", (PTbuiltin Tint);
    "uint", (PTbuiltin Tuint);
    "hashvalue", (PTbuiltin Thashvalue);
    "globalpointer", (PTbuiltin Tglobalpointer) (*;
    "val", PTbuiltin Tval;
    "flatmem", PTbuiltin Tflatmem*)
  ];
  tbl

let constant_table =
  let tbl = Hashtbl.create 20 in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) [
    "true", PEconstant (CONbool true);
    "false", PEconstant (CONbool false);
    "null_hash", PEconstant CONhashvalue;
    (*"array_init", PEconstant CONarray_init;*)
    "GLOBUndef", PEconstant CONglobalpointer_undef (*;
    "Vundef", PEconstant CONval_undef;
    "flatmem_empty", PEconstant CONflatmem_empty*)
  ];
  tbl

let either_to_double_list = function
  | `Left a -> [a], []
  | `Right b -> [], [b]
let cons_either_double_list e (l1, l2) = match e with
  | `Left a -> a :: l1, l2
  | `Right b -> l1, b :: l2
%}

/* Tokens */

%token ARRAY
%token MAPPING 
%token CONST
%token CONSTRUCTOR
%token EOF
%token EXTERNAL
%token GHOST
%token <Astcommon.ident> IDENT
%token <string> INT
%token <string> UINT
%token LAYER
%token LOGICAL
%token REFINED
%token OBJECT
/* %token OF */
%token SIGNATURE
%token <string> STRING
%token TRUSTED
%token TYPE
%token EVENT
#ifdef ANT
%token IDENTITY
#else
%token ADDRESS
#endif
%token INDEXED
%token EMIT

%token ASSERT
%token BEGIN
%token DENY
%token DO
%token ELSE
%token END
%token FAIL
%token FIRST
%token FOLD
%token FOR
%token IF
%token IN
%token LET
%token MATCH
%token MOD
/* %token SKIP */
%token THEN
%token TO
%token WITH
%token CLONE
%token TRANSFERETH

%token AT
%token ARROW
%token ASSIGN
%token BANG
%token BITNOT
%token BAR
%token BARBAR       
%token COLON
%token COLONCOLON       
%token COLONGREATER
%token COLONLESS
%token COMMA
%token CONJUNCTION
%token DISJUNCTION
%token DOT
%token DOUBLEARROW
%token EQUAL
%token GREATER
%token GREATEREQ
%token LBRACE
%token LBRACKET
%token LESS
%token LESSEQ
%token LPAREN
%token MINUS
%token PLUS
%token RBRACE
%token RBRACKET
%token RPAREN
%token SEMICOLON
%token STAR
%token SLASH
%token UNEQUAL
%token BITAND
%token XOR
%token SHL
%token SHR
%token LIST
       
/* layer calculus level precedences */
%right AT
%right COLONGREATER
%nonassoc COLON

/* object and command level precedences */
%nonassoc IN /* WITH */
%nonassoc BAR 
%nonassoc below_SEMICOLON
%nonassoc SEMICOLON  /* no longer relevant: below EQUAL: { f x = ...; g x = ... } */
%nonassoc LET  /* above SEMICOLON: ...; let ... in ... */
/*
%nonassoc ASSERT DENY
%nonassoc LET MATCH
*/
%nonassoc DO      /* below THEN (first ... = ... to ... do ...) */
%nonassoc THEN    /* below ELSE (first ... = ... to ... do ... then ...)
                            and (if ... then ...) */
%nonassoc ELSE    /* (first ... = ... to ... do ... then ... else ...)
                 and (if ... then ... else ...) */

/* type and expression level precedences */
/* (* XXX: rest of the precedences *) */
/* %right COMMA */
%right DISJUNCTION
%right CONJUNCTION
%nonassoc EQUAL UNEQUAL LESS LESSEQ GREATER GREATEREQ
%left BARBAR
%left XOR
%left BITAND
%left SHL SHR
%left PLUS MINUS
%left STAR SLASH MOD
%nonassoc above_STAR  /* parse [array[n] int * int] as [(array[n] int) * int] */
%nonassoc prec_unary_prefix
%nonassoc below_DOT
%left DOT LBRACKET

%start file
%type<Parsetree.p_file_structure> file

%%

/* Top level */
file:
    declarations EOF  { List.rev $1 }
;

declarations:
      { [] }
  | declarations declaration  { $2 :: $1 }
  | declarations events_declaration { $2 @ $1 }
  | declarations const_declaration  { $1 }
  | declarations external_declaration  { $2 :: $1 }
;
declaration:
    type_declaration  { fst $1, mkdecl ~loc:$sloc (PDtype (snd $1)) }
  | object_signature_declaration  { fst $1, mkdecl ~loc:$sloc (PDsignature (snd $1)) }
  | layer_signature_declaration  { fst $1, mkdecl ~loc:$sloc  (PDlayer_sig (snd $1)) }
  | object_declaration  { fst $1, mkdecl ~loc:$sloc (PDobject (snd $1)) }
  | layer_declaration  { fst $1, mkdecl ~loc:$sloc (PDlayer (snd $1)) }
;
const_declaration:
    CONST IDENT EQUAL e=expression
      { Hashtbl.add constant_table $2 e.p_expression_desc }
;
external_declaration:
    EXTERNAL annotations WITH STRING
      { "", mkdecl ~loc:$sloc (PDexternal_with ($4, $2)) }
  | EXTERNAL TYPE IDENT annotations eq_or_assign STRING
      { $3, mkdecl ~loc:$sloc (PDexternal_type ($6, None, $4)) }
  | EXTERNAL TYPE IDENT annotations eq_or_assign STRING STRING
      { $3, mkdecl ~loc:$sloc (PDexternal_type ($6, Some $7, $4)) }
  | EXTERNAL LET IDENT annotations COLON type_expression eq_or_assign STRING
      { $3, mkdecl ~loc:$sloc (PDexternal_const ($8, $6, $4)) }
  | EXTERNAL LET IDENT annotations COLON type_expression ARROW type_expression
        eq_or_assign STRING
      { $3, mkdecl ~loc:$sloc (PDexternal_function ($10, $6, $8, $4)) }
  | EXTERNAL ASSERT IDENT COLON type_expression eq_or_assign STRING
      { $3, mkdecl ~loc:$sloc (PDexternal_prop ($7, $5)) }
;

/* Types */
type_declaration:
    TYPE IDENT ASSIGN te=type_expression
      {   $2,  mkfotyp ~loc:$sloc te.p_type_FO_desc }
  | TYPE IDENT annotations EQUAL type_definition
      { $2, mkfotyp ~loc:$sloc (PTdata ($5, $3)) }
;

type_expression:
    IDENT
      { let x = try Hashtbl.find builtin_type_table $1
        with Not_found -> PTname $1
        in 
        mkfotyp ~loc:$sloc (x)  
       }
#ifdef ANT
  | IDENTITY { mkfotyp ~loc:$sloc (PTbuiltin Taddress) }
#else
  | ADDRESS { mkfotyp ~loc:$sloc (PTbuiltin Taddress) }
#endif
  | LPAREN type_expression RPAREN
      { $2 }
  | type_expression STAR type_expression
      { mkfotyp ~loc:$sloc (PTprod ($1, $3)) }
  | ARRAY LBRACKET e=expression RBRACKET type_expression %prec above_STAR
      { mkfotyp ~loc:$sloc (PTarray (int_of_p_expression e.p_expression_desc, $5)) }
  | MAPPING LBRACKET type_expression RBRACKET type_expression
      { mkfotyp ~loc:$sloc (PTmapping ($3, $5)) }
  | LIST type_expression
      { mkfotyp ~loc:$sloc (PTlist $2) }
;

type_definition:
    LBRACE field_declarations opt_semi RBRACE  { PTsingleton (List.rev $2) }
  | opt_bar constructor_declarations           { PTbranches (List.rev $2) }
;

field_declarations:
    field_declaration  { [$1] }
  | field_declarations SEMICOLON field_declaration  { $3 :: $1 }
;
field_declaration:
    IDENT COLON type_expression  { $1, $3 }
;

constructor_declarations:
    constructor_declaration  { [$1] }
  | constructor_declarations BAR constructor_declaration  { $3 :: $1 }
;
constructor_declaration:
    IDENT constructor_params annotations
      { { pTypeConstrName = $1;
          pTypeConstrArgs = $2;
          pTypeConstrAnnotations = $3;
          pTypeConstrLoc = (make_loc $sloc ) } }
;
constructor_params:
 constructor_parameters
      { List.fold_left (fun a b -> List.rev_append b a) [] $1 }
;
constructor_parameters:
      { [] }
  | constructor_parameters constructor_parameter  { $2 :: $1 }
;
constructor_parameter:
    LPAREN idents COLON type_expression RPAREN
      { List.map (fun i -> i, $4) $2 }
;
idents:
    IDENT  { [$1] }
  | idents IDENT  { $2 :: $1 }
;

events_declaration :
    EVENT opt_bar event_declarations { $3 }
;

event_declarations:
    event_declaration  { [$1] }
  | event_declarations BAR event_declaration  { $3 :: $1 }

;
event_declaration:
    IDENT event_parameters
      { ($1, mkdecl ~loc:$sloc (PDevent (List.rev $2))) }

event_parameters:
      { [] }
  | event_parameters event_parameter  { $2 :: $1 }
;

indexed_opt :
  { false }
| INDEXED { true }
;

event_parameter:
  LPAREN IDENT COLON type_expression indexed_opt RPAREN
      { ($2, $4, $5) }
;

/* Object Signature */
object_signature_declaration:
    SIGNATURE IDENT EQUAL object_signature_expr
      { $2, $4 }
  | opt_logical_or_trusted  /* allowing factorization with object_declaration */
    OBJECT SIGNATURE IDENT EQUAL object_signature_expr
      { if $1 <> POnormal then
          print_endline "Warning: object signatures should not be marked logical or trusted";
        $4, $6 }
;
object_signature_expr:
    IDENT  { mksig ~loc:$sloc (PSname $1) }
  | LBRACE RBRACE  { mksig ~loc:$sloc (PSconstr []) }
  | LBRACE object_signature_fields opt_semi RBRACE  { mksig ~loc:$sloc (PSconstr (List.rev $2)) }
  | object_signature_expr WITH GHOST IDENT  { mksig ~loc:$sloc (PSghostize ($1, [$4])) }
  | object_signature_expr WITH GHOST LBRACE idents_semi RBRACE  { mksig ~loc:$sloc (PSghostize ($1, $5)) }
  | object_signature_expr WITH LOGICAL IDENT  { mksig ~loc:$sloc (PSlogicize ($1, [$4])) }
  | object_signature_expr WITH LOGICAL LBRACE idents_semi RBRACE  { mksig ~loc:$sloc (PSlogicize ($1, $5)) }
  | object_signature_expr MINUS IDENT  { mksig ~loc:$sloc (PSminus ($1, [$3])) }
  | object_signature_expr MINUS LBRACE idents_semi RBRACE  { mksig ~loc:$sloc (PSminus ($1, $4)) }
object_signature_fields:
    object_signature_field  { [$1] }
  | object_signature_fields SEMICOLON object_signature_field  { $3 :: $1 }
;
object_signature_field:
    method_kind IDENT COLON type_expression ARROW type_expression
      { $2, $4, $6, $1 }
  | CONSTRUCTOR COLON type_expression ARROW type_expression
      {  "constructor", $3, $5, MKconstructor  }
;
method_kind:
             { MKnormal }
  | CONST    { MKconst }
  | GHOST    { MKghost }
  | CONST GHOST { MKconstghost }
  | GHOST CONST { MKconstghost }
  | LOGICAL  { MKlogical }
  | REFINED { MKrefined }
;
idents_semi:
    idents_semi_sep opt_semi  { List.rev $1 }
;
idents_semi_sep:
    IDENT  { [$1] }
  | idents_semi_sep SEMICOLON IDENT  { $3 :: $1 }
;

/* Layer Signature */
layer_signature_declaration:
    LAYER SIGNATURE IDENT EQUAL layer_signature
      { $3, $5 }
;
layer_signature:
    IDENT  { mklayersign ~loc:$sloc (PLSname $1) }
  | LBRACE RBRACE  { mklayersign ~loc:$sloc (PLSconstr []) }
  | LBRACE layer_signature_fields opt_semi RBRACE  { mklayersign ~loc:$sloc (PLSconstr (List.rev $2)) }
layer_signature_fields:
    layer_signature_field  { [$1] }
  | layer_signature_fields SEMICOLON layer_signature_field  { $3 :: $1 }
;
layer_signature_field:
    IDENT COLON object_signature_expr  { $1, $3 }
;

/* Object and layer types */
layer_type:
    LBRACKET layer_signature RBRACKET layer_signature
      { { pLayerBase = $2; pLayerSignature = $4; pLayerLoc =(make_loc $sloc) } }
  | LBRACKET RBRACKET layer_signature
      { { pLayerBase = mklayersign ~loc:$sloc (PLSconstr []); pLayerSignature = $3; pLayerLoc=(make_loc $sloc) } }
;

/* Expressions and commands */

  /* XXX: LR(1) does not look ahead enough to correctly decide between parse
          trees

       command -> IDENT DOT IDENT e    (primitive call)

     and

       command -> expression
               -> expression_atom_head
               -> expression_atom DOT IDENT   (struct field access)
               -> IDENT DOT IDENT

     Default behavior: shift DOT in command over reduce expression_atoms ->
     IDENT, thus the former.
  */
  /* Harmless parsing ambiguity introduced by allowing [assert (c)] as in
     [assert (let x = pe in !x)]: [assert (e)] can be parsed as

       command -> ASSERT command
               -> ASSERT expression
               -> ASSERT LPAREN expression LPAREN

     or

       command -> ASSERT LPAREN command RPAREN
               -> ASSERT LPAREN expression LPAREN

     Either way, the generated parse tree is always [PCassert (PCyield e)],
     hence harmless.  Default behavior is the former. */
  /* NOTE: Fixed on 3/17/2015 by restricting [expression] to not accept
           structure construction, adding a new [expression_or_structure]
           non-terminal, and changing assignment rule

       command -> expression ASSIGN expression

     into

       command -> expression ASSIGN expression_or_structure

     which is the only usage for structure construct anyway.

     XXX: Ambiguous grammar on input [{ x = e1; y = e2 }]:

       command -> LBRACE commands RBRACE
               -> LBRACE command SEMICOLON command RBRACE
               -> LBRACE expression SEMICOLON expression RBRACE
               -> LBRACE expression EQUAL expression SEMICOLON
                         expression EQUAL expression RBRACE
               -> LBRACE IDENT expression_atoms EQUAL expression SEMICOLON
                         IDENT expression_atoms EQUAL expression RBRACE
               -> LBRACE IDENT EQUAL expression SEMICOLON
                         IDENT EQUAL expression RBRACE

     producing [PCsequence (PCyield (PEbin (OPeq, PEglob "x", e1)),
                            PCyield (PEbin (OPeq, PEglob "y", e2)))], or

       command -> expression
               -> expression_atom_head
               -> LBRACE struct_fields RBRACE
               -> LBRACE struct_fields SEMICOLON struct_field RBRACE
               -> LBRACE struct_field SEMICOLON struct_field RBRACE
               -> LBRACE IDENT EQUAL expression SEMICOLON
                         IDENT EQUAL expression RBRACE

     producing [PCyield (PEstruct ["x", e1; "y", e2])].

     Default behavior: shift EQUAL in struct_field over reduce
     expression_atoms -> epsilon, thus the latter.
  */

atom:
    IDENT  { let x = try Hashtbl.find constant_table $1 with Not_found -> PEglob $1 in  mkexp_ ~loc:$sloc (x) }
  | INT  {  mkexp_ ~loc:$sloc (PEconstant (CONint (int_of_string($1)))) }
  | UINT  {  mkexp_ ~loc:$sloc (PEconstant (CONuint (int_of_string($1)))) }
#ifdef ANT
  | IDENTITY LPAREN INT RPAREN {  mkexp_ ~loc:$sloc (PEconstant (CONaddress $3)) }
  | IDENTITY LPAREN UINT RPAREN {  mkexp_ ~loc:$sloc (PEconstant (CONaddress $3)) }
#else
  | ADDRESS LPAREN INT RPAREN {  mkexp_ ~loc:$sloc (PEconstant (CONaddress $3)) }
  | ADDRESS LPAREN UINT RPAREN {  mkexp_ ~loc:$sloc (PEconstant (CONaddress $3)) }
#endif
  | LPAREN RPAREN  {  mkexp_ ~loc:$sloc (PEconstant CONunit) }
  | LPAREN comma_sep_expressions RPAREN  { $2 }
  | a=atom LBRACKET e=expression RBRACKET  {  mkexp_ ~loc:$sloc (PEindex (a, e)) }
  (* | atom LPAREN atom RPAREN { mkexp_ ~loc:$sloc (PEpair (a1, a2))  } *)
  (* | atom DOT IDENT  {  mkexp_ ~loc:$sloc (PEfield ($1, $3)) } *)
;

expression:
  | a=atom+ { match a with
      | [hd] -> hd
      | _ -> mkexp_ ~loc:$sloc (PEapp a)
    }
  | a1=atom+ DOT a2=atom+ { let x = mkexp_ ~loc:$sloc (PEfield (a1, a2)) in
      (* print_endline ("PEfield: " ^ (string_of_p_expression x)); *) x
    }
  | MINUS e=expression %prec prec_unary_prefix  {  mkexp_ ~loc:$sloc (PEun (OPneg, e)) }
  | BANG e=expression %prec prec_unary_prefix  {  mkexp_ ~loc:$sloc (PEun (OPnot, e)) }
  | BITNOT e=expression %prec prec_unary_prefix  {  mkexp_ ~loc:$sloc (PEun (OPbitnot, e)) }
  | e1=expression BARBAR e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPbitor, e1, e2 )) }
  | e1=expression XOR e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPxor, e1, e2 )) }
  | e1=expression BITAND e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPbitand, e1, e2 )) }
  | e1=expression SHL e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPshl, e1, e2 )) }
  | e1=expression SHR e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPshr, e1, e2)) }
  | e1=expression PLUS e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPplus, e1, e2 )) }
  | e1=expression MINUS e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPminus, e1, e2 )) }
  | e1=expression STAR e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPtimes, e1, e2 )) }
  | e1=expression SLASH e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPdivide, e1, e2 )) }
  | e1=expression MOD e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPremainder, e1, e2 )) }
  | e1=expression CONJUNCTION e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPand, e1, e2 )) }
  | e1=expression DISJUNCTION e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPor,e1, e2 )) }
  | e1=expression EQUAL e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPeq, e1, e2 )) }
  | e1=expression UNEQUAL e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPne, e1, e2 )) }
  | e1=expression LESS e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPlt, e1, e2 )) }
  | e1=expression LESSEQ e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPle, e1, e2 )) }
  | e1=expression GREATER e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPgt, e1, e2 )) }
  | e1=expression GREATEREQ e2=expression  {  mkexp_ ~loc:$sloc (PEbin (OPge, e1, e2 )) }
  | LBRACE struct_fields opt_semi RBRACE  { mkexp_ ~loc:$sloc (PEstruct (List.rev $2)) }
;

comma_sep_expressions:
    e=expression  { mkexp_ ~loc:$sloc e.p_expression_desc }
  | expression COMMA comma_sep_expressions  {  mkexp_ ~loc:$sloc (PEpair ($1, $3)) }
struct_fields:
    struct_field  { [$1] }
  | struct_fields SEMICOLON struct_field  { $3 :: $1 }
;
struct_field:
    IDENT EQUAL e=expression  { $1, mkexp_ ~loc:$sloc e.p_expression_desc }
;

command_core:
    e=expression  { PCyield e }


  | BEGIN c=commands END  { c.p_command_desc }
  | MATCH e=expression WITH opt_bar match_clauses END  { (PCmatch (mkexp_ ~loc:$sloc e.p_expression_desc, List.rev $5)) }
  | e1=expression ASSIGN e2=expression  { (PCstore (mkexp_ ~loc:$sloc e1.p_expression_desc, mkexp_ ~loc:$sloc e2.p_expression_desc)) }
  | FAIL  { PCfail }

  | IF e=expression THEN command  { PCcond (mkexp_ ~loc:$sloc e.p_expression_desc, $4, None) }
  | IF e=expression THEN command ELSE command  { PCcond (mkexp_ ~loc:$sloc e.p_expression_desc, $4, Some $6) }

  | EMIT e=expression { PCemit (mkexp_ ~loc:$sloc e.p_expression_desc) }

  | FOR annotations IDENT eq_or_assign e1=expression TO e2=expression DO command
      { PCfor ($3, mkexp_ ~loc:$sloc e1.p_expression_desc, mkexp_ ~loc:$sloc e2.p_expression_desc, $9, $2) }
  | FIRST annotations IDENT eq_or_assign e1=expression TO e2=expression
      DO command
      { PCfirst ($3, mkexp_ ~loc:$sloc e1.p_expression_desc, mkexp_ ~loc:$sloc e2.p_expression_desc, $9, None, None, $2) }
  | FIRST annotations IDENT eq_or_assign e1=expression TO e2=expression
      DO command ELSE command
      { PCfirst ($3, mkexp_ ~loc:$sloc e1.p_expression_desc, mkexp_ ~loc:$sloc e2.p_expression_desc, $9, None, Some $11, $2) }
  | FIRST annotations IDENT eq_or_assign e1=expression TO e2=expression
      DO command THEN command
      { PCfirst ($3,mkexp_ ~loc:$sloc e1.p_expression_desc, mkexp_ ~loc:$sloc e2.p_expression_desc, $9, Some $11, None, $2) }
  | FIRST annotations IDENT eq_or_assign e1=expression TO e2=expression
      DO command THEN command ELSE command
      { PCfirst ($3, mkexp_ ~loc:$sloc e1.p_expression_desc, mkexp_ ~loc:$sloc e2.p_expression_desc, $9, Some $11, Some $13, $2) }
  | FOLD annotations IDENT eq_or_assign e1=expression TO e2=expression
     BAR             IDENT eq_or_assign e3=expression DO command
      { PCfold ($3, mkexp_ ~loc:$sloc e1.p_expression_desc, mkexp_ ~loc:$sloc e2.p_expression_desc, $9, mkexp_ ~loc:$sloc e3.p_expression_desc, $13, $2) }
  | TRANSFERETH l=delimited(LPAREN, separated_list(COMMA, expression), RPAREN) { PCtransfer l }
;
command:
    c=command_core  { mkcmd ~loc:$sloc c }

  | ASSERT annotated_command  { mkcmd ~loc:$sloc (PCassert $2) }
  | DENY annotated_command  { mkcmd ~loc:$sloc (PCdeny $2) }
  | GHOST annotated_command  { mkcmd ~loc:$sloc (PCghost $2) }

  | LET IDENT EQUAL commands IN commands  { mkcmd ~loc:$sloc (PClet ($2, $4, $6)) }
;
annotated_command:
    c=command_core  { mkcmd ~loc:$sloc c}
  | LET IDENT EQUAL commands IN annotated_command  { mkcmd ~loc:$sloc (PClet ($2, $4, $6)) }
;
eq_or_assign:
    EQUAL { () }
  | ASSIGN { () }
;
commands:
    c=command %prec below_SEMICOLON  { mkcmd ~loc:$sloc c.p_command_desc }
  | command SEMICOLON commands  { mkcmd ~loc:$sloc (PCsequence ($1, $3)) }
;

  /* The parsing of clauses is slightly complicated because we have special cases for
       h :: t => c
       [] => c
*/

match_clauses:
    match_clause  { [$1] }
  | match_clauses BAR match_clause { $3 :: $1 }
  ;
match_clause:
    match_pattern DOUBLEARROW commands
      { let (cnstr, params) = $1 in
        (cnstr , params, $3) }

  | LBRACKET RBRACKET DOUBLEARROW commands
      { ("NIL", [], $4) }
  ;
    
match_pattern:
  IDENT match_pattern_tail
      { match $2 with
        | PPTcons x -> ("CONS", [x; $1])
        | PPTother xs -> ($1, xs) }
  ;
    
match_pattern_tail:
          { PPTother [] }  
  | COLONCOLON IDENT { PPTcons $2 }
  | idents { PPTother (List.rev $1) }
  ;
    
/* Object */
object_declaration:
    opt_logical_or_trusted
    OBJECT IDENT base_layer_signature COLON object_signature_expr LBRACE
      object_fields_and_methods RBRACE
      { let fields_rev, methods_rev = $8 in
        let obj_type = { pObjectBase = $4; pObjectSignature = $6; pObjectTypLoc = (make_loc $sloc) } in
        $3, {
          pObjectType = None;
          pObjectDesc = mkobj ~loc:$sloc (POconstr {
            pObjType = obj_type;
            pObjKind = $1;
            pObjFields = List.rev fields_rev;
            pObjMethods = List.rev methods_rev;
            pObjLoc = (make_loc $sloc) });
          pObjectLoc = (make_loc $sloc) }
      }
  | opt_logical_or_trusted
    OBJECT IDENT base_layer_signature COLON object_signature_expr
      EQUAL object_expression
      /* opt_logical_or_trusted are not used, just there to allow parser to factor with
         the previous rule */
      { $3, { pObjectType = Some { pObjectBase = $4; pObjectSignature = $6; pObjectTypLoc = (make_loc $sloc) };
              pObjectDesc = $8; pObjectLoc = (make_loc $sloc) } }
  | opt_logical_or_trusted OBJECT IDENT EQUAL object_expression
      { $3, { pObjectType = None; pObjectDesc = $5; pObjectLoc = (make_loc $sloc) } }
;

base_layer_signature:
      { mklayersign ~loc:$sloc (PLSconstr []) }
  | LPAREN RPAREN  { mklayersign ~loc:$sloc (PLSconstr []) }
  | LPAREN layer_signature RPAREN  { $2 }
  | LPAREN base_slots RPAREN  { mklayersign ~loc:$sloc  (PLSconstr (List.rev $2)) }
;
base_slots:
    base_slot  { [$1] }
  | base_slots COMMA base_slot  { $3 :: $1 }
;
base_slot:
    IDENT COLON object_signature_expr  { $1, $3 }
;

object_fields_and_methods:
    object_field_or_method
      { either_to_double_list $1 }
  | object_fields_and_methods object_field_or_method
      { cons_either_double_list $2 $1 }
;
object_field_or_method:
    LET annotations method_kind IDENT COLON type_expression ASSIGN e=expression
      { (* annotations are not used and only one (GHOST) method_kind is
           acceptable, just there to allow parser to factor with
           the next rule *)
    let is_ghost = match $3 with
      | MKnormal -> false
      | MKghost -> true
      | MKconstghost ->
        print_endline ("Warning: object fields cannot be declared `const ghost': " ^
          $4 ^ " marked as `ghost'.");
        true
      | k ->
        print_endline ("Warning: object fields cannot be declared `" ^
          string_of_method_kind k ^ "': " ^ $4 ^ " marked as normal.");
        false
      in `Left ($4, $6, (mkexp_ ~loc:$sloc e.p_expression_desc) , is_ghost) }
  | LET annotations method_kind IDENT method_param opt_type_annotation EQUAL
      commands
      { `Right ($4, { pMethodArguments = $5;
                      pMethodReturnType = $6;
                      pMethodKind = $3;
                      pMethodBody = $8;
                      pMethodAnnotations = $2;
                      pMethodLoc = (make_loc $sloc) }) }
  | LET annotations CONSTRUCTOR method_param opt_type_annotation EQUAL
      commands
      { `Right ("constructor", { pMethodArguments = $4;
                      pMethodReturnType = $5;
                      pMethodKind = MKconstructor;
                      pMethodBody = $7;
                      pMethodAnnotations = $2;
                      pMethodLoc = (make_loc $sloc) }) }
;

method_param:
    IDENT  { [$1, None] }
  | LPAREN RPAREN  { ["()", Some (mkfotyp ~loc:$sloc (PTbuiltin Tunit))] }
  | LPAREN method_parameters RPAREN  { List.rev $2 }
;
method_parameters:
    method_parameter { [$1] }
  | method_parameters COMMA method_parameter  { $3 :: $1 }
;
method_parameter:
    IDENT opt_type_annotation  { $1, $2 }
;
opt_type_annotation:
      { None }
  | COLON type_expression  { Some $2 }

object_expression:
    IDENT  { mkobj ~loc:$sloc (POname $1) }
  | CLONE IDENT  {mkobj ~loc:$sloc (POclone $2)}
  | object_expression COLONGREATER layer_signature  { mkobj ~loc:$sloc (POrelax ($1, $3)) }
  | LPAREN object_expression RPAREN  { $2 }
;

/* Layer */
layer_declaration:
    LAYER IDENT annotations layer_signature_annotation EQUAL layer_expression
        layer_invariant_annotation
      { $2, {  pLayerLoc = (make_loc $sloc); 
              pLayerAnnotations = $3;
              pLayerInvariant = $7;
              pLayerDesc = match $4 with
                | None -> $6
                | Some t -> mklayer ~loc:$sloc (PLrelax ($6, t));
             }
      }
;
layer_signature_annotation:
      { None }
  | COLON layer_type  { Some $2 }
;
layer_invariant_annotation:
      { None }
  | ASSERT proposition  { Some $2 }
layer_expression:
    IDENT
      {  mklayer ~loc:$sloc (PLname $1) }
  | LBRACE layer_slots RBRACE
      {  mklayer ~loc:$sloc (PLconstr (List.rev $2)) }
  | layer_expression COLON layer_type
      {  mklayer ~loc:$sloc (PLrelax ($1, $3)) }
  | layer_expression AT layer_expression
      {  mklayer ~loc:$sloc (PLinst ($1, $3)) }
  | layer_expression COLONGREATER layer_expression WITH proposition
      {  mklayer ~loc:$sloc (PLrefine ($1, $3, $5)) }
  | LPAREN layer_expression RPAREN  { $2 }
layer_slots:
      { [] }
  | layer_slots_plus opt_semi  { $1 }
;
layer_slots_plus:
    layer_slot  { [$1] }
  | layer_slots_plus SEMICOLON layer_slot  { $3 :: $1 }
;
layer_slot:
    IDENT EQUAL layer_obj_inst  { $1, $3 }
;
layer_obj_inst:
    object_expression { mkobjinst ~loc:$sloc (POinternal $1) }
#ifdef ANT
  | IDENTITY LPAREN INT RPAREN COLONLESS object_expression { mkobjinst ~loc:$sloc (POexternal ((CONaddress $3), $6)) }
  | IDENTITY LPAREN UINT RPAREN COLONLESS object_expression { mkobjinst ~loc:$sloc (POexternal ((CONaddress $3), $6)) }
#else
  | ADDRESS LPAREN INT RPAREN COLONLESS object_expression { mkobjinst ~loc:$sloc (POexternal ((CONaddress $3), $6)) }
  | ADDRESS LPAREN UINT RPAREN COLONLESS object_expression { mkobjinst ~loc:$sloc (POexternal ((CONaddress $3), $6)) }
#endif
;

annotations:
      { [] }
  | LBRACKET LBRACKET RBRACKET RBRACKET  { [] }
  | LBRACKET LBRACKET annotations_plus RBRACKET RBRACKET  { List.rev $3 }
;
annotations_plus:
    annotation  { [$1] }
  | annotations_plus COMMA annotation  { $3 :: $1 }
;

annotation:
    EQUAL command  { PAexpr $2 }
  | INT  { PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc  (PEconstant (CONint (int_of_string($1))))))) }
  | UINT  { PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc  (PEconstant (CONuint (int_of_string($1))))))) }
#ifdef ANT
  | IDENTITY LPAREN INT RPAREN { PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc (PEconstant (CONaddress $3))))) }
  | IDENTITY LPAREN UINT RPAREN { PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc (PEconstant (CONaddress $3))))) }
#else
  | ADDRESS LPAREN INT RPAREN { PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc (PEconstant (CONaddress $3))))) }
  | ADDRESS LPAREN UINT RPAREN { PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc (PEconstant (CONaddress $3))))) }
#endif
  | IDENT annotation_arguments  { PAclause ($1, $2) }
  | STRING annotation_arguments  { PAclause ($1, $2) }
;
annotation_arguments:
      { [] }
  | INT  { [PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc (PEconstant (CONint (int_of_string($1)))))))] }
  | UINT  { [PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc  (PEconstant (CONuint (int_of_string($1)))))))] }
#ifdef ANT
  | IDENTITY LPAREN INT RPAREN { [PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc (PEconstant (CONaddress $3)))))] }
  | IDENTITY LPAREN UINT RPAREN { [PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc (PEconstant (CONaddress $3)))))] }
#else
  | ADDRESS LPAREN INT RPAREN { [PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc (PEconstant (CONaddress $3)))))] }
  | ADDRESS LPAREN UINT RPAREN { [PAexpr (mkcmd ~loc:$loc (PCyield (mkexp_ ~loc:$loc (PEconstant (CONaddress $3)))))] }
#endif
  | IDENT  { [PAclause ($1, [])] }
  | STRING  { [PAclause ($1, [])] }
  | LPAREN RPAREN  { [] }
  | LPAREN annotations_plus RPAREN  { List.rev $2 }
;

proposition:
    IDENT  { mkprop ~loc:$sloc (PPident $1) }
  | STRING  { mkprop ~loc:$sloc (PPexternal $1) }
;

opt_logical_or_trusted:
             { POnormal }
  | LOGICAL  { POlogical }
  | TRUSTED  { POtrusted }
;
opt_semi:
               { () }
  | SEMICOLON  { () }
;
opt_bar:
         { () }
  | BAR  { () }
;
%%
