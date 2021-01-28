
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | XOR
    | WITH
    | UNEQUAL
    | UINT of (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
  )
    | TYPE
    | TRUSTED
    | TO
    | THEN
    | STRING of (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 23 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
  )
    | STAR
    | SLASH
    | SIGNATURE
    | SHR
    | SHL
    | SEMICOLON
    | RPAREN
    | REFINED
    | RBRACKET
    | RBRACE
    | PLUS
    | OBJECT
    | MOD
    | MINUS
    | MATCH
    | MAPPING
    | LPAREN
    | LOGICAL
    | LIST
    | LET
    | LESSEQ
    | LESS
    | LBRACKET
    | LBRACE
    | LAYER
    | INT of (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 53 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
  )
    | INDEXED
    | IN
    | IF
    | IDENTITY
    | IDENT of (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 62 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
  )
    | GREATEREQ
    | GREATER
    | GHOST
    | FOR
    | FOLD
    | FIRST
    | FAIL
    | EXTERNAL
    | EVENT
    | EQUAL
    | EOF
    | END
    | EMIT
    | ELSE
    | DOUBLEARROW
    | DOT
    | DO
    | DISJUNCTION
    | DENY
    | CONSTRUCTOR
    | CONST
    | CONJUNCTION
    | COMMA
    | COLONLESS
    | COLONGREATER
    | COLONCOLON
    | COLON
    | CLONE
    | BITNOT
    | BITAND
    | BEGIN
    | BARBAR
    | BAR
    | BANG
    | AT
    | ASSIGN
    | ASSERT
    | ARROW
    | ARRAY
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState507
  | MenhirState503
  | MenhirState501
  | MenhirState499
  | MenhirState497
  | MenhirState496
  | MenhirState495
  | MenhirState494
  | MenhirState491
  | MenhirState489
  | MenhirState485
  | MenhirState481
  | MenhirState480
  | MenhirState479
  | MenhirState477
  | MenhirState476
  | MenhirState475
  | MenhirState474
  | MenhirState473
  | MenhirState471
  | MenhirState468
  | MenhirState464
  | MenhirState458
  | MenhirState455
  | MenhirState452
  | MenhirState445
  | MenhirState439
  | MenhirState433
  | MenhirState432
  | MenhirState427
  | MenhirState426
  | MenhirState420
  | MenhirState419
  | MenhirState412
  | MenhirState411
  | MenhirState410
  | MenhirState409
  | MenhirState407
  | MenhirState402
  | MenhirState401
  | MenhirState399
  | MenhirState396
  | MenhirState393
  | MenhirState391
  | MenhirState387
  | MenhirState385
  | MenhirState376
  | MenhirState375
  | MenhirState371
  | MenhirState367
  | MenhirState361
  | MenhirState355
  | MenhirState354
  | MenhirState352
  | MenhirState351
  | MenhirState350
  | MenhirState346
  | MenhirState343
  | MenhirState342
  | MenhirState341
  | MenhirState339
  | MenhirState332
  | MenhirState331
  | MenhirState329
  | MenhirState326
  | MenhirState325
  | MenhirState317
  | MenhirState312
  | MenhirState304
  | MenhirState302
  | MenhirState294
  | MenhirState293
  | MenhirState292
  | MenhirState291
  | MenhirState287
  | MenhirState286
  | MenhirState283
  | MenhirState282
  | MenhirState281
  | MenhirState280
  | MenhirState273
  | MenhirState272
  | MenhirState266
  | MenhirState262
  | MenhirState261
  | MenhirState259
  | MenhirState256
  | MenhirState254
  | MenhirState248
  | MenhirState247
  | MenhirState246
  | MenhirState245
  | MenhirState243
  | MenhirState242
  | MenhirState240
  | MenhirState239
  | MenhirState238
  | MenhirState237
  | MenhirState235
  | MenhirState233
  | MenhirState232
  | MenhirState230
  | MenhirState228
  | MenhirState224
  | MenhirState223
  | MenhirState222
  | MenhirState220
  | MenhirState211
  | MenhirState205
  | MenhirState201
  | MenhirState193
  | MenhirState190
  | MenhirState187
  | MenhirState183
  | MenhirState178
  | MenhirState176
  | MenhirState174
  | MenhirState170
  | MenhirState162
  | MenhirState160
  | MenhirState159
  | MenhirState158
  | MenhirState156
  | MenhirState154
  | MenhirState152
  | MenhirState150
  | MenhirState149
  | MenhirState147
  | MenhirState146
  | MenhirState144
  | MenhirState143
  | MenhirState140
  | MenhirState138
  | MenhirState137
  | MenhirState135
  | MenhirState134
  | MenhirState132
  | MenhirState130
  | MenhirState127
  | MenhirState125
  | MenhirState124
  | MenhirState121
  | MenhirState120
  | MenhirState118
  | MenhirState117
  | MenhirState114
  | MenhirState111
  | MenhirState109
  | MenhirState107
  | MenhirState102
  | MenhirState96
  | MenhirState95
  | MenhirState89
  | MenhirState87
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState74
  | MenhirState72
  | MenhirState70
  | MenhirState68
  | MenhirState66
  | MenhirState64
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState52
  | MenhirState51
  | MenhirState49
  | MenhirState47
  | MenhirState46
  | MenhirState37
  | MenhirState35
  | MenhirState33
  | MenhirState32
  | MenhirState30
  | MenhirState20
  | MenhirState11
  | MenhirState8
  | MenhirState6
  | MenhirState4

# 2 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
  
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

# 385 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"

let rec _menhir_goto_match_clauses : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_match_clauses -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv2011 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) * _menhir_state * 'tv_match_clauses) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv2003 * _menhir_state * 'tv_match_clauses) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run193 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState205 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACKET ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState205 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState205) : 'freshtv2004)
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv2007 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) * _menhir_state * 'tv_match_clauses) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv2005 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) * _menhir_state * 'tv_match_clauses) = Obj.magic _menhir_stack in
        let (_endpos__6_ : Lexing.position) = _endpos in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_), _, (_4 : 'tv_opt_bar)), _, (_5 : 'tv_match_clauses)) = _menhir_stack in
        let _6 = () in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_command_core = let _endpos = _endpos__6_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 552 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                       ( (PCmatch (mkexp_ ~loc:_sloc e.p_expression_desc, List.rev _5)) )
# 429 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv2006)) : 'freshtv2008)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv2009 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) * _menhir_state * 'tv_match_clauses) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv2010)) : 'freshtv2012)

and _menhir_goto_match_clause : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_match_clause -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState205 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1997 * _menhir_state * 'tv_match_clauses)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_match_clause) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1995 * _menhir_state * 'tv_match_clauses)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_match_clause) : 'tv_match_clause) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_match_clauses)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_match_clauses = 
# 608 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                   ( _3 :: _1 )
# 457 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_match_clauses _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1996)) : 'freshtv1998)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv2001) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_match_clause) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1999) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_match_clause) : 'tv_match_clause) = _v in
        ((let _v : 'tv_match_clauses = 
# 607 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                  ( [_1] )
# 472 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_match_clauses _menhir_env _menhir_stack _menhir_s _v) : 'freshtv2000)) : 'freshtv2002)
    | _ ->
        _menhir_fail ()

and _menhir_goto_proposition : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_proposition -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState387 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1989 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_proposition) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1987 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos__5_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_5 : 'tv_proposition) : 'tv_proposition) = _v in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_layer_expression), _startpos__1_), _endpos__3_, _, (_3 : 'tv_layer_expression), _startpos__3_) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_layer_expression = let _endpos = _endpos__5_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 768 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      (  mklayer ~loc:_sloc (PLrefine (_1, _3, _5)) )
# 503 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1988)) : 'freshtv1990)
    | MenhirState396 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1993 * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_proposition) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1991 * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_proposition) : 'tv_proposition) = _v in
        ((let (_menhir_stack, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _endpos = _endpos__2_ in
        let _v : 'tv_layer_invariant_annotation = 
# 757 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                        ( Some _2 )
# 523 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_invariant_annotation _menhir_env _menhir_stack _endpos _v) : 'freshtv1992)) : 'freshtv1994)
    | _ ->
        _menhir_fail ()

and _menhir_goto_commands : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_commands -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1949 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1945 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1943 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_c_, _, (c : 'tv_commands)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_command_core = 
# 551 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                          ( c.p_command_desc )
# 555 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1944)) : 'freshtv1946)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1947 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1948)) : 'freshtv1950)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1953 * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1951 * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_command), _startpos__1_), _endpos__3_, _, (_3 : 'tv_commands)) = _menhir_stack in
        let _2 = () in
        let _endpos = _endpos__3_ in
        let _v : 'tv_commands = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 598 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                ( mkcmd ~loc:_sloc (PCsequence (_1, _3)) )
# 579 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_commands _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1952)) : 'freshtv1954)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1959 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 587 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1955 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 597 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState183 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState183 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState183 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState183 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState183) : 'freshtv1956)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1957 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 649 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1958)) : 'freshtv1960)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1965 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 658 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1961 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 668 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState190 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState190 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState190) : 'freshtv1962)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1963 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 726 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1964)) : 'freshtv1966)
    | MenhirState190 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1969 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 735 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1967 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 741 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 746 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__2_), _endpos__4_, _, (_4 : 'tv_commands)), _endpos__6_, _, (_6 : 'tv_commands)) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_command = let _endpos = _endpos__6_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 586 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                          ( mkcmd ~loc:_sloc (PClet (_2, _4, _6)) )
# 759 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1968)) : 'freshtv1970)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1973 * _menhir_state * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1971 * _menhir_state * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_), _endpos__4_, _, (_4 : 'tv_commands)) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_match_clause = 
# 616 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( ("NIL", [], _4) )
# 774 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_match_clause _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1972)) : 'freshtv1974)
    | MenhirState201 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1977 * _menhir_state * 'tv_match_pattern)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1975 * _menhir_state * 'tv_match_pattern)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_match_pattern)), _endpos__3_, _, (_3 : 'tv_commands)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_match_clause = 
# 612 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( let (cnstr, params) = _1 in
        (cnstr , params, _3) )
# 788 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_match_clause _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1976)) : 'freshtv1978)
    | MenhirState491 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv1981 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv1979 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _), _, (_4 : 'tv_method_param)), _, (_5 : 'tv_opt_type_annotation)), _endpos__7_, _, (_7 : 'tv_commands)) = _menhir_stack in
        let _6 = () in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_object_field_or_method = let _endpos = _endpos__7_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 707 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( `Right ("constructor", { pMethodArguments = _4;
                      pMethodReturnType = _5;
                      pMethodKind = MKconstructor;
                      pMethodBody = _7;
                      pMethodAnnotations = _2;
                      pMethodLoc = (make_loc _sloc) }) )
# 811 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_object_field_or_method _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1980)) : 'freshtv1982)
    | MenhirState501 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv1985 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 819 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv1983 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 825 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _, (_3 : 'tv_method_kind)), _endpos__4_, (_4 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 830 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__4_), _, (_5 : 'tv_method_param)), _, (_6 : 'tv_opt_type_annotation)), _endpos__8_, _, (_8 : 'tv_commands)) = _menhir_stack in
        let _7 = () in
        let _1 = () in
        let _v : 'tv_object_field_or_method = let _endpos = _endpos__8_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 699 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( `Right (_4, { pMethodArguments = _5;
                      pMethodReturnType = _6;
                      pMethodKind = _3;
                      pMethodBody = _8;
                      pMethodAnnotations = _2;
                      pMethodLoc = (make_loc _sloc) }) )
# 845 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_object_field_or_method _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1984)) : 'freshtv1986)
    | _ ->
        _menhir_fail ()

and _menhir_run199 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_idents -> Lexing.position -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 854 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1941 * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
    let (_endpos__2_ : Lexing.position) = _endpos in
    let ((_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 864 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 868 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__2_ : Lexing.position) = _startpos in
    ((let (_menhir_stack, _menhir_s, (_1 : 'tv_idents)) = _menhir_stack in
    let _v : 'tv_idents = 
# 316 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                  ( _2 :: _1 )
# 875 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_idents _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1942)

and _menhir_goto_object_fields_and_methods : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_object_fields_and_methods -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((((('freshtv1939 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 886 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LET ->
        _menhir_run474 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState503 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv1937 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 898 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState503 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv1935 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 907 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
        let (_endpos__9_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        ((let (((((((_menhir_stack, _endpos__1_, (_1 : 'tv_opt_logical_or_trusted), _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 914 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__3_), (_4 : 'tv_base_layer_signature)), _endpos__6_, _, (_6 : 'tv_object_signature_expr), _startpos__6_), _startpos__7_), _, (_8 : 'tv_object_fields_and_methods)) = _menhir_stack in
        let _9 = () in
        let _7 = () in
        let _5 = () in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__9_ in
        let _v : 'tv_object_declaration = let _endpos = _endpos__9_ in
        let _symbolstartpos = if _startpos__1_ != _endpos__1_ then
          _startpos__1_
        else
          _startpos__2_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 637 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( let fields_rev, methods_rev = _8 in
        let obj_type = { pObjectBase = _4; pObjectSignature = _6; pObjectTypLoc = (make_loc _sloc) } in
        _3, {
          pObjectType = None;
          pObjectDesc = mkobj ~loc:_sloc (POconstr {
            pObjType = obj_type;
            pObjKind = _1;
            pObjFields = List.rev fields_rev;
            pObjMethods = List.rev methods_rev;
            pObjLoc = (make_loc _sloc) });
          pObjectLoc = (make_loc _sloc) }
      )
# 942 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_object_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv1936)) : 'freshtv1938)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState503) : 'freshtv1940)

and _menhir_goto_idents_semi_sep : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_idents_semi_sep -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1933 * _menhir_state * 'tv_idents_semi_sep) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1931 * _menhir_state * 'tv_idents_semi_sep) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState304 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1927 * _menhir_state * 'tv_idents_semi_sep) * _menhir_state) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 973 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1925 * _menhir_state * 'tv_idents_semi_sep) * _menhir_state) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let ((_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 983 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 987 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos__3_ : Lexing.position) = _startpos in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_idents_semi_sep)), _) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_idents_semi_sep = 
# 391 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                     ( _3 :: _1 )
# 995 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_idents_semi_sep _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1926)) : 'freshtv1928)
        | RBRACE ->
            _menhir_reduce214 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1929 * _menhir_state * 'tv_idents_semi_sep) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1930)) : 'freshtv1932)
    | RBRACE ->
        _menhir_reduce213 _menhir_env (Obj.magic _menhir_stack) MenhirState304
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState304) : 'freshtv1934)

and _menhir_goto_layer_slots_plus : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_layer_slots_plus -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1923 * _menhir_state * 'tv_layer_slots_plus) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1921 * _menhir_state * 'tv_layer_slots_plus) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState375 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run353 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState376 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            _menhir_reduce214 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState376) : 'freshtv1922)
    | RBRACE ->
        _menhir_reduce213 _menhir_env (Obj.magic _menhir_stack) MenhirState375
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState375) : 'freshtv1924)

and _menhir_goto_layer_invariant_annotation : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_layer_invariant_annotation -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((('freshtv1919 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1051 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_layer_invariant_annotation) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((('freshtv1917 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1059 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos__7_ : Lexing.position) = _endpos in
    let ((_7 : 'tv_layer_invariant_annotation) : 'tv_layer_invariant_annotation) = _v in
    ((let (((((_menhir_stack, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1066 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )), _startpos__2_), _endpos__3_, _, (_3 : 'tv_annotations)), (_4 : 'tv_layer_signature_annotation)), _endpos__6_, _, (_6 : 'tv_layer_expression), _startpos__6_) = _menhir_stack in
    let _5 = () in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__7_ in
    let _v : 'tv_layer_declaration = let _endpos = _endpos__7_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 742 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _2, {  pLayerLoc = (make_loc _sloc); 
              pLayerAnnotations = _3;
              pLayerInvariant = _7;
              pLayerDesc = match _4 with
                | None -> _6
                | Some t -> mklayer ~loc:_sloc (PLrelax (_6, t));
             }
      )
# 1085 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1915) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_layer_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1913) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_layer_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1911) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let ((_1 : 'tv_layer_declaration) : 'tv_layer_declaration) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_declaration = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 228 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                       ( fst _1, mkdecl ~loc:_sloc (PDlayer (snd _1)) )
# 1109 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1912)) : 'freshtv1914)) : 'freshtv1916)) : 'freshtv1918)) : 'freshtv1920)

and _menhir_run388 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1116 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1909) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1127 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1131 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_proposition = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 820 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
            ( mkprop ~loc:_sloc (PPexternal _1) )
# 1141 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_proposition _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1910)

and _menhir_run389 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1148 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1907) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1159 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1163 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_proposition = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 819 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
           ( mkprop ~loc:_sloc (PPident _1) )
# 1173 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_proposition _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1908)

and _menhir_run385 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run382 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState385 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run352 _menhir_env (Obj.magic _menhir_stack) MenhirState385 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run351 _menhir_env (Obj.magic _menhir_stack) MenhirState385 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState385

and _menhir_run391 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run342 _menhir_env (Obj.magic _menhir_stack) MenhirState391 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState391

and _menhir_run393 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run382 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState393 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run352 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run351 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState393

and _menhir_reduce215 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_opt_type_annotation = 
# 728 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( None )
# 1226 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_opt_type_annotation _menhir_env _menhir_stack _menhir_s _v

and _menhir_run480 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARRAY ->
        _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState480 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState480 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState480 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIST ->
        _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState480 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState480 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MAPPING ->
        _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState480 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState480

and _menhir_goto_layer_slots : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_layer_slots -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1905 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_slots) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1901 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_slots) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1899 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_slots) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_layer_slots)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_layer_expression = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 762 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      (  mklayer ~loc:_sloc (PLconstr (List.rev _2)) )
# 1280 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1900)) : 'freshtv1902)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1903 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_slots) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1904)) : 'freshtv1906)

and _menhir_run353 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1294 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1895 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1306 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CLONE ->
            _menhir_run357 _menhir_env (Obj.magic _menhir_stack) MenhirState354 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run356 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState354 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1893) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState354 in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1889 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv1875 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_v : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1340 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                    )) = _v in
                    let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | RPAREN ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (('freshtv1871 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1352 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        ((let _menhir_stack = (_menhir_stack, _endpos) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | COLONLESS ->
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((('freshtv1867 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1364 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                            ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                            ((let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            match _tok with
                            | CLONE ->
                                _menhir_run357 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | IDENT _v ->
                                _menhir_run356 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState371 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | LPAREN ->
                                _menhir_run355 _menhir_env (Obj.magic _menhir_stack) MenhirState371 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState371) : 'freshtv1868)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((('freshtv1869 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1386 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                            ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                            ((let ((((_menhir_stack, _, _menhir_s, _), _), _, _, _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1870)) : 'freshtv1872)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (('freshtv1873 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1397 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1874)) : 'freshtv1876)
                | UINT _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv1885 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_v : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1408 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                    )) = _v in
                    let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | RPAREN ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (('freshtv1881 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1420 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        ((let _menhir_stack = (_menhir_stack, _endpos) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | COLONLESS ->
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((('freshtv1877 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1432 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                            ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                            ((let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            match _tok with
                            | CLONE ->
                                _menhir_run357 _menhir_env (Obj.magic _menhir_stack) MenhirState367 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | IDENT _v ->
                                _menhir_run356 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState367 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | LPAREN ->
                                _menhir_run355 _menhir_env (Obj.magic _menhir_stack) MenhirState367 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState367) : 'freshtv1878)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((('freshtv1879 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1454 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                            ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                            ((let ((((_menhir_stack, _, _menhir_s, _), _), _, _, _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1880)) : 'freshtv1882)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (('freshtv1883 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 1465 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1884)) : 'freshtv1886)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv1887 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1888)) : 'freshtv1890)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1891 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1892)) : 'freshtv1894)
        | LPAREN ->
            _menhir_run355 _menhir_env (Obj.magic _menhir_stack) MenhirState354 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState354) : 'freshtv1896)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1897 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1496 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1898)

and _menhir_run211 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_annotations_plus -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState211
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState211 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState211 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState211 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState211 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState211 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState211

and _menhir_goto_command : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_command -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState501 | MenhirState491 | MenhirState201 | MenhirState114 | MenhirState117 | MenhirState190 | MenhirState124 | MenhirState170 | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1815 * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1809 * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState170 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState170 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState170) : 'freshtv1810)
        | BAR | COMMA | ELSE | END | IN | LET | RBRACE | RBRACKET | RPAREN | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1811 * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_c_, _menhir_s, (c : 'tv_command), _startpos_c_) = _menhir_stack in
            let _endpos = _endpos_c_ in
            let _v : 'tv_commands = let _endpos = _endpos_c_ in
            let _symbolstartpos = _startpos_c_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 597 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                     ( mkcmd ~loc:_sloc c.p_command_desc )
# 1596 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_commands _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1812)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1813 * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1814)) : 'freshtv1816)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv1825 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1611 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1817 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1621 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178) : 'freshtv1818)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1819 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1677 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState174 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState174 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState174) : 'freshtv1820)
        | BAR | COMMA | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1821 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1733 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1738 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__3_), _, (_4 : 'tv_eq_or_assign)), _endpos_e1_, _, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_), _endpos__9_, _, (_9 : 'tv_command), _startpos__9_) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__9_ in
            let _v : 'tv_command_core = let _endpos = _endpos__9_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 565 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( PCfirst (_3, mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, None, None, _2) )
# 1751 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1822)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1823 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1761 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1824)) : 'freshtv1826)
    | MenhirState174 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv1833 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1770 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv1827 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1780 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176) : 'freshtv1828)
        | BAR | COMMA | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv1829 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1836 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1841 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__3_), _, (_4 : 'tv_eq_or_assign)), _endpos_e1_, _, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_), _endpos__9_, _, (_9 : 'tv_command), _startpos__9_), _endpos__11_, _, (_11 : 'tv_command), _startpos__11_) = _menhir_stack in
            let _10 = () in
            let _8 = () in
            let _6 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__11_ in
            let _v : 'tv_command_core = let _endpos = _endpos__11_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 571 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( PCfirst (_3,mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, Some _11, None, _2) )
# 1855 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1830)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv1831 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1865 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1832)) : 'freshtv1834)
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((((('freshtv1837 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1874 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((((('freshtv1835 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1880 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1885 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__3_), _, (_4 : 'tv_eq_or_assign)), _endpos_e1_, _, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_), _endpos__9_, _, (_9 : 'tv_command), _startpos__9_), _endpos__11_, _, (_11 : 'tv_command), _startpos__11_), _endpos__13_, _, (_13 : 'tv_command), _startpos__13_) = _menhir_stack in
        let _12 = () in
        let _10 = () in
        let _8 = () in
        let _6 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__13_ in
        let _v : 'tv_command_core = let _endpos = _endpos__13_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 574 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( PCfirst (_3, mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, Some _11, Some _13, _2) )
# 1900 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1836)) : 'freshtv1838)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv1841 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1908 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv1839 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1914 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1919 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__3_), _, (_4 : 'tv_eq_or_assign)), _endpos_e1_, _, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_), _endpos__9_, _, (_9 : 'tv_command), _startpos__9_), _endpos__11_, _, (_11 : 'tv_command), _startpos__11_) = _menhir_stack in
        let _10 = () in
        let _8 = () in
        let _6 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__11_ in
        let _v : 'tv_command_core = let _endpos = _endpos__11_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 568 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( PCfirst (_3, mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, None, Some _11, _2) )
# 1933 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1840)) : 'freshtv1842)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((((('freshtv1845 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1941 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1945 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((((('freshtv1843 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1951 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1955 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((((((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1960 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__3_), _, (_4 : 'tv_eq_or_assign)), _endpos_e1_, _, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_), _endpos__9_, (_9 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1964 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__9_), _, (_10 : 'tv_eq_or_assign)), _endpos_e3_, _, (e3 : 'tv_expression), _startpos_e3_), _endpos__13_, _, (_13 : 'tv_command), _startpos__13_) = _menhir_stack in
        let _12 = () in
        let _8 = () in
        let _6 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__13_ in
        let _v : 'tv_command_core = let _endpos = _endpos__13_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 577 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( PCfold (_3, mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, mkexp_ ~loc:_sloc e3.p_expression_desc, _13, _2) )
# 1978 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1844)) : 'freshtv1846)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv1849 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1986 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv1847 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1992 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 1997 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__3_), _, (_4 : 'tv_eq_or_assign)), _endpos_e1_, _, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_), _endpos__9_, _, (_9 : 'tv_command), _startpos__9_) = _menhir_stack in
        let _8 = () in
        let _6 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__9_ in
        let _v : 'tv_command_core = let _endpos = _endpos__9_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 562 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( PCfor (_3, mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, _2) )
# 2010 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1848)) : 'freshtv1850)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1857 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1851 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState187 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState187 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState187) : 'freshtv1852)
        | BAR | COMMA | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1853 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_), _endpos__4_, _, (_4 : 'tv_command), _startpos__4_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_command_core = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 556 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                  ( PCcond (mkexp_ ~loc:_sloc e.p_expression_desc, _4, None) )
# 2085 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1854)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1855 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1856)) : 'freshtv1858)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1861 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1859 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_), _endpos__4_, _, (_4 : 'tv_command), _startpos__4_), _endpos__6_, _, (_6 : 'tv_command), _startpos__6_) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_command_core = let _endpos = _endpos__6_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 557 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                               ( PCcond (mkexp_ ~loc:_sloc e.p_expression_desc, _4, Some _6) )
# 2112 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1860)) : 'freshtv1862)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1865 * _menhir_state) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1863 * _menhir_state) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _endpos__2_, _, (_2 : 'tv_command), _startpos__2_) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_annotation = 
# 798 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                   ( PAexpr _2 )
# 2125 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1864)) : 'freshtv1866)
    | _ ->
        _menhir_fail ()

and _menhir_goto_annotated_command : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_annotated_command -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState160 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1795 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotated_command) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1793 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_annotated_command) : 'tv_annotated_command) = _v in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_command = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 582 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                              ( mkcmd ~loc:_sloc (PCassert _2) )
# 2155 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1794)) : 'freshtv1796)
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1799 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotated_command) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1797 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_annotated_command) : 'tv_annotated_command) = _v in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_command = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 583 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                            ( mkcmd ~loc:_sloc (PCdeny _2) )
# 2179 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1798)) : 'freshtv1800)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1803 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 2187 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotated_command) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1801 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 2196 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) = Obj.magic _menhir_stack in
        let (_endpos__6_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_6 : 'tv_annotated_command) : 'tv_annotated_command) = _v in
        ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 2204 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__2_), _endpos__4_, _, (_4 : 'tv_commands)) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _endpos = _endpos__6_ in
        let _v : 'tv_annotated_command = let _endpos = _endpos__6_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 590 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                   ( mkcmd ~loc:_sloc (PClet (_2, _4, _6)) )
# 2216 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_annotated_command _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1802)) : 'freshtv1804)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1807 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotated_command) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1805 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_annotated_command) : 'tv_annotated_command) = _v in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_command = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 584 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                             ( mkcmd ~loc:_sloc (PCghost _2) )
# 2240 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1806)) : 'freshtv1808)
    | _ ->
        _menhir_fail ()

and _menhir_goto_opt_semi : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_opt_semi -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1733 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1729 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1727 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_struct_fields)), _, (_3 : 'tv_opt_semi)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_expression = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 533 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                          ( mkexp_ ~loc:_sloc (PEstruct (List.rev _2)) )
# 2275 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1728)) : 'freshtv1730)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1731 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1732)) : 'freshtv1734)
    | MenhirState247 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1741 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1737 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1735 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_field_declarations)), _, (_3 : 'tv_opt_semi)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _endpos = _endpos__4_ in
            let _v : 'tv_type_definition = 
# 279 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                               ( PTsingleton (List.rev _2) )
# 2306 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_type_definition _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1736)) : 'freshtv1738)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1739 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1740)) : 'freshtv1742)
    | MenhirState286 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1749 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1745 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1743 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_object_signature_fields)), _, (_3 : 'tv_opt_semi)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_object_signature_expr = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 360 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                    ( mksig ~loc:_sloc (PSconstr (List.rev _2)) )
# 2341 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1744)) : 'freshtv1746)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1747 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1748)) : 'freshtv1750)
    | MenhirState304 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1779 * _menhir_state * 'tv_idents_semi_sep) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1777 * _menhir_state * 'tv_idents_semi_sep) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_idents_semi_sep)), _, (_2 : 'tv_opt_semi)) = _menhir_stack in
        let _v : 'tv_idents_semi = 
# 387 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                              ( List.rev _1 )
# 2360 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1775) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_idents_semi) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState302 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1757 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1753 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1751 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                let (_endpos__6_ : Lexing.position) = _endpos in
                ((let ((((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_object_signature_expr), _startpos__1_), _endpos__3_, _startpos__3_), _startpos__4_), _, (_5 : 'tv_idents_semi)) = _menhir_stack in
                let _6 = () in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__6_ in
                let _v : 'tv_object_signature_expr = let _endpos = _endpos__6_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 364 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                                  ( mksig ~loc:_sloc (PSlogicize (_1, _5)) )
# 2395 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1752)) : 'freshtv1754)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1755 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1756)) : 'freshtv1758)
        | MenhirState312 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1765 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1761 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1759 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                let (_endpos__6_ : Lexing.position) = _endpos in
                ((let ((((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_object_signature_expr), _startpos__1_), _startpos__3_), _startpos__4_), _, (_5 : 'tv_idents_semi)) = _menhir_stack in
                let _6 = () in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__6_ in
                let _v : 'tv_object_signature_expr = let _endpos = _endpos__6_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 362 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                                ( mksig ~loc:_sloc (PSghostize (_1, _5)) )
# 2432 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1760)) : 'freshtv1762)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1763 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1764)) : 'freshtv1766)
        | MenhirState317 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1773 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1769 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1767 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                let (_endpos__5_ : Lexing.position) = _endpos in
                ((let ((((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_object_signature_expr), _startpos__1_), _startpos__2_), _startpos__3_), _, (_4 : 'tv_idents_semi)) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _2 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__5_ in
                let _v : 'tv_object_signature_expr = let _endpos = _endpos__5_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 366 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                           ( mksig ~loc:_sloc (PSminus (_1, _4)) )
# 2468 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1768)) : 'freshtv1770)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1771 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1772)) : 'freshtv1774)
        | _ ->
            _menhir_fail ()) : 'freshtv1776)) : 'freshtv1778)) : 'freshtv1780)
    | MenhirState331 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1787 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1783 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1781 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_layer_signature_fields)), _, (_3 : 'tv_opt_semi)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _endpos = _endpos__4_ in
            let _v : 'tv_layer_signature = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 402 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                   ( mklayersign ~loc:_sloc (PLSconstr (List.rev _2)) )
# 2504 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_layer_signature _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1782)) : 'freshtv1784)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1785 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1786)) : 'freshtv1788)
    | MenhirState375 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1791 * _menhir_state * 'tv_layer_slots_plus) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1789 * _menhir_state * 'tv_layer_slots_plus) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_layer_slots_plus)), _, (_2 : 'tv_opt_semi)) = _menhir_stack in
        let _v : 'tv_layer_slots = 
# 772 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                               ( _1 )
# 2523 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_slots _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1790)) : 'freshtv1792)
    | _ ->
        _menhir_fail ()

and _menhir_goto_event_declarations : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_event_declarations -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv1725) * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_event_declarations) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1713 * Lexing.position * _menhir_state * 'tv_event_declarations) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run428 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState439 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState439) : 'freshtv1714)
    | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1721) * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_event_declarations) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, (_2 : 'tv_opt_bar)), _endpos__3_, _, (_3 : 'tv_event_declarations)) = _menhir_stack in
        let _1 = () in
        let _endpos = _endpos__3_ in
        let _v : 'tv_events_declaration = 
# 320 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                     ( _3 )
# 2558 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1719) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_events_declaration) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1717 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_events_declaration) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1715 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let ((_2 : 'tv_events_declaration) : 'tv_events_declaration) = _v in
        ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_declarations)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_declarations = 
# 219 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                    ( _2 @ _1 )
# 2577 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_declarations _menhir_env _menhir_stack _endpos _v) : 'freshtv1716)) : 'freshtv1718)) : 'freshtv1720)) : 'freshtv1722)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1723) * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_event_declarations) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1724)) : 'freshtv1726)

and _menhir_goto_idents : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_idents -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState193 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1705 * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run199 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOUBLEARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1701 * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_idents)) = _menhir_stack in
            let _v : 'tv_match_pattern_tail = 
# 629 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
           ( PPTother (List.rev _1) )
# 2607 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_match_pattern_tail _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1702)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1703 * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1704)) : 'freshtv1706)
    | MenhirState259 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1711 * Lexing.position) * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1707 * Lexing.position) * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ARRAY ->
                _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState261 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState261 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState261) : 'freshtv1708)
        | IDENT _v ->
            _menhir_run199 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1709 * Lexing.position) * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1710)) : 'freshtv1712)
    | _ ->
        _menhir_fail ()

and _menhir_goto_object_field_or_method : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_object_field_or_method -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState503 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1695 * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_object_field_or_method) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1693 * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_object_field_or_method) : 'tv_object_field_or_method) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_object_fields_and_methods)) = _menhir_stack in
        let _v : 'tv_object_fields_and_methods = 
# 678 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( cons_either_double_list _2 _1 )
# 2673 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_object_fields_and_methods _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1694)) : 'freshtv1696)
    | MenhirState473 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1699) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_object_field_or_method) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1697) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_object_field_or_method) : 'tv_object_field_or_method) = _v in
        ((let _v : 'tv_object_fields_and_methods = 
# 676 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( either_to_double_list _1 )
# 2688 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_object_fields_and_methods _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1698)) : 'freshtv1700)
    | _ ->
        _menhir_fail ()

and _menhir_goto_comma_sep_expressions : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_comma_sep_expressions -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1683 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1681 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_expression), _startpos__1_), _endpos__3_, _, (_3 : 'tv_comma_sep_expressions)) = _menhir_stack in
        let _2 = () in
        let _endpos = _endpos__3_ in
        let _v : 'tv_comma_sep_expressions = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 538 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                            (  mkexp_ ~loc:_sloc (PEpair (_1, _3)) )
# 2712 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_comma_sep_expressions _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1682)) : 'freshtv1684)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1691 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1687 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1685 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_comma_sep_expressions)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_atom = 
# 498 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                         ( _2 )
# 2737 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1686)) : 'freshtv1688)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1689 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1690)) : 'freshtv1692)
    | _ ->
        _menhir_fail ()

and _menhir_goto_struct_fields : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_struct_fields -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1679 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1677 * _menhir_state * 'tv_struct_fields) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState95 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState96 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            _menhir_reduce214 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState96) : 'freshtv1678)
    | RBRACE ->
        _menhir_reduce213 _menhir_env (Obj.magic _menhir_stack) MenhirState95
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState95) : 'freshtv1680)

and _menhir_run54 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run72 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState72 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_run56 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run58 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run60 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run68 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run62 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run64 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run66 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run77 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run83 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run87 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState87 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87

and _menhir_run89 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState89 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState89 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState89

and _menhir_run70 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run74 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState74 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState74 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState74

and _menhir_run303 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3289 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1675) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3300 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3304 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_idents_semi_sep = 
# 390 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
           ( [_1] )
# 3310 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_idents_semi_sep _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1676)

and _menhir_goto_object_declaration : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_object_declaration -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1673) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_object_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1671) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let ((_1 : 'tv_object_declaration) : 'tv_object_declaration) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_declaration = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 227 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                        ( fst _1, mkdecl ~loc:_sloc (PDobject (snd _1)) )
# 3333 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1672)) : 'freshtv1674)

and _menhir_goto_layer_obj_inst : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_layer_obj_inst -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1669 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3343 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position)) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_layer_obj_inst) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1667 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3351 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position)) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((_3 : 'tv_layer_obj_inst) : 'tv_layer_obj_inst) = _v in
    ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3358 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )), _startpos__1_) = _menhir_stack in
    let _2 = () in
    let _v : 'tv_layer_slot = 
# 779 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                ( _1, _3 )
# 3364 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1665) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_layer_slot) = _v in
    ((match _menhir_s with
    | MenhirState376 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1659 * _menhir_state * 'tv_layer_slots_plus) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_layer_slot) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1657 * _menhir_state * 'tv_layer_slots_plus) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_layer_slot) : 'tv_layer_slot) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_layer_slots_plus)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_layer_slots_plus = 
# 776 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                           ( _3 :: _1 )
# 3385 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_slots_plus _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1658)) : 'freshtv1660)
    | MenhirState352 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1663) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_layer_slot) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1661) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_layer_slot) : 'tv_layer_slot) = _v in
        ((let _v : 'tv_layer_slots_plus = 
# 775 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                ( [_1] )
# 3400 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_slots_plus _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1662)) : 'freshtv1664)
    | _ ->
        _menhir_fail ()) : 'freshtv1666)) : 'freshtv1668)) : 'freshtv1670)

and _menhir_run361 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run337 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState361 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run326 _menhir_env (Obj.magic _menhir_stack) MenhirState361 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState361

and _menhir_goto_layer_expression : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_layer_expression -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState351 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1635 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AT ->
            _menhir_run393 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run391 _menhir_env (Obj.magic _menhir_stack)
        | COLONGREATER ->
            _menhir_run385 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1631 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1629 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_layer_expression), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_layer_expression = 
# 769 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                    ( _2 )
# 3452 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1630)) : 'freshtv1632)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1633 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1634)) : 'freshtv1636)
    | MenhirState385 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1641 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AT ->
            _menhir_run393 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run391 _menhir_env (Obj.magic _menhir_stack)
        | COLONGREATER ->
            _menhir_run385 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1637 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run389 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState387 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run388 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState387 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState387) : 'freshtv1638)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1639 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1640)) : 'freshtv1642)
    | MenhirState393 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1647 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AT ->
            _menhir_run393 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run391 _menhir_env (Obj.magic _menhir_stack)
        | COLONGREATER ->
            _menhir_run385 _menhir_env (Obj.magic _menhir_stack)
        | ASSERT | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | RPAREN | SIGNATURE | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1643 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_layer_expression), _startpos__1_), _endpos__3_, _, (_3 : 'tv_layer_expression), _startpos__3_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_layer_expression = let _endpos = _endpos__3_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 766 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      (  mklayer ~loc:_sloc (PLinst (_1, _3)) )
# 3520 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1644)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1645 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1646)) : 'freshtv1648)
    | MenhirState350 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1655 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3535 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSERT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1649) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run389 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState396 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run388 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState396 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState396) : 'freshtv1650)
        | AT ->
            _menhir_run393 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run391 _menhir_env (Obj.magic _menhir_stack)
        | COLONGREATER ->
            _menhir_run385 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1651) = Obj.magic _menhir_stack in
            ((let (_, _endpos) = Obj.magic _menhir_stack in
            let _v : 'tv_layer_invariant_annotation = 
# 756 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( None )
# 3569 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_layer_invariant_annotation _menhir_env _menhir_stack _endpos _v) : 'freshtv1652)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1653 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3579 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1654)) : 'freshtv1656)
    | _ ->
        _menhir_fail ()

and _menhir_run479 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3589 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run480 _menhir_env (Obj.magic _menhir_stack) MenhirState479
    | COMMA | RPAREN ->
        _menhir_reduce215 _menhir_env (Obj.magic _menhir_stack) MenhirState479
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState479

and _menhir_goto_method_param : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_method_param -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState476 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1625 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run480 _menhir_env (Obj.magic _menhir_stack) MenhirState489
        | EQUAL ->
            _menhir_reduce215 _menhir_env (Obj.magic _menhir_stack) MenhirState489
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState489) : 'freshtv1626)
    | MenhirState494 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1627 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3628 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run480 _menhir_env (Obj.magic _menhir_stack) MenhirState499
        | EQUAL ->
            _menhir_reduce215 _menhir_env (Obj.magic _menhir_stack) MenhirState499
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState499) : 'freshtv1628)
    | _ ->
        _menhir_fail ()

and _menhir_run351 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run382 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState351 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run352 _menhir_env (Obj.magic _menhir_stack) MenhirState351 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run351 _menhir_env (Obj.magic _menhir_stack) MenhirState351 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState351

and _menhir_run352 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run353 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState352 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1623) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState352 in
        ((let _v : 'tv_layer_slots = 
# 771 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( [] )
# 3676 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_slots _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1624)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState352

and _menhir_run382 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3687 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1621) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3698 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3702 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_layer_expression = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 760 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      (  mklayer ~loc:_sloc (PLname _1) )
# 3713 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1622)

and _menhir_goto_type_definition : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_type_definition -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv1619 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3723 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_type_definition) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv1617 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3732 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
    let (_endpos__5_ : Lexing.position) = _endpos in
    let (_ : _menhir_state) = _menhir_s in
    let ((_5 : 'tv_type_definition) : 'tv_type_definition) = _v in
    ((let (((_menhir_stack, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3740 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )), _startpos__2_), _endpos__3_, _, (_3 : 'tv_annotations)) = _menhir_stack in
    let _4 = () in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__5_ in
    let _v : 'tv_type_declaration = let _endpos = _endpos__5_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 255 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _2, mkfotyp ~loc:_sloc (PTdata (_5, _3)) )
# 3752 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_type_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv1618)) : 'freshtv1620)

and _menhir_goto_annotations_plus : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_annotations_plus -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1603 * _menhir_state * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            _menhir_run211 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1599 * _menhir_state * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1597 * _menhir_state * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_annotations_plus)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_annotation_arguments = 
# 815 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                    ( List.rev _2 )
# 3782 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1598)) : 'freshtv1600)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1601 * _menhir_state * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1602)) : 'freshtv1604)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1615 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            _menhir_run211 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1611 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1607 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1605 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__5_ : Lexing.position) = _endpos in
                ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, (_3 : 'tv_annotations_plus)), _endpos__4_) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _endpos = _endpos__5_ in
                let _v : 'tv_annotations = 
# 790 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                          ( List.rev _3 )
# 3825 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_annotations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1606)) : 'freshtv1608)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1609 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1610)) : 'freshtv1612)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1613 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1614)) : 'freshtv1616)
    | _ ->
        _menhir_fail ()

and _menhir_goto_annotation_arguments : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_annotation_arguments -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1591 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3853 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotation_arguments) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1589 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3861 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_annotation_arguments) : 'tv_annotation_arguments) = _v in
        ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3868 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__1_) = _menhir_stack in
        let _v : 'tv_annotation = 
# 803 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                ( PAclause (_1, _2) )
# 3873 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1590)) : 'freshtv1592)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1595 * Lexing.position * _menhir_state * (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 3881 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotation_arguments) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1593 * Lexing.position * _menhir_state * (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 3889 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_annotation_arguments) : 'tv_annotation_arguments) = _v in
        ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 3896 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__1_) = _menhir_stack in
        let _v : 'tv_annotation = 
# 804 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                 ( PAclause (_1, _2) )
# 3901 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1594)) : 'freshtv1596)
    | _ ->
        _menhir_fail ()

and _menhir_goto_command_core : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_command_core -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState121 | MenhirState183 | MenhirState158 | MenhirState160 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1583) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_command_core) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1581) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_command_core) : 'tv_command_core) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _endpos = _endpos_c_ in
        let _v : 'tv_annotated_command = let _endpos = _endpos_c_ in
        let _symbolstartpos = _startpos_c_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 589 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                    ( mkcmd ~loc:_sloc c)
# 3930 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_annotated_command _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1582)) : 'freshtv1584)
    | MenhirState501 | MenhirState491 | MenhirState30 | MenhirState201 | MenhirState114 | MenhirState117 | MenhirState190 | MenhirState187 | MenhirState120 | MenhirState124 | MenhirState134 | MenhirState146 | MenhirState178 | MenhirState176 | MenhirState174 | MenhirState154 | MenhirState170 | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1587) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_command_core) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1585) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_command_core) : 'tv_command_core) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_command = let _endpos = _endpos_c_ in
        let _symbolstartpos = _startpos_c_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 580 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                    ( mkcmd ~loc:_sloc c )
# 3954 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1586)) : 'freshtv1588)
    | _ ->
        _menhir_fail ()

and _menhir_run122 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1577 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3973 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1573 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 3985 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState124 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState124 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState124 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState124 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState124) : 'freshtv1574)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1575 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4043 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1576)) : 'freshtv1578)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1579 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1580)

and _menhir_goto_method_parameters : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_method_parameters -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1571 * _menhir_state * Lexing.position) * _menhir_state * 'tv_method_parameters) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1563 * _menhir_state * 'tv_method_parameters) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run479 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState485 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState485) : 'freshtv1564)
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1567 * _menhir_state * Lexing.position) * _menhir_state * 'tv_method_parameters) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1565 * _menhir_state * Lexing.position) * _menhir_state * 'tv_method_parameters) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_method_parameters)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_method_param = 
# 718 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                     ( List.rev _2 )
# 4089 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_method_param _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1566)) : 'freshtv1568)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1569 * _menhir_state * Lexing.position) * _menhir_state * 'tv_method_parameters) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1570)) : 'freshtv1572)

and _menhir_goto_eq_or_assign : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_eq_or_assign -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1517 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4109 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState130 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState130 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState130) : 'freshtv1518)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1519 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4141 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState138 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState138 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState138 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState138 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState138) : 'freshtv1520)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv1521 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4173 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4177 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState144 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState144 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState144) : 'freshtv1522)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1523 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4209 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState150 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState150 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState150) : 'freshtv1524)
    | MenhirState402 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1537 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4241 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STRING _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1533 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4251 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4257 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | STRING _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv1527 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4269 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4273 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4279 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv1525 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4287 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4291 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__7_ : Lexing.position) = _endpos in
                let ((_7 : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4297 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4301 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) = _v in
                let (_startpos__7_ : Lexing.position) = _startpos in
                ((let ((((((_menhir_stack, _endpos__1_, _startpos__1_), _, _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4307 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )), _startpos__3_), _endpos__4_, _, (_4 : 'tv_annotations)), _, (_5 : 'tv_eq_or_assign)), _endpos__6_, (_6 : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4311 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )), _startpos__6_) = _menhir_stack in
                let _2 = () in
                let _1 = () in
                let _endpos = _endpos__7_ in
                let _v : 'tv_external_declaration = let _endpos = _endpos__7_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 240 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _3, mkdecl ~loc:_sloc (PDexternal_type (_6, Some _7, _4)) )
# 4322 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1526)) : 'freshtv1528)
            | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv1529 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4330 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4334 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((((((_menhir_stack, _endpos__1_, _startpos__1_), _, _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4339 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )), _startpos__3_), _endpos__4_, _, (_4 : 'tv_annotations)), _, (_5 : 'tv_eq_or_assign)), _endpos__6_, (_6 : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4343 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )), _startpos__6_) = _menhir_stack in
                let _2 = () in
                let _1 = () in
                let _endpos = _endpos__6_ in
                let _v : 'tv_external_declaration = let _endpos = _endpos__6_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 238 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _3, mkdecl ~loc:_sloc (PDexternal_type (_6, None, _4)) )
# 4354 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1530)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv1531 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4364 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4368 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1532)) : 'freshtv1534)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1535 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4379 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1536)) : 'freshtv1538)
    | MenhirState412 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv1545 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4388 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STRING _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1541 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4398 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4404 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1539 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4412 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos__10_ : Lexing.position) = _endpos in
            let ((_10 : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4418 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4422 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos__10_ : Lexing.position) = _startpos in
            ((let ((((((((_menhir_stack, _endpos__1_, _startpos__1_), _endpos__2_, _, _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4428 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__3_), _endpos__4_, _, (_4 : 'tv_annotations)), _endpos__6_, _, (_6 : 'tv_type_expression), _startpos__6_), _), _endpos__8_, _, (_8 : 'tv_type_expression), _startpos__8_), _, (_9 : 'tv_eq_or_assign)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _2 = () in
            let _1 = () in
            let _endpos = _endpos__10_ in
            let _v : 'tv_external_declaration = let _endpos = _endpos__10_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 245 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _3, mkdecl ~loc:_sloc (PDexternal_function (_10, _6, _8, _4)) )
# 4441 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1540)) : 'freshtv1542)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1543 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4451 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1544)) : 'freshtv1546)
    | MenhirState410 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv1553 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4460 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STRING _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1549 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4470 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4476 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1547 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4484 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos__8_ : Lexing.position) = _endpos in
            let ((_8 : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4490 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4494 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos__8_ : Lexing.position) = _startpos in
            ((let ((((((_menhir_stack, _endpos__1_, _startpos__1_), _endpos__2_, _, _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4500 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__3_), _endpos__4_, _, (_4 : 'tv_annotations)), _endpos__6_, _, (_6 : 'tv_type_expression), _startpos__6_), _, (_7 : 'tv_eq_or_assign)) = _menhir_stack in
            let _5 = () in
            let _2 = () in
            let _1 = () in
            let _endpos = _endpos__8_ in
            let _v : 'tv_external_declaration = let _endpos = _endpos__8_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 242 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _3, mkdecl ~loc:_sloc (PDexternal_const (_8, _6, _4)) )
# 4512 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1548)) : 'freshtv1550)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1551 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4522 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1552)) : 'freshtv1554)
    | MenhirState420 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1561 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4531 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STRING _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1557 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4541 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4547 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1555 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4555 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos__7_ : Lexing.position) = _endpos in
            let ((_7 : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4561 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 4565 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos__7_ : Lexing.position) = _startpos in
            ((let (((((_menhir_stack, _endpos__1_, _startpos__1_), _, _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4571 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__3_), _endpos__5_, _, (_5 : 'tv_type_expression), _startpos__5_), _, (_6 : 'tv_eq_or_assign)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _endpos = _endpos__7_ in
            let _v : 'tv_external_declaration = let _endpos = _endpos__7_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 247 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _3, mkdecl ~loc:_sloc (PDexternal_prop (_7, _5)) )
# 4583 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1556)) : 'freshtv1558)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1559 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4593 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1560)) : 'freshtv1562)
    | _ ->
        _menhir_fail ()

and _menhir_goto_object_signature_fields : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_object_signature_fields -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1515 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1513 * _menhir_state * 'tv_object_signature_fields) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState286 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONST ->
            _menhir_run284 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | CONSTRUCTOR ->
            _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | GHOST ->
            _menhir_run277 _menhir_env (Obj.magic _menhir_stack) MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LOGICAL ->
            _menhir_run276 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState287 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REFINED ->
            _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | RBRACE ->
            _menhir_reduce214 _menhir_env (Obj.magic _menhir_stack)
        | IDENT _ ->
            _menhir_reduce166 _menhir_env (Obj.magic _menhir_stack) MenhirState287
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState287) : 'freshtv1514)
    | RBRACE ->
        _menhir_reduce213 _menhir_env (Obj.magic _menhir_stack) MenhirState286
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState286) : 'freshtv1516)

and _menhir_reduce213 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_opt_semi = 
# 829 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
               ( () )
# 4646 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_opt_semi _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce214 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _1 = () in
    let _v : 'tv_opt_semi = 
# 830 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
               ( () )
# 4657 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_opt_semi _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_event_parameters : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_event_parameters -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1511 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4668 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * 'tv_event_parameters) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1495) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1491 * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4688 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1487 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4700 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ARRAY ->
                    _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState432 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState432 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENTITY ->
                    _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState432 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIST ->
                    _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState432 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState432 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MAPPING ->
                    _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState432 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState432) : 'freshtv1488)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1489 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4728 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv1490)) : 'freshtv1492)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1493 * Lexing.position) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv1494)) : 'freshtv1496)
    | BAR | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1507 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4742 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_event_parameters) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4747 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__1_), _endpos__2_, (_2 : 'tv_event_parameters)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_event_declaration = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 330 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( (_1, mkdecl ~loc:_sloc (PDevent (List.rev _2))) )
# 4756 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1505) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_event_declaration) = _v in
        ((match _menhir_s with
        | MenhirState439 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1499 * Lexing.position * _menhir_state * 'tv_event_declarations)) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_event_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1497 * Lexing.position * _menhir_state * 'tv_event_declarations)) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            let ((_3 : 'tv_event_declaration) : 'tv_event_declaration) = _v in
            ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_event_declarations)) = _menhir_stack in
            let _2 = () in
            let _endpos = _endpos__3_ in
            let _v : 'tv_event_declarations = 
# 325 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                              ( _3 :: _1 )
# 4781 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_event_declarations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1498)) : 'freshtv1500)
        | MenhirState427 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1503) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_event_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1501) = Obj.magic _menhir_stack in
            let (_endpos__1_ : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_event_declaration) : 'tv_event_declaration) = _v in
            ((let _endpos = _endpos__1_ in
            let _v : 'tv_event_declarations = 
# 324 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                       ( [_1] )
# 4799 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_event_declarations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1502)) : 'freshtv1504)
        | _ ->
            _menhir_fail ()) : 'freshtv1506)) : 'freshtv1508)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1509 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4811 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_event_parameters) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1510)) : 'freshtv1512)

and _menhir_run194 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4819 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1485) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4830 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4834 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_idents = 
# 315 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
           ( [_1] )
# 4840 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_idents _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1486)

and _menhir_goto_match_pattern_tail : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_match_pattern_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1483 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4850 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_match_pattern_tail) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1481 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4858 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((_2 : 'tv_match_pattern_tail) : 'tv_match_pattern_tail) = _v in
    ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 4865 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )), _startpos__1_) = _menhir_stack in
    let _v : 'tv_match_pattern = 
# 621 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( match _2 with
        | PPTcons x -> ("CONS", [x; _1])
        | PPTother xs -> (_1, xs) )
# 4872 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1479) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_match_pattern) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1477 * _menhir_state * 'tv_match_pattern) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOUBLEARROW ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1473 * _menhir_state * 'tv_match_pattern) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSERT ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BEGIN ->
            _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DENY ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EMIT ->
            _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FAIL ->
            _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FIRST ->
            _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOLD ->
            _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | GHOST ->
            _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MATCH ->
            _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState201 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState201 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState201) : 'freshtv1474)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1475 * _menhir_state * 'tv_match_pattern) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1476)) : 'freshtv1478)) : 'freshtv1480)) : 'freshtv1482)) : 'freshtv1484)

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_expression -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1231 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1227 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1225 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _endpos_a_, _menhir_s, (a : 'tv_atom), _startpos_a_), _, _startpos__2_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_a_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_atom = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos_a_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 499 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                           (  mkexp_ ~loc:_sloc (PEindex (a, e)) )
# 4997 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1226)) : 'freshtv1228)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1229 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1230)) : 'freshtv1232)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1237 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1233 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 516 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                     (  mkexp_ ~loc:_sloc (PEbin (OPxor, e1, e2 )) )
# 5054 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1234)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1235 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1236)) : 'freshtv1238)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1241 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1239 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : 'tv_expression = let _endpos = _endpos_e2_ in
        let _symbolstartpos = _startpos_e1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 522 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                      (  mkexp_ ~loc:_sloc (PEbin (OPtimes, e1, e2 )) )
# 5079 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1240)) : 'freshtv1242)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1245 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1243 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : 'tv_expression = let _endpos = _endpos_e2_ in
        let _symbolstartpos = _startpos_e1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 523 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                       (  mkexp_ ~loc:_sloc (PEbin (OPdivide, e1, e2 )) )
# 5097 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1244)) : 'freshtv1246)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1251 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1247 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 519 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                     (  mkexp_ ~loc:_sloc (PEbin (OPshr, e1, e2)) )
# 5129 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1248)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1249 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1250)) : 'freshtv1252)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1257 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | MINUS | OBJECT | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1253 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 520 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                      (  mkexp_ ~loc:_sloc (PEbin (OPplus, e1, e2 )) )
# 5164 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1254)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1255 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1256)) : 'freshtv1258)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1261 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1259 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : 'tv_expression = let _endpos = _endpos_e2_ in
        let _symbolstartpos = _startpos_e1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 524 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                     (  mkexp_ ~loc:_sloc (PEbin (OPremainder, e1, e2 )) )
# 5189 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1260)) : 'freshtv1262)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1267 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | MINUS | OBJECT | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1263 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _startpos__2_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 521 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                       (  mkexp_ ~loc:_sloc (PEbin (OPminus, e1, e2 )) )
# 5217 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1265 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1266)) : 'freshtv1268)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1273 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1269 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 518 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                     (  mkexp_ ~loc:_sloc (PEbin (OPshl, e1, e2 )) )
# 5256 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1270)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1271 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1272)) : 'freshtv1274)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1279 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1275 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 517 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                        (  mkexp_ ~loc:_sloc (PEbin (OPbitand, e1, e2 )) )
# 5299 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1276)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1277 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1278)) : 'freshtv1280)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1285 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1281 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 528 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                         (  mkexp_ ~loc:_sloc (PEbin (OPne, e1, e2 )) )
# 5348 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1282)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1283 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1284)) : 'freshtv1286)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1291 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1287 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 515 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                        (  mkexp_ ~loc:_sloc (PEbin (OPbitor, e1, e2 )) )
# 5395 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1288)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1289 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1290)) : 'freshtv1292)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1297 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1293 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 530 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                        (  mkexp_ ~loc:_sloc (PEbin (OPle, e1, e2 )) )
# 5444 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1294)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1295 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1296)) : 'freshtv1298)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1303 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1299 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 529 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                      (  mkexp_ ~loc:_sloc (PEbin (OPlt, e1, e2 )) )
# 5493 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1300)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1301 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1302)) : 'freshtv1304)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1309 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1305 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 532 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                           (  mkexp_ ~loc:_sloc (PEbin (OPge, e1, e2 )) )
# 5542 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1306)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1307 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1308)) : 'freshtv1310)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1315 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1311 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 531 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                         (  mkexp_ ~loc:_sloc (PEbin (OPgt, e1, e2 )) )
# 5591 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1312)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1313 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1314)) : 'freshtv1316)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1321 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1317 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 527 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                       (  mkexp_ ~loc:_sloc (PEbin (OPeq, e1, e2 )) )
# 5640 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1318)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1319 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1320)) : 'freshtv1322)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1327 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONST | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1323 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 526 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                             (  mkexp_ ~loc:_sloc (PEbin (OPor,e1, e2 )) )
# 5705 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1324)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1325 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1326)) : 'freshtv1328)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1333 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1329 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 525 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                             (  mkexp_ ~loc:_sloc (PEbin (OPand, e1, e2 )) )
# 5768 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1330)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1331 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1332)) : 'freshtv1334)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1337 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1335 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_expression = let _endpos = _endpos_e_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 513 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                               (  mkexp_ ~loc:_sloc (PEun (OPnot, e)) )
# 5793 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1336)) : 'freshtv1338)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1341 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1339 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_expression = let _endpos = _endpos_e_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 514 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                 (  mkexp_ ~loc:_sloc (PEun (OPbitnot, e)) )
# 5811 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1340)) : 'freshtv1342)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1357 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 5819 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1353 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 5865 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 5870 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_struct_field = let _endpos = _endpos_e_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 544 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                              ( _1, mkexp_ ~loc:_sloc e.p_expression_desc )
# 5879 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1351) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_struct_field) = _v in
            ((match _menhir_s with
            | MenhirState96 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1345 * _menhir_state * 'tv_struct_fields) * _menhir_state) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_struct_field) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1343 * _menhir_state * 'tv_struct_fields) * _menhir_state) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : 'tv_struct_field) : 'tv_struct_field) = _v in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_struct_fields)), _) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_struct_fields = 
# 541 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                          ( _3 :: _1 )
# 5900 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_struct_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1344)) : 'freshtv1346)
            | MenhirState35 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1349) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_struct_field) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1347) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_struct_field) : 'tv_struct_field) = _v in
                ((let _v : 'tv_struct_fields = 
# 540 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                  ( [_1] )
# 5915 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_struct_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1348)) : 'freshtv1350)
            | _ ->
                _menhir_fail ()) : 'freshtv1352)) : 'freshtv1354)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1355 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 5927 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1356)) : 'freshtv1358)
    | MenhirState102 | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1365 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1359 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState102 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState102 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState102) : 'freshtv1360)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1361 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _endpos = _endpos_e_ in
            let _v : 'tv_comma_sep_expressions = let _endpos = _endpos_e_ in
            let _symbolstartpos = _startpos_e_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 537 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                  ( mkexp_ ~loc:_sloc e.p_expression_desc )
# 6012 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_comma_sep_expressions _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1362)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1363 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1364)) : 'freshtv1366)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1369 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1367 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_expression = let _endpos = _endpos_e_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 512 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                (  mkexp_ ~loc:_sloc (PEun (OPneg, e)) )
# 6037 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1368)) : 'freshtv1370)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1375 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1371 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BAR ->
                _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | IDENT _ | LBRACKET ->
                _menhir_reduce208 _menhir_env (Obj.magic _menhir_stack) MenhirState109
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109) : 'freshtv1372)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1373 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1374)) : 'freshtv1376)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1381 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1377 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120) : 'freshtv1378)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1379 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1380)) : 'freshtv1382)
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1387 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6209 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1383 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6251 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132) : 'freshtv1384)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1385 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6289 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1386)) : 'freshtv1388)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv1393 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6298 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1389 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6316 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134) : 'freshtv1390)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1391 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6402 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1392)) : 'freshtv1394)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1399 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6411 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1395 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6453 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140) : 'freshtv1396)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1397 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6491 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1398)) : 'freshtv1400)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv1409 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6500 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1405 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6510 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv1401 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6520 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6526 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ASSIGN ->
                    _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | EQUAL ->
                    _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState143
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState143) : 'freshtv1402)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv1403 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6548 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1404)) : 'freshtv1406)
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1407 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6595 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1408)) : 'freshtv1410)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv1415 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6604 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6608 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv1411 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6626 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6630 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146) : 'freshtv1412)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv1413 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6716 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6720 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1414)) : 'freshtv1416)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1421 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6729 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1417 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6771 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState152 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState152 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState152 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152) : 'freshtv1418)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1419 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6809 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1420)) : 'freshtv1422)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv1427 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6818 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1423 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6836 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv1424)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1425 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 6922 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1426)) : 'freshtv1428)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1433 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | ELSE | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1429 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : 'tv_command_core = let _endpos = _endpos_e_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 559 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                      ( PCemit (mkexp_ ~loc:_sloc e.p_expression_desc) )
# 6981 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1430)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1431 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1432)) : 'freshtv1434)
    | MenhirState501 | MenhirState491 | MenhirState30 | MenhirState201 | MenhirState114 | MenhirState190 | MenhirState117 | MenhirState187 | MenhirState120 | MenhirState121 | MenhirState183 | MenhirState124 | MenhirState134 | MenhirState146 | MenhirState178 | MenhirState176 | MenhirState174 | MenhirState154 | MenhirState158 | MenhirState170 | MenhirState159 | MenhirState160 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1441 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1435 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162) : 'freshtv1436)
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | ELSE | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1437 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos_e_ in
            let _v : 'tv_command_core = 
# 548 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                  ( PCyield e )
# 7070 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1438)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1439 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1440)) : 'freshtv1442)
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1447 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | ELSE | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1443 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_command_core = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 553 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                        ( (PCstore (mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc)) )
# 7135 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1444)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1445 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1446)) : 'freshtv1448)
    | MenhirState228 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1453 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1449 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ARRAY ->
                _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState230 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState230) : 'freshtv1450)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1451 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1452)) : 'freshtv1454)
    | MenhirState445 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1465) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7223 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1461) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7269 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7274 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__2_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _endpos = _endpos_e_ in
            let _v : 'tv_const_declaration = 
# 232 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( Hashtbl.add constant_table _2 e.p_expression_desc )
# 7282 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1459) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_v : 'tv_const_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1457 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_v : 'tv_const_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1455 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let ((_2 : 'tv_const_declaration) : 'tv_const_declaration) = _v in
            ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_declarations)) = _menhir_stack in
            let _endpos = _endpos__2_ in
            let _v : 'tv_declarations = 
# 220 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                    ( _1 )
# 7301 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_declarations _menhir_env _menhir_stack _endpos _v) : 'freshtv1456)) : 'freshtv1458)) : 'freshtv1460)) : 'freshtv1462)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1463) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7311 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1464)) : 'freshtv1466)
    | MenhirState497 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv1471 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7320 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run74 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run89 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run87 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | LET | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv1467 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7366 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _, (_3 : 'tv_method_kind)), _endpos__4_, (_4 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7371 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__4_), _), _endpos__6_, _, (_6 : 'tv_type_expression), _startpos__6_), _), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _1 = () in
            let _v : 'tv_object_field_or_method = let _endpos = _endpos_e_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 682 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( (* annotations are not used and only one (GHOST) method_kind is
           acceptable, just there to allow parser to factor with
           the next rule *)
    let is_ghost = match _3 with
      | MKnormal -> false
      | MKghost -> true
      | MKconstghost ->
        print_endline ("Warning: object fields cannot be declared `const ghost': " ^
          _4 ^ " marked as `ghost'.");
        true
      | k ->
        print_endline ("Warning: object fields cannot be declared `" ^
          string_of_method_kind k ^ "': " ^ _4 ^ " marked as normal.");
        false
      in `Left (_4, _6, (mkexp_ ~loc:_sloc e.p_expression_desc) , is_ghost) )
# 7396 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_object_field_or_method _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1468)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv1469 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7406 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1470)) : 'freshtv1472)
    | _ ->
        _menhir_fail ()

and _menhir_goto_method_kind : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_method_kind -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState273 | MenhirState287 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1215 * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1211 * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7430 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1207 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7442 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ARRAY ->
                    _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState291 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENTITY ->
                    _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIST ->
                    _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MAPPING ->
                    _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState291 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState291) : 'freshtv1208)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1209 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7470 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1210)) : 'freshtv1212)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1213 * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1214)) : 'freshtv1216)
    | MenhirState475 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1223 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1219 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7494 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1217 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7506 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState494 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ARRAY ->
                    _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState495 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState495 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENTITY ->
                    _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState495 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIST ->
                    _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState495 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState495 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MAPPING ->
                    _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState495 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState495) : 'freshtv1218)
            | IDENT _v ->
                _menhir_run488 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState494 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run477 _menhir_env (Obj.magic _menhir_stack) MenhirState494 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState494) : 'freshtv1220)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1221 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1222)) : 'freshtv1224)
    | _ ->
        _menhir_fail ()

and _menhir_run474 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState474 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST | CONSTRUCTOR | GHOST | IDENT _ | LOGICAL | REFINED ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState474
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState474

and _menhir_goto_base_slots : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_base_slots -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1205 * Lexing.position) * _menhir_state * 'tv_base_slots) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1197 * _menhir_state * 'tv_base_slots) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1195) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState464 in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7584 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLON ->
                _menhir_run458 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1193 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7600 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1194)) : 'freshtv1196)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState464) : 'freshtv1198)
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1201 * Lexing.position) * _menhir_state * 'tv_base_slots) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1199 * Lexing.position) * _menhir_state * 'tv_base_slots) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        ((let ((_menhir_stack, _startpos__1_), _, (_2 : 'tv_base_slots)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_base_layer_signature = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 664 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                              ( mklayersign ~loc:_sloc  (PLSconstr (List.rev _2)) )
# 7625 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_base_layer_signature _menhir_env _menhir_stack _v) : 'freshtv1200)) : 'freshtv1202)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1203 * Lexing.position) * _menhir_state * 'tv_base_slots) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1204)) : 'freshtv1206)

and _menhir_goto_layer_signature_fields : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_layer_signature_fields -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1191 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1189 * _menhir_state * 'tv_layer_signature_fields) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState331 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run328 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState332 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            _menhir_reduce214 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState332) : 'freshtv1190)
    | RBRACE ->
        _menhir_reduce213 _menhir_env (Obj.magic _menhir_stack) MenhirState331
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState331) : 'freshtv1192)

and _menhir_goto_object_signature_declaration : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_object_signature_declaration -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1187) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_object_signature_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1185) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let ((_1 : 'tv_object_signature_declaration) : 'tv_object_signature_declaration) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_declaration = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 225 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                  ( fst _1, mkdecl ~loc:_sloc (PDsignature (snd _1)) )
# 7686 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1186)) : 'freshtv1188)

and _menhir_run300 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | GHOST ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1171 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1165 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7710 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1163 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let ((_4 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7720 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7724 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos__4_ : Lexing.position) = _startpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_object_signature_expr), _startpos__1_), _startpos__3_) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_object_signature_expr = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 361 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                            ( mksig ~loc:_sloc (PSghostize (_1, [_4])) )
# 7738 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1164)) : 'freshtv1166)
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1167 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run303 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState312 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState312) : 'freshtv1168)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1169 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1170)) : 'freshtv1172)
    | LOGICAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1181 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1175 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7778 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1173 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let ((_4 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7788 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7792 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos__4_ : Lexing.position) = _startpos in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_object_signature_expr), _startpos__1_), _endpos__3_, _startpos__3_) = _menhir_stack in
            let _3 = () in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_object_signature_expr = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 363 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                              ( mksig ~loc:_sloc (PSlogicize (_1, [_4])) )
# 7806 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1174)) : 'freshtv1176)
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1177 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run303 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState302 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState302) : 'freshtv1178)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1179 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _, _), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1180)) : 'freshtv1182)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1183 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1184)

and _menhir_run316 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1157 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7851 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1155 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        let ((_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7861 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 7865 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) = _v in
        let (_startpos__3_ : Lexing.position) = _startpos in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_object_signature_expr), _startpos__1_), _startpos__2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_object_signature_expr = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 365 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                       ( mksig ~loc:_sloc (PSminus (_1, [_3])) )
# 7878 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1156)) : 'freshtv1158)
    | LBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1159 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run303 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState317 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState317) : 'freshtv1160)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1161 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1162)

and _menhir_goto_object_expression : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_object_expression -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState355 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1123 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run361 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1119 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1117 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_object_expression), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_object_expression = 
# 735 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                     ( _2 )
# 7931 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_object_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1118)) : 'freshtv1120)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1121 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1122)) : 'freshtv1124)
    | MenhirState367 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1129 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 7946 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run361 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1125 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 7958 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 7963 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__3_), _endpos__4_), _endpos__6_, _, (_6 : 'tv_object_expression), _startpos__6_) = _menhir_stack in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_layer_obj_inst = let _endpos = _endpos__6_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 784 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                            ( mkobjinst ~loc:_sloc (POexternal ((CONaddress _3), _6)) )
# 7975 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_layer_obj_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1126)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1127 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 7985 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1128)) : 'freshtv1130)
    | MenhirState371 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1135 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 7994 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run361 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1131 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8006 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8011 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__3_), _endpos__4_), _endpos__6_, _, (_6 : 'tv_object_expression), _startpos__6_) = _menhir_stack in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_layer_obj_inst = let _endpos = _endpos__6_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 783 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                           ( mkobjinst ~loc:_sloc (POexternal ((CONaddress _3), _6)) )
# 8023 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_layer_obj_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1132)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1133 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8033 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1134)) : 'freshtv1136)
    | MenhirState354 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1141 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run361 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1137 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_object_expression), _startpos__1_) = _menhir_stack in
            let _v : 'tv_layer_obj_inst = let _endpos = _endpos__1_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 782 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                      ( mkobjinst ~loc:_sloc (POinternal _1) )
# 8055 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_layer_obj_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1138)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1139 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1140)) : 'freshtv1142)
    | MenhirState468 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1147 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8070 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run361 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1143 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8082 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _endpos__1_, (_1 : 'tv_opt_logical_or_trusted), _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8087 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__3_), _endpos__5_, _, (_5 : 'tv_object_expression), _startpos__5_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__5_ in
            let _v : 'tv_object_declaration = let _endpos = _endpos__5_ in
            let _symbolstartpos = if _startpos__1_ != _endpos__1_ then
              _startpos__1_
            else
              _startpos__2_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 657 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _3, { pObjectType = None; pObjectDesc = _5; pObjectLoc = (make_loc _sloc) } )
# 8102 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_object_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv1144)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1145 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8112 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1146)) : 'freshtv1148)
    | MenhirState507 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv1153 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8121 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run361 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv1149 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8133 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _endpos__1_, (_1 : 'tv_opt_logical_or_trusted), _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8138 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__3_), (_4 : 'tv_base_layer_signature)), _endpos__6_, _, (_6 : 'tv_object_signature_expr), _startpos__6_), _endpos__8_, _, (_8 : 'tv_object_expression), _startpos__8_) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__8_ in
            let _v : 'tv_object_declaration = let _endpos = _endpos__8_ in
            let _symbolstartpos = if _startpos__1_ != _endpos__1_ then
              _startpos__1_
            else
              _startpos__2_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 654 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _3, { pObjectType = Some { pObjectBase = _4; pObjectSignature = _6; pObjectTypLoc = (make_loc _sloc) };
              pObjectDesc = _8; pObjectLoc = (make_loc _sloc) } )
# 8155 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_object_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv1150)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv1151 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8165 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1152)) : 'freshtv1154)
    | _ ->
        _menhir_fail ()

and _menhir_goto_layer_type : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_layer_type -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState341 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1111) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_layer_type) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1109) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_layer_type) : 'tv_layer_type) = _v in
        ((let _1 = () in
        let _v : 'tv_layer_signature_annotation = 
# 753 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                      ( Some _2 )
# 8190 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_signature_annotation _menhir_env _menhir_stack _v) : 'freshtv1110)) : 'freshtv1112)
    | MenhirState391 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1115 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_layer_type) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1113 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_layer_type) : 'tv_layer_type) = _v in
        ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_layer_expression), _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_layer_expression = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 764 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      (  mklayer ~loc:_sloc (PLrelax (_1, _3)) )
# 8214 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1114)) : 'freshtv1116)
    | _ ->
        _menhir_fail ()

and _menhir_goto_declaration : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_declaration -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1107 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_declaration) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1105 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
    let (_endpos__2_ : Lexing.position) = _endpos in
    let ((_2 : 'tv_declaration) : 'tv_declaration) = _v in
    ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_declarations)) = _menhir_stack in
    let _endpos = _endpos__2_ in
    let _v : 'tv_declarations = 
# 218 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                              ( _2 :: _1 )
# 8235 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_declarations _menhir_env _menhir_stack _endpos _v) : 'freshtv1106)) : 'freshtv1108)

and _menhir_run477 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run479 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState477 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1103 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState477 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1101 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_method_param = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 717 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                   ( ["()", Some (mkfotyp ~loc:_sloc (PTbuiltin Tunit))] )
# 8266 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_method_param _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1102)) : 'freshtv1104)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState477

and _menhir_run488 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8277 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1099) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8288 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8292 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_method_param = 
# 716 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
           ( [_1, None] )
# 8298 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_method_param _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1100)

and _menhir_goto_external_declaration : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_external_declaration -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1097 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_external_declaration) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1095 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
    let (_endpos__2_ : Lexing.position) = _endpos in
    let ((_2 : 'tv_external_declaration) : 'tv_external_declaration) = _v in
    ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_declarations)) = _menhir_stack in
    let _endpos = _endpos__2_ in
    let _v : 'tv_declarations = 
# 221 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                       ( _2 :: _1 )
# 8317 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_declarations _menhir_env _menhir_stack _endpos _v) : 'freshtv1096)) : 'freshtv1098)

and _menhir_goto_layer_signature_annotation : _menhir_env -> 'ttv_tail -> 'tv_layer_signature_annotation -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv1093 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8328 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1089 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8338 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run382 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState350 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run352 _menhir_env (Obj.magic _menhir_stack) MenhirState350 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run351 _menhir_env (Obj.magic _menhir_stack) MenhirState350 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState350) : 'freshtv1090)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1091 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8360 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1092)) : 'freshtv1094)

and _menhir_run342 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run337 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState342 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run326 _menhir_env (Obj.magic _menhir_stack) MenhirState342 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1087 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState342 in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run337 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState343 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run326 _menhir_env (Obj.magic _menhir_stack) MenhirState343 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState343) : 'freshtv1088)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState342

and _menhir_goto_constructor_declarations : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_constructor_declarations -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1085 * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_constructor_declarations) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1079 * Lexing.position * _menhir_state * 'tv_constructor_declarations) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run255 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState266 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState266) : 'freshtv1080)
    | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1081 * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_constructor_declarations) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_opt_bar)), _endpos__2_, _, (_2 : 'tv_constructor_declarations)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_type_definition = 
# 280 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                               ( PTbranches (List.rev _2) )
# 8425 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_type_definition _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1082)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1083 * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_constructor_declarations) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1084)) : 'freshtv1086)

and _menhir_run244 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8439 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1075 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8451 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARRAY ->
            _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState245 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState245 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState245 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIST ->
            _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState245 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState245 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MAPPING ->
            _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState245 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState245) : 'freshtv1076)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1077 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8479 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1078)

and _menhir_goto_annotation : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_annotation -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState211 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1069 * _menhir_state * 'tv_annotations_plus)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotation) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1067 * _menhir_state * 'tv_annotations_plus)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_annotation) : 'tv_annotation) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_annotations_plus)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_annotations_plus = 
# 794 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                       ( _3 :: _1 )
# 8501 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_annotations_plus _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1068)) : 'freshtv1070)
    | MenhirState6 | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1073) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotation) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1071) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_annotation) : 'tv_annotation) = _v in
        ((let _v : 'tv_annotations_plus = 
# 793 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                ( [_1] )
# 8516 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_annotations_plus _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1072)) : 'freshtv1074)
    | _ ->
        _menhir_fail ()

and _menhir_reduce10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_annotation_arguments = 
# 807 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( [] )
# 8527 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8534 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1065) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8545 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8549 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation_arguments = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    
# 809 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
          ( [PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc  (PEconstant (CONuint (int_of_string(_1)))))))] )
# 8558 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1066)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8565 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1063) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8576 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8580 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation_arguments = 
# 813 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
            ( [PAclause (_1, [])] )
# 8586 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1064)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | IDENT _v ->
        _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1061 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState11 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1059 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_annotation_arguments = 
# 814 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                   ( [] )
# 8620 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1060)) : 'freshtv1062)
    | STRING _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run21 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8635 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1057) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8646 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8650 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation_arguments = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    
# 808 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
         ( [PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc (PEconstant (CONint (int_of_string(_1)))))))] )
# 8659 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1058)

and _menhir_run22 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1053 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1041 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8684 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1037 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8696 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1035 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8704 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8710 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_annotation_arguments = let _endpos = _endpos__4_ in
                let _startpos = _startpos__1_ in
                let _loc = (_startpos, _endpos) in
                
# 810 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                               ( [PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc (PEconstant (CONaddress _3)))))] )
# 8721 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1036)) : 'freshtv1038)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1039 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8731 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1040)) : 'freshtv1042)
        | UINT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1049 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8742 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1045 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8754 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1043 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8762 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8768 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_annotation_arguments = let _endpos = _endpos__4_ in
                let _startpos = _startpos__1_ in
                let _loc = (_startpos, _endpos) in
                
# 811 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                ( [PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc (PEconstant (CONaddress _3)))))] )
# 8779 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1044)) : 'freshtv1046)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1047 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 8789 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1048)) : 'freshtv1050)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1051 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1052)) : 'freshtv1054)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1055 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1056)

and _menhir_run28 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8811 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1033) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8822 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8826 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation_arguments = 
# 812 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
           ( [PAclause (_1, [])] )
# 8832 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1034)

and _menhir_run107 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107

and _menhir_run115 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1029 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8878 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1025 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8890 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState117 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState117 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState117) : 'freshtv1026)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1027 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 8948 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1028)) : 'freshtv1030)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1031 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1032)

and _menhir_run118 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState118 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState118 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState118

and _menhir_run121 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BEGIN ->
        _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EMIT ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FAIL ->
        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FIRST ->
        _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOLD ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MATCH ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState121 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState121 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState121

and _menhir_run125 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState125 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _ ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState125
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState125

and _menhir_run135 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState135 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _ ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState135
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState135

and _menhir_run147 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState147 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _ ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState147
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState147

and _menhir_run155 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1023) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_command_core = 
# 554 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
          ( PCfail )
# 9095 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1024)

and _menhir_run156 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156

and _menhir_run158 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BEGIN ->
        _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EMIT ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FAIL ->
        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FIRST ->
        _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOLD ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MATCH ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158

and _menhir_run159 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSERT ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BEGIN ->
        _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DENY ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EMIT ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FAIL ->
        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FIRST ->
        _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOLD ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | GHOST ->
        _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MATCH ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState159 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState159 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState159

and _menhir_run160 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BEGIN ->
        _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EMIT ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FAIL ->
        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FIRST ->
        _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOLD ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run122 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MATCH ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160

and _menhir_goto_opt_type_annotation : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_opt_type_annotation -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState479 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1009 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9284 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1007 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9290 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9295 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__1_), _, (_2 : 'tv_opt_type_annotation)) = _menhir_stack in
        let _v : 'tv_method_parameter = 
# 725 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                               ( _1, _2 )
# 9300 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1005) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_method_parameter) = _v in
        ((match _menhir_s with
        | MenhirState485 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv999 * _menhir_state * 'tv_method_parameters)) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_method_parameter) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv997 * _menhir_state * 'tv_method_parameters)) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((_3 : 'tv_method_parameter) : 'tv_method_parameter) = _v in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_method_parameters)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_method_parameters = 
# 722 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                              ( _3 :: _1 )
# 9321 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_method_parameters _menhir_env _menhir_stack _menhir_s _v) : 'freshtv998)) : 'freshtv1000)
        | MenhirState477 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1003) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_method_parameter) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1001) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_method_parameter) : 'tv_method_parameter) = _v in
            ((let _v : 'tv_method_parameters = 
# 721 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                     ( [_1] )
# 9336 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_method_parameters _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1002)) : 'freshtv1004)
        | _ ->
            _menhir_fail ()) : 'freshtv1006)) : 'freshtv1008)) : 'freshtv1010)
    | MenhirState489 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1015 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1011 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState491 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState491 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState491 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState491 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState491) : 'freshtv1012)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1013 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1014)) : 'freshtv1016)
    | MenhirState499 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1021 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9411 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1017 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9421 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState501 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState501 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState501 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState501 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState501) : 'freshtv1018)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1019 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9479 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1020)) : 'freshtv1022)
    | _ ->
        _menhir_fail ()

and _menhir_goto_indexed_opt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_indexed_opt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv995 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9493 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_indexed_opt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv991 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9503 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_indexed_opt) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv989 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9511 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_indexed_opt) = Obj.magic _menhir_stack in
        let (_endpos__6_ : Lexing.position) = _endpos in
        ((let ((((_menhir_stack, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9517 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__2_), _endpos__4_, _, (_4 : 'tv_type_expression), _startpos__4_), _, (_5 : 'tv_indexed_opt)) = _menhir_stack in
        let _6 = () in
        let _3 = () in
        let _1 = () in
        let _endpos = _endpos__6_ in
        let _v : 'tv_event_parameter = 
# 344 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( (_2, _4, _5) )
# 9526 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv987) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_event_parameter) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv985 * Lexing.position * 'tv_event_parameters) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_event_parameter) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv983 * Lexing.position * 'tv_event_parameters) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let ((_2 : 'tv_event_parameter) : 'tv_event_parameter) = _v in
        ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_event_parameters)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_event_parameters = 
# 334 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                      ( _2 :: _1 )
# 9545 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_event_parameters _menhir_env _menhir_stack _endpos _v) : 'freshtv984)) : 'freshtv986)) : 'freshtv988)) : 'freshtv990)) : 'freshtv992)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv993 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9555 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_indexed_opt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv994)) : 'freshtv996)

and _menhir_run128 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv981) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_eq_or_assign = 
# 593 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
          ( () )
# 9570 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_eq_or_assign _menhir_env _menhir_stack _menhir_s _v) : 'freshtv982)

and _menhir_run129 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv979) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_eq_or_assign = 
# 594 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
           ( () )
# 9584 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_eq_or_assign _menhir_env _menhir_stack _menhir_s _v) : 'freshtv980)

and _menhir_goto_object_signature_field : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_object_signature_field -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState287 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv973 * _menhir_state * 'tv_object_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_object_signature_field) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv971 * _menhir_state * 'tv_object_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_object_signature_field) : 'tv_object_signature_field) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_object_signature_fields)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_object_signature_fields = 
# 369 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                              ( _3 :: _1 )
# 9605 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_object_signature_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv972)) : 'freshtv974)
    | MenhirState273 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv977) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_object_signature_field) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv975) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_object_signature_field) : 'tv_object_signature_field) = _v in
        ((let _v : 'tv_object_signature_fields = 
# 368 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                            ( [_1] )
# 9620 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_object_signature_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv976)) : 'freshtv978)
    | _ ->
        _menhir_fail ()

and _menhir_goto_constructor_parameters : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_constructor_parameters -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv969 * Lexing.position * 'tv_constructor_parameters) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv959) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run194 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState259 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState259) : 'freshtv960)
    | BAR | CONST | EOF | EVENT | EXTERNAL | LAYER | LBRACKET | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv965 * Lexing.position * 'tv_constructor_parameters) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_constructor_parameters)) = _menhir_stack in
        let _endpos = _endpos__1_ in
        let _v : 'tv_constructor_params = 
# 304 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( List.fold_left (fun a b -> List.rev_append b a) [] _1 )
# 9656 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv963) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_constructor_params) = _v in
        ((let _menhir_stack = (_menhir_stack, _endpos, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv961 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9667 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_constructor_params) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACKET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState256 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BAR | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState256
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState256) : 'freshtv962)) : 'freshtv964)) : 'freshtv966)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv967 * Lexing.position * 'tv_constructor_parameters) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv968)) : 'freshtv970)

and _menhir_goto_field_declarations : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_field_declarations -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv957 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv955 * _menhir_state * 'tv_field_declarations) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState247 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run244 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState248 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            _menhir_reduce214 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState248) : 'freshtv956)
    | RBRACE ->
        _menhir_reduce213 _menhir_env (Obj.magic _menhir_stack) MenhirState247
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState247) : 'freshtv958)

and _menhir_goto_type_declaration : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_type_declaration -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv953) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_type_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv951) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let ((_1 : 'tv_type_declaration) : 'tv_type_declaration) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_declaration = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 224 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                      ( fst _1, mkdecl ~loc:_sloc (PDtype (snd _1)) )
# 9737 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv952)) : 'freshtv954)

and _menhir_run233 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARRAY ->
        _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState233 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIST ->
        _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MAPPING ->
        _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState233 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState233

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run428 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9772 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv949) = Obj.magic _menhir_stack in
    ((let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_event_parameters = 
# 333 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( [] )
# 9783 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_event_parameters _menhir_env _menhir_stack _endpos _v) : 'freshtv950)

and _menhir_run255 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9790 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv947) = Obj.magic _menhir_stack in
    ((let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_constructor_parameters = 
# 307 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( [] )
# 9801 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_constructor_parameters _menhir_env _menhir_stack _endpos _v) : 'freshtv948)

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv943 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOUBLEARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv939 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSERT ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState114 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState114 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState114) : 'freshtv940)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv941 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv942)) : 'freshtv944)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv945 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv946)

and _menhir_run193 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9889 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLONCOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv935) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState193 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv931 * _menhir_state) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9911 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv929 * _menhir_state) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let ((_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9921 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 9925 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos__2_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_match_pattern_tail = 
# 628 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                     ( PPTcons _2 )
# 9933 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_match_pattern_tail _menhir_env _menhir_stack _menhir_s _v) : 'freshtv930)) : 'freshtv932)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv933 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv934)) : 'freshtv936)
    | IDENT _v ->
        _menhir_run194 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState193 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOUBLEARROW ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv937) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState193 in
        ((let _v : 'tv_match_pattern_tail = 
# 627 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
          ( PPTother [] )
# 9952 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_match_pattern_tail _menhir_env _menhir_stack _menhir_s _v) : 'freshtv938)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState193

and _menhir_goto_nonempty_list_atom_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_nonempty_list_atom_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState501 | MenhirState497 | MenhirState491 | MenhirState445 | MenhirState228 | MenhirState30 | MenhirState201 | MenhirState114 | MenhirState190 | MenhirState117 | MenhirState187 | MenhirState120 | MenhirState121 | MenhirState183 | MenhirState124 | MenhirState134 | MenhirState146 | MenhirState178 | MenhirState176 | MenhirState174 | MenhirState154 | MenhirState158 | MenhirState170 | MenhirState159 | MenhirState162 | MenhirState160 | MenhirState156 | MenhirState152 | MenhirState150 | MenhirState144 | MenhirState140 | MenhirState138 | MenhirState132 | MenhirState130 | MenhirState118 | MenhirState107 | MenhirState32 | MenhirState102 | MenhirState33 | MenhirState37 | MenhirState46 | MenhirState89 | MenhirState87 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState74 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv919 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv913 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState49 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49) : 'freshtv914)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | MINUS | MOD | OBJECT | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | SLASH | STAR | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv915 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_a_, _menhir_s, (a : 'tv_nonempty_list_atom_), _startpos_a_) = _menhir_stack in
            let _startpos = _startpos_a_ in
            let _endpos = _endpos_a_ in
            let _v : 'tv_expression = let _endpos = _endpos_a_ in
            let _symbolstartpos = _startpos_a_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 505 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
            ( match a with
      | [hd] -> hd
      | _ -> mkexp_ ~loc:_sloc (PEapp a)
    )
# 10005 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv916)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv917 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv918)) : 'freshtv920)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv923 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position)) * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv921 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position)) * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_a1_, _menhir_s, (a1 : 'tv_nonempty_list_atom_), _startpos_a1_), _endpos_a2_, _, (a2 : 'tv_nonempty_list_atom_), _startpos_a2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_a1_ in
        let _endpos = _endpos_a2_ in
        let _v : 'tv_expression = let _endpos = _endpos_a2_ in
        let _symbolstartpos = _startpos_a1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 509 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                          ( let x = mkexp_ ~loc:_sloc (PEfield (a1, a2)) in
      (* print_endline ("PEfield: " ^ (string_of_p_expression x)); *) x
    )
# 10032 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv922)) : 'freshtv924)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv927 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv925 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_atom), _startpos_x_), _endpos_xs_, _, (xs : 'tv_nonempty_list_atom_), _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_atom_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 10046 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_nonempty_list_atom_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv926)) : 'freshtv928)
    | _ ->
        _menhir_fail ()

and _menhir_reduce166 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_method_kind = 
# 378 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
             ( MKnormal )
# 10057 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v

and _menhir_run274 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv911) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_method_kind = 
# 384 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
            ( MKrefined )
# 10071 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv912)

and _menhir_run276 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv909) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _v : 'tv_method_kind = 
# 383 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
             ( MKlogical )
# 10087 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv910)

and _menhir_run277 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv903 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv901 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_method_kind = 
# 382 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                ( MKconstghost )
# 10109 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv902)) : 'freshtv904)
    | IDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv905 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_method_kind = 
# 380 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
             ( MKghost )
# 10120 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv906)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv907 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv908)

and _menhir_run279 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv897 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARRAY ->
            _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState280 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState280 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState280 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIST ->
            _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState280 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState280 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MAPPING ->
            _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState280 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState280) : 'freshtv898)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv899 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv900)

and _menhir_run284 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | GHOST ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv891 * _menhir_state) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv889 * _menhir_state) = Obj.magic _menhir_stack in
        let (_startpos__2_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_method_kind = 
# 381 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                ( MKconstghost )
# 10187 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv890)) : 'freshtv892)
    | IDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv893 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_method_kind = 
# 379 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
             ( MKconst )
# 10198 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv894)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv895 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv896)

and _menhir_goto_object_signature_expr : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_object_signature_expr -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState272 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv841 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10218 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run316 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WITH ->
            _menhir_run300 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv837 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10232 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10237 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__2_), _endpos__4_, _, (_4 : 'tv_object_signature_expr), _startpos__4_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_object_signature_declaration = 
# 350 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _2, _4 )
# 10246 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv838)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv839 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10256 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv840)) : 'freshtv842)
    | MenhirState329 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv857 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10265 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run316 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WITH ->
            _menhir_run300 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv853 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10279 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10284 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__1_), _endpos__3_, _, (_3 : 'tv_object_signature_expr), _startpos__3_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_layer_signature_field = 
# 408 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                       ( _1, _3 )
# 10290 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv851) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_layer_signature_field) = _v in
            ((match _menhir_s with
            | MenhirState332 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv845 * _menhir_state * 'tv_layer_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_layer_signature_field) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv843 * _menhir_state * 'tv_layer_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : 'tv_layer_signature_field) : 'tv_layer_signature_field) = _v in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_layer_signature_fields)), _) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_layer_signature_fields = 
# 405 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                            ( _3 :: _1 )
# 10311 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_layer_signature_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv844)) : 'freshtv846)
            | MenhirState326 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv849) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_layer_signature_field) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv847) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_layer_signature_field) : 'tv_layer_signature_field) = _v in
                ((let _v : 'tv_layer_signature_fields = 
# 404 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                           ( [_1] )
# 10326 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_layer_signature_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv848)) : 'freshtv850)
            | _ ->
                _menhir_fail ()) : 'freshtv852)) : 'freshtv854)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv855 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10338 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv856)) : 'freshtv858)
    | MenhirState452 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv863 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10347 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run316 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WITH ->
            _menhir_run300 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv859 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10361 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _endpos__1_, (_1 : 'tv_opt_logical_or_trusted), _startpos__1_), _startpos__2_), _startpos__3_), _endpos__4_, (_4 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10366 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__4_), _endpos__6_, _, (_6 : 'tv_object_signature_expr), _startpos__6_) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__6_ in
            let _v : 'tv_object_signature_declaration = 
# 353 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( if _1 <> POnormal then
          print_endline "Warning: object signatures should not be marked logical or trusted";
        _4, _6 )
# 10378 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv860)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv861 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10388 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv862)) : 'freshtv864)
    | MenhirState458 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv879 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10397 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run316 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WITH ->
            _menhir_run300 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv875 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10411 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10416 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__1_), _endpos__3_, _, (_3 : 'tv_object_signature_expr), _startpos__3_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_base_slot = 
# 671 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                       ( _1, _3 )
# 10422 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv873) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_base_slot) = _v in
            ((match _menhir_s with
            | MenhirState464 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv867 * _menhir_state * 'tv_base_slots)) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_base_slot) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv865 * _menhir_state * 'tv_base_slots)) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : 'tv_base_slot) : 'tv_base_slot) = _v in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_base_slots)) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_base_slots = 
# 668 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                ( _3 :: _1 )
# 10443 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_base_slots _menhir_env _menhir_stack _menhir_s _v) : 'freshtv866)) : 'freshtv868)
            | MenhirState455 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv871) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_base_slot) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv869) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_base_slot) : 'tv_base_slot) = _v in
                ((let _v : 'tv_base_slots = 
# 667 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
               ( [_1] )
# 10458 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_base_slots _menhir_env _menhir_stack _menhir_s _v) : 'freshtv870)) : 'freshtv872)
            | _ ->
                _menhir_fail ()) : 'freshtv874)) : 'freshtv876)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv877 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10470 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv878)) : 'freshtv880)
    | MenhirState471 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv887 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10479 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv881 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10489 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CLONE ->
                _menhir_run357 _menhir_env (Obj.magic _menhir_stack) MenhirState507 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run356 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState507 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run355 _menhir_env (Obj.magic _menhir_stack) MenhirState507 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState507) : 'freshtv882)
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv883 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10509 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LET ->
                _menhir_run474 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState473 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState473) : 'freshtv884)
        | MINUS ->
            _menhir_run316 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WITH ->
            _menhir_run300 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv885 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10533 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv886)) : 'freshtv888)
    | _ ->
        _menhir_fail ()

and _menhir_goto_base_layer_signature : _menhir_env -> 'ttv_tail -> 'tv_base_layer_signature -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv835 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10547 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    ) * Lexing.position) * 'tv_base_layer_signature) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv831 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10557 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run298 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState471 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState471 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState471) : 'freshtv832)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv833 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10577 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv834)) : 'freshtv836)

and _menhir_run458 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10584 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run298 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState458 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState458 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState458

and _menhir_run355 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLONE ->
        _menhir_run357 _menhir_env (Obj.magic _menhir_stack) MenhirState355 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run356 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState355 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run355 _menhir_env (Obj.magic _menhir_stack) MenhirState355 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState355

and _menhir_run356 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10619 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv829) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10630 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10634 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_object_expression = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 732 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
           ( mkobj ~loc:_sloc (POname _1) )
# 10645 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_object_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv830)

and _menhir_run357 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv825 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10662 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv823 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let ((_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10672 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10676 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) = _v in
        let (_startpos__2_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_object_expression = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 733 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                 (mkobj ~loc:_sloc (POclone _2))
# 10689 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_object_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv824)) : 'freshtv826)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv827 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv828)

and _menhir_goto_layer_signature : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_layer_signature -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState325 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv795 * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10709 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv793 * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10715 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10720 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__3_), _endpos__5_, _, (_5 : 'tv_layer_signature)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_layer_signature_declaration = 
# 397 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _3, _5 )
# 10730 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv791) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_layer_signature_declaration) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv789) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_layer_signature_declaration) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv787) = Obj.magic _menhir_stack in
        let (_endpos__1_ : Lexing.position) = _endpos in
        let ((_1 : 'tv_layer_signature_declaration) : 'tv_layer_signature_declaration) = _v in
        let (_startpos__1_ : Lexing.position) = _startpos in
        ((let _endpos = _endpos__1_ in
        let _v : 'tv_declaration = let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 226 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                 ( fst _1, mkdecl ~loc:_sloc  (PDlayer_sig (snd _1)) )
# 10754 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv788)) : 'freshtv790)) : 'freshtv792)) : 'freshtv794)) : 'freshtv796)
    | MenhirState343 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv799 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv797 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _), _endpos__3_, _, (_3 : 'tv_layer_signature)) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos__3_ in
        let _v : 'tv_layer_type = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 416 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( { pLayerBase = mklayersign ~loc:_sloc (PLSconstr []); pLayerSignature = _3; pLayerLoc=(make_loc _sloc) } )
# 10772 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_type _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv798)) : 'freshtv800)
    | MenhirState342 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv805 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv801 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run337 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState346 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run326 _menhir_env (Obj.magic _menhir_stack) MenhirState346 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState346) : 'freshtv802)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv803 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv804)) : 'freshtv806)
    | MenhirState346 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv809 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv807 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_layer_signature)), _endpos__3_), _endpos__4_, _, (_4 : 'tv_layer_signature)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _endpos = _endpos__4_ in
        let _v : 'tv_layer_type = let _endpos = _endpos__4_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 414 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( { pLayerBase = _2; pLayerSignature = _4; pLayerLoc =(make_loc _sloc) } )
# 10819 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_type _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv808)) : 'freshtv810)
    | MenhirState361 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv813 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv811 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_object_expression), _startpos__1_), _endpos__3_, _, (_3 : 'tv_layer_signature)) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_object_expression = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 734 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                    ( mkobj ~loc:_sloc (POrelax (_1, _3)) )
# 10837 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_object_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv812)) : 'freshtv814)
    | MenhirState455 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv821 * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv817 * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv815 * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _startpos__1_), _endpos__2_, _, (_2 : 'tv_layer_signature)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_base_layer_signature = 
# 663 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                   ( _2 )
# 10860 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_base_layer_signature _menhir_env _menhir_stack _v) : 'freshtv816)) : 'freshtv818)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv819 * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv820)) : 'freshtv822)
    | _ ->
        _menhir_fail ()

and _menhir_run328 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10876 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv783 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10888 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run298 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState329 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState329 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState329) : 'freshtv784)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv785 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10908 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv786)

and _menhir_reduce142 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10916 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 10922 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )), _startpos__1_) = _menhir_stack in
    let _endpos = _endpos__1_ in
    let _v : 'tv_layer_signature = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 400 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
           ( mklayersign ~loc:_sloc (PLSname _1) )
# 10931 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_layer_signature _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 10938 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv781) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 10949 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 10953 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    
# 800 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
          ( PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc  (PEconstant (CONuint (int_of_string(_1))))))) )
# 10962 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv782)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 10969 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COMMA | RBRACKET | RPAREN ->
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState8
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState8

and _menhir_goto_annotations : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_annotations -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv713 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv709 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11012 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | EQUAL ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState127
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127) : 'freshtv710)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv711 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv712)) : 'freshtv714)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv719 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv715 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11047 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | EQUAL ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState137
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137) : 'freshtv716)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv717 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv718)) : 'freshtv720)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv725 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv721 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11082 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | EQUAL ->
                _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState149
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149) : 'freshtv722)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv723 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv724)) : 'freshtv726)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv733 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11109 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv729 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11119 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BAR ->
                _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | LBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv727) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState242 in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run244 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState243 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState243) : 'freshtv728)
            | IDENT _ ->
                _menhir_reduce208 _menhir_env (Obj.magic _menhir_stack) MenhirState242
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState242) : 'freshtv730)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv731 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11154 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv732)) : 'freshtv734)
    | MenhirState256 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv747 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11163 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_constructor_params) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv745 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11169 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_constructor_params) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11174 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )), _startpos__1_), _endpos__2_, (_2 : 'tv_constructor_params)), _endpos__3_, _, (_3 : 'tv_annotations)) = _menhir_stack in
        let _endpos = _endpos__3_ in
        let _v : 'tv_constructor_declaration = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 297 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( { pTypeConstrName = _1;
          pTypeConstrArgs = _2;
          pTypeConstrAnnotations = _3;
          pTypeConstrLoc = (make_loc _sloc ) } )
# 11186 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv743) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_constructor_declaration) = _v in
        ((match _menhir_s with
        | MenhirState266 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv737 * Lexing.position * _menhir_state * 'tv_constructor_declarations)) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_constructor_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv735 * Lexing.position * _menhir_state * 'tv_constructor_declarations)) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            let ((_3 : 'tv_constructor_declaration) : 'tv_constructor_declaration) = _v in
            ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_constructor_declarations)) = _menhir_stack in
            let _2 = () in
            let _endpos = _endpos__3_ in
            let _v : 'tv_constructor_declarations = 
# 293 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                          ( _3 :: _1 )
# 11211 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_constructor_declarations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv736)) : 'freshtv738)
        | MenhirState254 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv741) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_constructor_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv739) = Obj.magic _menhir_stack in
            let (_endpos__1_ : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_constructor_declaration) : 'tv_constructor_declaration) = _v in
            ((let _endpos = _endpos__1_ in
            let _v : 'tv_constructor_declarations = 
# 292 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                             ( [_1] )
# 11229 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_constructor_declarations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv740)) : 'freshtv742)
        | _ ->
            _menhir_fail ()) : 'freshtv744)) : 'freshtv746)) : 'freshtv748)
    | MenhirState339 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv755 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11239 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv749) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACKET ->
                _menhir_run342 _menhir_env (Obj.magic _menhir_stack) MenhirState341 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState341) : 'freshtv750)
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv751) = Obj.magic _menhir_stack in
            ((let _v : 'tv_layer_signature_annotation = 
# 752 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( None )
# 11262 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_layer_signature_annotation _menhir_env _menhir_stack _v) : 'freshtv752)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv753 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11272 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv754)) : 'freshtv756)
    | MenhirState401 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv757 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11281 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState402
        | EQUAL ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState402
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState402) : 'freshtv758)
    | MenhirState407 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv763 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11299 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv759 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11309 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ARRAY ->
                _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState409 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState409 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState409) : 'freshtv760)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv761 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11337 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv762)) : 'freshtv764)
    | MenhirState399 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv775 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv771 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | STRING _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv767 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11360 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv765 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                let ((_4 : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11370 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) : (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11374 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) = _v in
                let (_startpos__4_ : Lexing.position) = _startpos in
                ((let ((_menhir_stack, _endpos__1_, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _endpos = _endpos__4_ in
                let _v : 'tv_external_declaration = let _endpos = _endpos__4_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 236 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( "", mkdecl ~loc:_sloc (PDexternal_with (_4, _2)) )
# 11387 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv766)) : 'freshtv768)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv769 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv770)) : 'freshtv772)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv773 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv774)) : 'freshtv776)
    | MenhirState474 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv779 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONST ->
            _menhir_run284 _menhir_env (Obj.magic _menhir_stack) MenhirState475
        | CONSTRUCTOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv777 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState475 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run488 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState476 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run477 _menhir_env (Obj.magic _menhir_stack) MenhirState476 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState476) : 'freshtv778)
        | GHOST ->
            _menhir_run277 _menhir_env (Obj.magic _menhir_stack) MenhirState475 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LOGICAL ->
            _menhir_run276 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState475 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REFINED ->
            _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState475
        | IDENT _ ->
            _menhir_reduce166 _menhir_env (Obj.magic _menhir_stack) MenhirState475
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState475) : 'freshtv780)
    | _ ->
        _menhir_fail ()

and _menhir_run13 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11446 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv707) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11457 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11461 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    
# 799 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
         ( PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc  (PEconstant (CONint (int_of_string(_1))))))) )
# 11470 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv708)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv703 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv691 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11495 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv687 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11507 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv685 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11515 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11521 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_annotation = let _endpos = _endpos__4_ in
                let _startpos = _startpos__1_ in
                let _loc = (_startpos, _endpos) in
                
# 801 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                               ( PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc (PEconstant (CONaddress _3))))) )
# 11532 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv686)) : 'freshtv688)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv689 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11542 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv690)) : 'freshtv692)
        | UINT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv699 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11553 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv695 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11565 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv693 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11573 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11579 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_annotation = let _endpos = _endpos__4_ in
                let _startpos = _startpos__1_ in
                let _loc = (_startpos, _endpos) in
                
# 802 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                ( PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc (PEconstant (CONaddress _3))))) )
# 11590 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv694)) : 'freshtv696)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv697 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 11600 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv698)) : 'freshtv700)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv701 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv702)) : 'freshtv704)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv705 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv706)

and _menhir_run20 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11622 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run22 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run21 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState20 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState20 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COMMA | RBRACKET | RPAREN ->
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState20
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState20

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ASSERT ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BEGIN ->
        _menhir_run159 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DENY ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EMIT ->
        _menhir_run156 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FAIL ->
        _menhir_run155 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FIRST ->
        _menhir_run147 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOLD ->
        _menhir_run135 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run125 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | GHOST ->
        _menhir_run121 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run118 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run115 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MATCH ->
        _menhir_run107 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState30 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState30 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState30

and _menhir_goto_type_expression : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_type_expression -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState230 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv591 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv589 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_), _endpos__4_), _endpos__5_, _, (_5 : 'tv_type_expression), _startpos__5_) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_type_expression = let _endpos = _endpos__5_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 271 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( mkfotyp ~loc:_sloc (PTarray (int_of_p_expression e.p_expression_desc, _5)) )
# 11722 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv590)) : 'freshtv592)
    | MenhirState224 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv595 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState232
        | ARROW | ASSIGN | COMMA | CONST | EOF | EQUAL | EVENT | EXTERNAL | INDEXED | LAYER | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv593 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_type_expression), _startpos__2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_type_expression = let _endpos = _endpos__2_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 275 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( mkfotyp ~loc:_sloc (PTlist _2) )
# 11746 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv594)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState232) : 'freshtv596)
    | MenhirState233 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv599 * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv597 * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_type_expression), _startpos__1_), _), _endpos__3_, _, (_3 : 'tv_type_expression), _startpos__3_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_type_expression = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 269 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( mkfotyp ~loc:_sloc (PTprod (_1, _3)) )
# 11768 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv598)) : 'freshtv600)
    | MenhirState223 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv605 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv603 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState235 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv601 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_type_expression), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_type_expression = 
# 267 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _2 )
# 11795 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv602)) : 'freshtv604)
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState235
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState235) : 'freshtv606)
    | MenhirState222 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv609 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv607 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState237 in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ARRAY ->
                _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState238 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState238 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState238 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState238) : 'freshtv608)
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState237
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState237) : 'freshtv610)
    | MenhirState238 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv613 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState239
        | ARROW | ASSIGN | COMMA | CONST | EOF | EQUAL | EVENT | EXTERNAL | INDEXED | LAYER | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv611 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, _, (_3 : 'tv_type_expression), _startpos__3_), _endpos__4_, _), _endpos__5_, _, (_5 : 'tv_type_expression), _startpos__5_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__5_ in
            let _v : 'tv_type_expression = let _endpos = _endpos__5_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 273 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( mkfotyp ~loc:_sloc (PTmapping (_3, _5)) )
# 11864 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv612)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState239) : 'freshtv614)
    | MenhirState220 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv617 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11876 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv615 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11888 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11893 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__2_), _), _endpos_te_, _, (te : 'tv_type_expression), _startpos_te_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_te_ in
            let _v : 'tv_type_declaration = let _endpos = _endpos_te_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 253 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      (   _2,  mkfotyp ~loc:_sloc te.p_type_FO_desc )
# 11905 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_type_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv616)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState240) : 'freshtv618)
    | MenhirState245 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv631 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11917 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState246
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv629 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11929 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 11934 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__1_), _endpos__3_, _, (_3 : 'tv_type_expression), _startpos__3_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_field_declaration = 
# 288 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                 ( _1, _3 )
# 11940 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv627) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_field_declaration) = _v in
            ((match _menhir_s with
            | MenhirState248 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv621 * _menhir_state * 'tv_field_declarations) * _menhir_state) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_field_declaration) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv619 * _menhir_state * 'tv_field_declarations) * _menhir_state) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : 'tv_field_declaration) : 'tv_field_declaration) = _v in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_field_declarations)), _) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_field_declarations = 
# 285 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                    ( _3 :: _1 )
# 11961 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_field_declarations _menhir_env _menhir_stack _menhir_s _v) : 'freshtv620)) : 'freshtv622)
            | MenhirState243 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv625) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_field_declaration) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv623) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_field_declaration) : 'tv_field_declaration) = _v in
                ((let _v : 'tv_field_declarations = 
# 284 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                       ( [_1] )
# 11976 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_field_declarations _menhir_env _menhir_stack _menhir_s _v) : 'freshtv624)) : 'freshtv626)
            | _ ->
                _menhir_fail ()) : 'freshtv628)) : 'freshtv630)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState246) : 'freshtv632)
    | MenhirState261 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv643 * Lexing.position) * _menhir_state * 'tv_idents)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv641 * Lexing.position) * _menhir_state * 'tv_idents)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState262 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv639 * Lexing.position) * _menhir_state * 'tv_idents)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__5_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _startpos__1_), _, (_2 : 'tv_idents)), _endpos__4_, _, (_4 : 'tv_type_expression), _startpos__4_) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _endpos = _endpos__5_ in
            let _v : 'tv_constructor_parameter = 
# 312 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( List.map (fun i -> i, _4) _2 )
# 12009 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv637) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_v : 'tv_constructor_parameter) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv635 * Lexing.position * 'tv_constructor_parameters) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_v : 'tv_constructor_parameter) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv633 * Lexing.position * 'tv_constructor_parameters) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let ((_2 : 'tv_constructor_parameter) : 'tv_constructor_parameter) = _v in
            ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_constructor_parameters)) = _menhir_stack in
            let _endpos = _endpos__2_ in
            let _v : 'tv_constructor_parameters = 
# 308 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                                  ( _2 :: _1 )
# 12028 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_constructor_parameters _menhir_env _menhir_stack _endpos _v) : 'freshtv634)) : 'freshtv636)) : 'freshtv638)) : 'freshtv640)) : 'freshtv642)
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState262
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState262) : 'freshtv644)
    | MenhirState280 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv647 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv645 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState281 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ARRAY ->
                _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState282 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState282 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState282 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState282 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState282 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState282 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState282) : 'freshtv646)
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState281
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState281) : 'freshtv648)
    | MenhirState282 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv651 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState283
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv649 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _endpos__3_, _, (_3 : 'tv_type_expression), _startpos__3_), _), _endpos__5_, _, (_5 : 'tv_type_expression), _startpos__5_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_object_signature_field = 
# 375 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      (  "constructor", _3, _5, MKconstructor  )
# 12091 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_field _menhir_env _menhir_stack _menhir_s _v) : 'freshtv650)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState283) : 'freshtv652)
    | MenhirState291 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv655 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12103 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv653 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12113 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState292 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ARRAY ->
                _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState293 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState293 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState293) : 'freshtv654)
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState292
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState292) : 'freshtv656)
    | MenhirState293 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv659 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12147 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState294
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv657 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12159 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s, (_1 : 'tv_method_kind)), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12164 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )), _startpos__2_), _endpos__4_, _, (_4 : 'tv_type_expression), _startpos__4_), _), _endpos__6_, _, (_6 : 'tv_type_expression), _startpos__6_) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : 'tv_object_signature_field = 
# 373 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( _2, _4, _6, _1 )
# 12171 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_field _menhir_env _menhir_stack _menhir_s _v) : 'freshtv658)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState294) : 'freshtv660)
    | MenhirState409 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv663 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12183 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv661 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12193 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState410 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ARRAY ->
                _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState411 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState411 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState411 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState411 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState411 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState411 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState411) : 'freshtv662)
        | ASSIGN ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState410
        | EQUAL ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState410
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState410
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState410) : 'freshtv664)
    | MenhirState411 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv665 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12231 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState412
        | EQUAL ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState412
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState412
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState412) : 'freshtv666)
    | MenhirState419 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv667 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12251 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run129 _menhir_env (Obj.magic _menhir_stack) MenhirState420
        | EQUAL ->
            _menhir_run128 _menhir_env (Obj.magic _menhir_stack) MenhirState420
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState420
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState420) : 'freshtv668)
    | MenhirState432 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv675 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12271 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INDEXED ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv671) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState433 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv669) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            ((let _1 = () in
            let _v : 'tv_indexed_opt = 
# 339 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
          ( true )
# 12288 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_indexed_opt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv670)) : 'freshtv672)
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState433
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv673) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState433 in
            ((let _v : 'tv_indexed_opt = 
# 338 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
  ( false )
# 12300 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_indexed_opt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv674)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState433) : 'freshtv676)
    | MenhirState480 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv679 * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState481
        | COMMA | EQUAL | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv677 * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _endpos__2_, _, (_2 : 'tv_type_expression), _startpos__2_) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_opt_type_annotation = 
# 729 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                           ( Some _2 )
# 12323 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
             in
            _menhir_goto_opt_type_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv678)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState481) : 'freshtv680)
    | MenhirState495 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv683 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12335 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv681 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12345 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState496 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BANG ->
                _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState497 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState497 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState497 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENTITY ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState497 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState497 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState497 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState497 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState497 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState497 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState497) : 'freshtv682)
        | STAR ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) MenhirState496
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState496) : 'freshtv684)
    | _ ->
        _menhir_fail ()

and _menhir_goto_opt_bar : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_opt_bar -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv583 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run193 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState111 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACKET ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState111 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv584)
    | MenhirState242 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv585 * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run255 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState254 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState254) : 'freshtv586)
    | MenhirState426 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv587) * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run428 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState427 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState427) : 'freshtv588)
    | _ ->
        _menhir_fail ()

and _menhir_run36 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12431 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv579 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12443 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37) : 'freshtv580)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv581 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12477 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv582)

and _menhir_goto_atom : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_atom -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv577 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv573 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState51 in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52) : 'freshtv574)
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState51 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState51 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | DOT | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | MINUS | MOD | OBJECT | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | SLASH | STAR | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv575 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_atom), _startpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_atom_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 12540 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_nonempty_list_atom_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv576)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState51) : 'freshtv578)

and _menhir_run273 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST ->
        _menhir_run284 _menhir_env (Obj.magic _menhir_stack) MenhirState273
    | CONSTRUCTOR ->
        _menhir_run279 _menhir_env (Obj.magic _menhir_stack) MenhirState273
    | GHOST ->
        _menhir_run277 _menhir_env (Obj.magic _menhir_stack) MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LOGICAL ->
        _menhir_run276 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState273 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv571 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState273 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv569 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_object_signature_expr = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 359 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                   ( mksig ~loc:_sloc (PSconstr []) )
# 12583 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv570)) : 'freshtv572)
    | REFINED ->
        _menhir_run274 _menhir_env (Obj.magic _menhir_stack) MenhirState273
    | IDENT _ ->
        _menhir_reduce166 _menhir_env (Obj.magic _menhir_stack) MenhirState273
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState273

and _menhir_run298 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12598 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv567) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12609 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12613 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_object_signature_expr = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 358 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
           ( mksig ~loc:_sloc (PSname _1) )
# 12624 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv568)

and _menhir_goto_opt_logical_or_trusted : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_opt_logical_or_trusted -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv565 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OBJECT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv561 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv547 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12651 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv531 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12663 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | CLONE ->
                    _menhir_run357 _menhir_env (Obj.magic _menhir_stack) MenhirState468 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run356 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState468 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run355 _menhir_env (Obj.magic _menhir_stack) MenhirState468 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState468) : 'freshtv532)
            | LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv541) = Obj.magic _menhir_stack in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv535) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_menhir_s : _menhir_state) = MenhirState455 in
                    let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12694 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                    )) = _v in
                    let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | COLON ->
                        _menhir_run458 _menhir_env (Obj.magic _menhir_stack)
                    | RPAREN ->
                        _menhir_reduce142 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv533 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12712 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv534)) : 'freshtv536)
                | LBRACE ->
                    _menhir_run326 _menhir_env (Obj.magic _menhir_stack) MenhirState455 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RPAREN ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv539 * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_menhir_s : _menhir_state) = MenhirState455 in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv537 * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos__2_ : Lexing.position) = _endpos in
                    let (_ : _menhir_state) = _menhir_s in
                    ((let (_menhir_stack, _startpos__1_) = _menhir_stack in
                    let _2 = () in
                    let _1 = () in
                    let _v : 'tv_base_layer_signature = let _endpos = _endpos__2_ in
                    let _symbolstartpos = _startpos__1_ in
                    let _sloc = (_symbolstartpos, _endpos) in
                    
# 662 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                   ( mklayersign ~loc:_sloc (PLSconstr []) )
# 12737 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                     in
                    _menhir_goto_base_layer_signature _menhir_env _menhir_stack _v) : 'freshtv538)) : 'freshtv540)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState455) : 'freshtv542)
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv543) = Obj.magic _menhir_stack in
                ((let (_, _endpos__0_) = Obj.magic _menhir_stack in
                let _v : 'tv_base_layer_signature = let _endpos = _endpos__0_ in
                let _symbolstartpos = _endpos in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 661 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( mklayersign ~loc:_sloc (PLSconstr []) )
# 12754 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_base_layer_signature _menhir_env _menhir_stack _v) : 'freshtv544)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv545 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12764 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv546)) : 'freshtv548)
        | SIGNATURE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv557 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv553 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12782 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | EQUAL ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv549 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12794 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | IDENT _v ->
                        _menhir_run298 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState452 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LBRACE ->
                        _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState452 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState452) : 'freshtv550)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv551 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12814 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    (raise _eRR : 'freshtv552)) : 'freshtv554)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv555 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv556)) : 'freshtv558)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv559 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv560)) : 'freshtv562)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv563 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv564)) : 'freshtv566)

and _menhir_run326 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run328 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState326 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv529 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState326 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv527 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos__2_ in
        let _v : 'tv_layer_signature = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 401 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                   ( mklayersign ~loc:_sloc (PLSconstr []) )
# 12864 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_layer_signature _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv528)) : 'freshtv530)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState326

and _menhir_run337 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 12875 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce142 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_annotations = 
# 788 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( [] )
# 12888 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_annotations _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv523 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | IDENT _v ->
            _menhir_run20 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv521 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState6 in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv517 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv515 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, _) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _endpos = _endpos__4_ in
                let _v : 'tv_annotations = 
# 789 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                         ( [] )
# 12940 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_annotations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv516)) : 'freshtv518)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv519 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv520)) : 'freshtv522)
        | STRING _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv524)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv525 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv526)

and _menhir_run221 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv511 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARRAY ->
            _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState222 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState222 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIST ->
            _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MAPPING ->
            _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState222 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState222) : 'freshtv512)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv513 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv514)

and _menhir_run223 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARRAY ->
        _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState223 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIST ->
        _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MAPPING ->
        _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState223 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState223

and _menhir_run224 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ARRAY ->
        _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState224 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIST ->
        _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MAPPING ->
        _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState224 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState224

and _menhir_run225 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv509) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_type_expression = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 265 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
             ( mkfotyp ~loc:_sloc (PTbuiltin Taddress) )
# 13067 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv510)

and _menhir_run226 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13074 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv507) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13085 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13089 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_type_expression = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 260 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( let x = try Hashtbl.find builtin_type_table _1
        with Not_found -> PTname _1
        in 
        mkfotyp ~loc:_sloc (x)  
       )
# 13104 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv508)

and _menhir_run227 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv503 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BANG ->
            _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState228 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENTITY ->
            _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState228 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState228 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState228) : 'freshtv504)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv505 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv506)

and _menhir_reduce208 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_opt_bar = 
# 833 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
         ( () )
# 13157 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_opt_bar _menhir_env _menhir_stack _menhir_s _v

and _menhir_run110 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv501) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_opt_bar = 
# 834 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
         ( () )
# 13171 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_opt_bar _menhir_env _menhir_stack _menhir_s _v) : 'freshtv502)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState507 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv131 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13183 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState503 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv133 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13192 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState501 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv135 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13201 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState499 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv137 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13210 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState497 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv139 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13219 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState496 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv141 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13228 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState495 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv143 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13237 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState494 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv145 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13246 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState491 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv147 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState489 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv149 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState485 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state * 'tv_method_parameters)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState481 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv153 * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState480 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState479 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13280 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState477 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState476 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState475 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv163 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState474 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState473 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv167 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13309 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState471 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv169 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13318 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv170)
    | MenhirState468 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv171 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13326 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv172)
    | MenhirState464 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv173 * _menhir_state * 'tv_base_slots)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState458 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13339 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState455 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv178)
    | MenhirState452 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv179 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13352 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv180)
    | MenhirState445 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv181) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13360 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv182)
    | MenhirState439 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * Lexing.position * _menhir_state * 'tv_event_declarations)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState433 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv185 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13373 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState432 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv187 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13382 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv188)
    | MenhirState427 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189) * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState426 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv192)
    | MenhirState420 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv193 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13399 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState419 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv195 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13408 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState412 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv197 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13417 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState411 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv199 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13426 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState410 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv201 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13435 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState409 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv203 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13444 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState407 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv205 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13453 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)
    | MenhirState402 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv207 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13462 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)
    | MenhirState401 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv209 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13471 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)
    | MenhirState399 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv212)
    | MenhirState396 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213 * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv214)
    | MenhirState393 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv215 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)
    | MenhirState391 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv217 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)
    | MenhirState387 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv219 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)
    | MenhirState385 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv221 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)
    | MenhirState376 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv223 * _menhir_state * 'tv_layer_slots_plus) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)
    | MenhirState375 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_layer_slots_plus) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)
    | MenhirState371 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv227 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 13518 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _, _menhir_s, _), _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)
    | MenhirState367 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv229 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 13527 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _, _menhir_s, _), _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)
    | MenhirState361 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv231 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)
    | MenhirState355 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv233 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)
    | MenhirState354 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv235 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13546 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)
    | MenhirState352 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)
    | MenhirState351 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv239 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)
    | MenhirState350 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv241 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13565 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)
    | MenhirState346 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv243 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)
    | MenhirState343 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv245 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)
    | MenhirState342 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)
    | MenhirState341 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv250)
    | MenhirState339 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13593 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv252)
    | MenhirState332 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv253 * _menhir_state * 'tv_layer_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)
    | MenhirState331 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv255 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)
    | MenhirState329 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13611 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)
    | MenhirState326 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)
    | MenhirState325 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv261 * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13625 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv262)
    | MenhirState317 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv263 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _, _menhir_s, _, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)
    | MenhirState312 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv265 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _, _menhir_s, _, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)
    | MenhirState304 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv267 * _menhir_state * 'tv_idents_semi_sep) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)
    | MenhirState302 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv269 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _, _menhir_s, _, _), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv270)
    | MenhirState294 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv271 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13653 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)
    | MenhirState293 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv273 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13662 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)
    | MenhirState292 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv275 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13671 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)
    | MenhirState291 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv277 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13680 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)
    | MenhirState287 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv279 * _menhir_state * 'tv_object_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)
    | MenhirState286 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv281 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)
    | MenhirState283 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv283 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)
    | MenhirState282 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv285 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)
    | MenhirState281 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv287 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)
    | MenhirState280 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv289 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)
    | MenhirState273 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv292)
    | MenhirState272 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv293 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13724 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv294)
    | MenhirState266 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv295 * Lexing.position * _menhir_state * 'tv_constructor_declarations)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)
    | MenhirState262 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv297 * Lexing.position) * _menhir_state * 'tv_idents)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv298)
    | MenhirState261 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv299 * Lexing.position) * _menhir_state * 'tv_idents)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)
    | MenhirState259 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv301 * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv302)
    | MenhirState256 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv303 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13751 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_constructor_params) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)
    | MenhirState254 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)
    | MenhirState248 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * _menhir_state * 'tv_field_declarations) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)
    | MenhirState247 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv309 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv310)
    | MenhirState246 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv311 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13775 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)
    | MenhirState245 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv313 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13784 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)
    | MenhirState243 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv315 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)
    | MenhirState242 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv317 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13798 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)
    | MenhirState240 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv319 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13807 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)
    | MenhirState239 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv321 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)
    | MenhirState238 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv323 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)
    | MenhirState237 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv325 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)
    | MenhirState235 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv327 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)
    | MenhirState233 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv329 * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)
    | MenhirState232 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv331 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)
    | MenhirState230 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv333 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv334)
    | MenhirState228 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv335 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)
    | MenhirState224 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv337 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv338)
    | MenhirState223 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv339 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)
    | MenhirState222 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv341 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv342)
    | MenhirState220 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv343 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13871 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv344)
    | MenhirState211 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv345 * _menhir_state * 'tv_annotations_plus)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv346)
    | MenhirState205 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv347 * _menhir_state * 'tv_match_clauses)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)
    | MenhirState201 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv349 * _menhir_state * 'tv_match_pattern)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)
    | MenhirState193 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13895 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv352)
    | MenhirState190 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv353 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13904 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv354)
    | MenhirState187 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv355 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv356)
    | MenhirState183 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv357 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13918 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv358)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv359 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13927 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv360)
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((('freshtv361 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13936 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv362)
    | MenhirState174 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv363 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13945 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv364)
    | MenhirState170 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv365 * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv366)
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)
    | MenhirState160 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv369 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv370)
    | MenhirState159 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv371 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv372)
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv373 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv374)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv375 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv377 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13984 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv378)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv379 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 13993 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)
    | MenhirState150 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv381 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14002 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv382)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv383 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14011 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)
    | MenhirState147 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv385 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv386)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((('freshtv387 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14025 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14029 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)
    | MenhirState144 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv389 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14038 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14042 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv390)
    | MenhirState143 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv391 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14051 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14055 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv392)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv393 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14064 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv394)
    | MenhirState138 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv395 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14073 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv396)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14082 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv398)
    | MenhirState135 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv399 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv400)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv401 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14096 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv403 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14105 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv404)
    | MenhirState130 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv405 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14114 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv406)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv407 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14123 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)
    | MenhirState125 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv409 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv410)
    | MenhirState124 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14137 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv412)
    | MenhirState121 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv413 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv414)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv415 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)
    | MenhirState118 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv417 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv418)
    | MenhirState117 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv419 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14161 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)
    | MenhirState114 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv421 * _menhir_state * Lexing.position) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv422)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv423 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv424)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv425 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv427 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv428)
    | MenhirState102 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv429 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv430)
    | MenhirState96 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv431 * _menhir_state * 'tv_struct_fields) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv432)
    | MenhirState95 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv433 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv434)
    | MenhirState89 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv435 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv436)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv437 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv438)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv439 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv440)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv442)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv443 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv444)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv445 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv446)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv447 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv448)
    | MenhirState74 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv449 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv450)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv451 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv452)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv453 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv454)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv455 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv456)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv457 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv458)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv459 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv460)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv461 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv462)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv463 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv464)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv465 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv466)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv467 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv468)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv469 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv470)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv471 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv472)
    | MenhirState51 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv473 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv474)
    | MenhirState49 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv475 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv476)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv477 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv478)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv480)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv481 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14320 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv482)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv483 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv484)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv485 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv486)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv487 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv488)
    | MenhirState30 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv489 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv490)
    | MenhirState20 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv491 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14349 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv492)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv493 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv494)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv495 * Lexing.position * _menhir_state * (
# 100 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14363 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv496)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv497 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv498)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv499 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14377 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv500)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14384 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv129) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14395 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14399 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_atom = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 494 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
          (  mkexp_ ~loc:_sloc (PEconstant (CONuint (int_of_string(_1)))) )
# 14410 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv130)

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState32 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState32 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState32 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState33 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv125 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_atom = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 497 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                   (  mkexp_ ~loc:_sloc (PEconstant CONunit) )
# 14486 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv126)) : 'freshtv128)
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState35 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run38 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14512 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14523 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14527 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_atom = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 493 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
         (  mkexp_ ~loc:_sloc (PEconstant (CONint (int_of_string(_1)))) )
# 14538 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv124)

and _menhir_run39 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv107 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14563 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv103 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14575 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv101 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14583 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14589 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__4_ in
                let _v : 'tv_atom = let _endpos = _endpos__4_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 495 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                               (  mkexp_ ~loc:_sloc (PEconstant (CONaddress _3)) )
# 14602 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv102)) : 'freshtv104)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv105 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14612 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)) : 'freshtv108)
        | UINT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv115 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14623 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv111 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14635 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv109 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14643 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14649 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__4_ in
                let _v : 'tv_atom = let _endpos = _endpos__4_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 496 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                                (  mkexp_ ~loc:_sloc (PEconstant (CONaddress _3)) )
# 14662 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                 in
                _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv110)) : 'freshtv112)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv113 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (string)
# 14672 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)) : 'freshtv116)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv117 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)) : 'freshtv120)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)

and _menhir_run45 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14694 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv99) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14705 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14709 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_atom = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 492 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
           ( let x = try Hashtbl.find constant_table _1 with Not_found -> PEglob _1 in  mkexp_ ~loc:_sloc (x) )
# 14720 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv100)

and _menhir_run46 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState46 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run47 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BANG ->
        _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENTITY ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47

and _menhir_goto_declarations : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_declarations -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv97 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv13) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14803 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv5) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14815 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | BANG ->
                    _menhir_run47 _menhir_env (Obj.magic _menhir_stack) MenhirState445 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | BITNOT ->
                    _menhir_run46 _menhir_env (Obj.magic _menhir_stack) MenhirState445 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run45 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState445 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENTITY ->
                    _menhir_run39 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState445 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | INT _v ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState445 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LBRACE ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState445 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState445 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MINUS ->
                    _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState445 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UINT _v ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState445 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState445) : 'freshtv6)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv7) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14849 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv8)) : 'freshtv10)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv12)) : 'freshtv14)
    | EOF ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv23 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv21 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_declarations)) = _menhir_stack in
        let _2 = () in
        let _v : (
# 207 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 14868 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        ) = 
# 213 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
                      ( List.rev _1 )
# 14872 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19) = _menhir_stack in
        let (_v : (
# 207 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 14879 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        let (_v : (
# 207 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 14886 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
        let ((_1 : (
# 207 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 14893 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) : (
# 207 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 14897 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv16)) : 'freshtv18)) : 'freshtv20)) : 'freshtv22)) : 'freshtv24)
    | EVENT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BAR ->
            _menhir_run110 _menhir_env (Obj.magic _menhir_stack) MenhirState426
        | IDENT _ ->
            _menhir_reduce208 _menhir_env (Obj.magic _menhir_stack) MenhirState426
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState426) : 'freshtv26)
    | EXTERNAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv49) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSERT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv35 * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState399 in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv31 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14939 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | COLON ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv27 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14951 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ARRAY ->
                        _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState419 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | IDENT _v ->
                        _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState419 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | IDENTITY ->
                        _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState419 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LIST ->
                        _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState419 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LPAREN ->
                        _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState419 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | MAPPING ->
                        _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState419 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState419) : 'freshtv28)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv29 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 14979 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv30)) : 'freshtv32)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv33 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv34)) : 'freshtv36)
        | LBRACKET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState399 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv41 * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState399 in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv37 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 15009 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LBRACKET ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | COLON ->
                    _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState407
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState407) : 'freshtv38)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv39 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv40)) : 'freshtv42)
        | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv47 * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState399 in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv43 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 15047 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LBRACKET ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState401 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | ASSIGN | EQUAL ->
                    _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState401
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState401) : 'freshtv44)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv45 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)) : 'freshtv48)
        | WITH ->
            _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState399
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState399) : 'freshtv50)
    | LAYER ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv65) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv51 * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 15090 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACKET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState339 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | COLON | EQUAL ->
                _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState339
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState339) : 'freshtv52)
        | SIGNATURE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv61 * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv57 * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 15120 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | EQUAL ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv53 * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 15132 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | IDENT _v ->
                        _menhir_run337 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState325 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LBRACE ->
                        _menhir_run326 _menhir_env (Obj.magic _menhir_stack) MenhirState325 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState325) : 'freshtv54)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv55 * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 15152 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    (raise _eRR : 'freshtv56)) : 'freshtv58)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv59 * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv60)) : 'freshtv62)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv63 * Lexing.position) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv64)) : 'freshtv66)
    | LOGICAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv69) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv67) = Obj.magic _menhir_stack in
        let (_endpos__1_ : Lexing.position) = _endpos in
        let (_startpos__1_ : Lexing.position) = _startpos in
        ((let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_opt_logical_or_trusted = 
# 825 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
             ( POlogical )
# 15183 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_opt_logical_or_trusted _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv68)) : 'freshtv70)
    | SIGNATURE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv79) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv75 * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 15201 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv71 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 15213 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run298 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState272 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LBRACE ->
                    _menhir_run273 _menhir_env (Obj.magic _menhir_stack) MenhirState272 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState272) : 'freshtv72)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv73 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 15233 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv74)) : 'freshtv76)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv77 * Lexing.position) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv78)) : 'freshtv80)
    | TRUSTED ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv83) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv81) = Obj.magic _menhir_stack in
        let (_endpos__1_ : Lexing.position) = _endpos in
        let (_startpos__1_ : Lexing.position) = _startpos in
        ((let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_opt_logical_or_trusted = 
# 826 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
             ( POtrusted )
# 15258 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_opt_logical_or_trusted _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv82)) : 'freshtv84)
    | TYPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv91) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv87 * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 15276 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv85 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
       (Astcommon.ident)
# 15288 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState4 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ARRAY ->
                    _menhir_run227 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run226 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState220 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENTITY ->
                    _menhir_run225 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIST ->
                    _menhir_run224 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run223 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MAPPING ->
                    _menhir_run221 _menhir_env (Obj.magic _menhir_stack) MenhirState220 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState220) : 'freshtv86)
            | LBRACKET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EQUAL ->
                _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState4
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4) : 'freshtv88)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv89 * Lexing.position) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv90)) : 'freshtv92)
    | OBJECT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv93) = Obj.magic _menhir_stack in
        ((let (_, _startpos) = Obj.magic _menhir_stack in
        let _endpos = _startpos in
        let _v : 'tv_opt_logical_or_trusted = 
# 824 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
             ( POnormal )
# 15333 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
         in
        _menhir_goto_opt_logical_or_trusted _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv94)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv96)) : 'freshtv98)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and file : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 207 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 15358 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv3) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = Obj.magic _menhir_stack in
    ((let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_declarations = 
# 217 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
      ( [] )
# 15381 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
     in
    _menhir_goto_declarations _menhir_env _menhir_stack _endpos _v) : 'freshtv2)) : 'freshtv4))

# 836 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.mly"
  

# 15388 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"

# 269 "<standard.mly>"
  

# 15393 "/Users/zachpage/Documents/deepsea-2/Edsger/parser.ml"
