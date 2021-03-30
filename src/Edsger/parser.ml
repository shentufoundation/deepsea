
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | XOR
    | WITH
    | UNEQUAL
    | UINT of (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
  )
    | TYPE
    | TRUSTED
    | TRANSFERETH
    | TO
    | THEN
    | STRING of (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 24 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 54 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
  )
    | INDEXED
    | IN
    | IF
    | IDENT of (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 62 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
    | ADDRESS
  
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
  | MenhirState515
  | MenhirState511
  | MenhirState509
  | MenhirState507
  | MenhirState505
  | MenhirState504
  | MenhirState503
  | MenhirState502
  | MenhirState499
  | MenhirState497
  | MenhirState493
  | MenhirState489
  | MenhirState488
  | MenhirState487
  | MenhirState485
  | MenhirState484
  | MenhirState483
  | MenhirState482
  | MenhirState481
  | MenhirState479
  | MenhirState476
  | MenhirState472
  | MenhirState466
  | MenhirState463
  | MenhirState460
  | MenhirState453
  | MenhirState447
  | MenhirState441
  | MenhirState440
  | MenhirState435
  | MenhirState434
  | MenhirState428
  | MenhirState427
  | MenhirState420
  | MenhirState419
  | MenhirState418
  | MenhirState417
  | MenhirState415
  | MenhirState410
  | MenhirState409
  | MenhirState407
  | MenhirState404
  | MenhirState401
  | MenhirState399
  | MenhirState395
  | MenhirState393
  | MenhirState384
  | MenhirState383
  | MenhirState379
  | MenhirState375
  | MenhirState369
  | MenhirState363
  | MenhirState362
  | MenhirState360
  | MenhirState359
  | MenhirState358
  | MenhirState354
  | MenhirState351
  | MenhirState350
  | MenhirState349
  | MenhirState347
  | MenhirState340
  | MenhirState339
  | MenhirState337
  | MenhirState334
  | MenhirState333
  | MenhirState325
  | MenhirState320
  | MenhirState312
  | MenhirState310
  | MenhirState302
  | MenhirState301
  | MenhirState300
  | MenhirState299
  | MenhirState295
  | MenhirState294
  | MenhirState291
  | MenhirState290
  | MenhirState289
  | MenhirState288
  | MenhirState281
  | MenhirState280
  | MenhirState274
  | MenhirState270
  | MenhirState269
  | MenhirState267
  | MenhirState264
  | MenhirState262
  | MenhirState256
  | MenhirState255
  | MenhirState254
  | MenhirState253
  | MenhirState251
  | MenhirState250
  | MenhirState248
  | MenhirState247
  | MenhirState246
  | MenhirState245
  | MenhirState243
  | MenhirState241
  | MenhirState240
  | MenhirState237
  | MenhirState235
  | MenhirState232
  | MenhirState231
  | MenhirState230
  | MenhirState228
  | MenhirState219
  | MenhirState207
  | MenhirState203
  | MenhirState195
  | MenhirState192
  | MenhirState189
  | MenhirState185
  | MenhirState180
  | MenhirState178
  | MenhirState176
  | MenhirState172
  | MenhirState164
  | MenhirState162
  | MenhirState161
  | MenhirState160
  | MenhirState158
  | MenhirState156
  | MenhirState154
  | MenhirState152
  | MenhirState151
  | MenhirState149
  | MenhirState148
  | MenhirState146
  | MenhirState145
  | MenhirState142
  | MenhirState140
  | MenhirState139
  | MenhirState137
  | MenhirState136
  | MenhirState134
  | MenhirState132
  | MenhirState129
  | MenhirState127
  | MenhirState126
  | MenhirState123
  | MenhirState122
  | MenhirState120
  | MenhirState119
  | MenhirState116
  | MenhirState113
  | MenhirState111
  | MenhirState109
  | MenhirState107
  | MenhirState98
  | MenhirState92
  | MenhirState91
  | MenhirState85
  | MenhirState83
  | MenhirState81
  | MenhirState79
  | MenhirState77
  | MenhirState75
  | MenhirState73
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
  | MenhirState50
  | MenhirState48
  | MenhirState47
  | MenhirState45
  | MenhirState37
  | MenhirState36
  | MenhirState33
  | MenhirState31
  | MenhirState29
  | MenhirState28
  | MenhirState27
  | MenhirState24
  | MenhirState14
  | MenhirState11
  | MenhirState8
  | MenhirState6
  | MenhirState4

# 2 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
  
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

# 388 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"

let rec _menhir_goto_match_clauses : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_match_clauses -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv2045 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) * _menhir_state * 'tv_match_clauses) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv2037 * _menhir_state * 'tv_match_clauses) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run195 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState207 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACKET ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState207 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState207) : 'freshtv2038)
    | END ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv2041 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) * _menhir_state * 'tv_match_clauses) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv2039 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) * _menhir_state * 'tv_match_clauses) = Obj.magic _menhir_stack in
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
        
# 553 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                       ( (PCmatch (mkexp_ ~loc:_sloc e.p_expression_desc, List.rev _5)) )
# 432 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv2040)) : 'freshtv2042)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv2043 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) * _menhir_state * 'tv_match_clauses) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv2044)) : 'freshtv2046)

and _menhir_goto_match_clause : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_match_clause -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState207 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv2031 * _menhir_state * 'tv_match_clauses)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_match_clause) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv2029 * _menhir_state * 'tv_match_clauses)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_match_clause) : 'tv_match_clause) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_match_clauses)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_match_clauses = 
# 610 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                   ( _3 :: _1 )
# 460 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_match_clauses _menhir_env _menhir_stack _menhir_s _v) : 'freshtv2030)) : 'freshtv2032)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv2035) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_match_clause) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv2033) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_match_clause) : 'tv_match_clause) = _v in
        ((let _v : 'tv_match_clauses = 
# 609 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                  ( [_1] )
# 475 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_match_clauses _menhir_env _menhir_stack _menhir_s _v) : 'freshtv2034)) : 'freshtv2036)
    | _ ->
        _menhir_fail ()

and _menhir_goto_proposition : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_proposition -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState395 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv2023 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_proposition) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv2021 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
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
        
# 770 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      (  mklayer ~loc:_sloc (PLrefine (_1, _3, _5)) )
# 506 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv2022)) : 'freshtv2024)
    | MenhirState404 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv2027 * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_proposition) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv2025 * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_proposition) : 'tv_proposition) = _v in
        ((let (_menhir_stack, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _endpos = _endpos__2_ in
        let _v : 'tv_layer_invariant_annotation = 
# 759 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                        ( Some _2 )
# 526 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_invariant_annotation _menhir_env _menhir_stack _endpos _v) : 'freshtv2026)) : 'freshtv2028)
    | _ ->
        _menhir_fail ()

and _menhir_goto_commands : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_commands -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1983 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | END ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1979 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1977 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_c_, _, (c : 'tv_commands)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_command_core = 
# 552 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                          ( c.p_command_desc )
# 558 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1978)) : 'freshtv1980)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1981 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1982)) : 'freshtv1984)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1987 * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1985 * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_command), _startpos__1_), _endpos__3_, _, (_3 : 'tv_commands)) = _menhir_stack in
        let _2 = () in
        let _endpos = _endpos__3_ in
        let _v : 'tv_commands = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 600 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                ( mkcmd ~loc:_sloc (PCsequence (_1, _3)) )
# 582 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_commands _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1986)) : 'freshtv1988)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1993 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 590 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1989 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 600 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState185 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState185 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState185) : 'freshtv1990)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1991 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 654 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1992)) : 'freshtv1994)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1999 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 663 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1995 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 673 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState192 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState192 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState192) : 'freshtv1996)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1997 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 733 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1998)) : 'freshtv2000)
    | MenhirState192 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv2003 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 742 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv2001 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 748 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 753 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__2_), _endpos__4_, _, (_4 : 'tv_commands)), _endpos__6_, _, (_6 : 'tv_commands)) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_command = let _endpos = _endpos__6_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 588 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                          ( mkcmd ~loc:_sloc (PClet (_2, _4, _6)) )
# 766 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv2002)) : 'freshtv2004)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv2007 * _menhir_state * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv2005 * _menhir_state * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_), _endpos__4_, _, (_4 : 'tv_commands)) = _menhir_stack in
        let _3 = () in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_match_clause = 
# 618 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( ("NIL", [], _4) )
# 781 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_match_clause _menhir_env _menhir_stack _menhir_s _v) : 'freshtv2006)) : 'freshtv2008)
    | MenhirState203 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv2011 * _menhir_state * 'tv_match_pattern)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv2009 * _menhir_state * 'tv_match_pattern)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_match_pattern)), _endpos__3_, _, (_3 : 'tv_commands)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_match_clause = 
# 614 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( let (cnstr, params) = _1 in
        (cnstr , params, _3) )
# 795 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_match_clause _menhir_env _menhir_stack _menhir_s _v) : 'freshtv2010)) : 'freshtv2012)
    | MenhirState499 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv2015 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv2013 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let ((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _), _, (_4 : 'tv_method_param)), _, (_5 : 'tv_opt_type_annotation)), _endpos__7_, _, (_7 : 'tv_commands)) = _menhir_stack in
        let _6 = () in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_object_field_or_method = let _endpos = _endpos__7_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 709 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( `Right ("constructor", { pMethodArguments = _4;
                      pMethodReturnType = _5;
                      pMethodKind = MKconstructor;
                      pMethodBody = _7;
                      pMethodAnnotations = _2;
                      pMethodLoc = (make_loc _sloc) }) )
# 818 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_object_field_or_method _menhir_env _menhir_stack _menhir_s _v) : 'freshtv2014)) : 'freshtv2016)
    | MenhirState509 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv2019 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 826 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv2017 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 832 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) * Lexing.position * _menhir_state * 'tv_commands) = Obj.magic _menhir_stack in
        ((let (((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _, (_3 : 'tv_method_kind)), _endpos__4_, (_4 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 837 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__4_), _, (_5 : 'tv_method_param)), _, (_6 : 'tv_opt_type_annotation)), _endpos__8_, _, (_8 : 'tv_commands)) = _menhir_stack in
        let _7 = () in
        let _1 = () in
        let _v : 'tv_object_field_or_method = let _endpos = _endpos__8_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 701 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( `Right (_4, { pMethodArguments = _5;
                      pMethodReturnType = _6;
                      pMethodKind = _3;
                      pMethodBody = _8;
                      pMethodAnnotations = _2;
                      pMethodLoc = (make_loc _sloc) }) )
# 852 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_object_field_or_method _menhir_env _menhir_stack _menhir_s _v) : 'freshtv2018)) : 'freshtv2020)
    | _ ->
        _menhir_fail ()

and _menhir_run201 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_idents -> Lexing.position -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 861 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1975 * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
    let (_endpos__2_ : Lexing.position) = _endpos in
    let ((_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 871 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 875 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__2_ : Lexing.position) = _startpos in
    ((let (_menhir_stack, _menhir_s, (_1 : 'tv_idents)) = _menhir_stack in
    let _v : 'tv_idents = 
# 317 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                  ( _2 :: _1 )
# 882 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_idents _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1976)

and _menhir_goto_object_fields_and_methods : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_object_fields_and_methods -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((((('freshtv1973 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 893 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LET ->
        _menhir_run482 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState511 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv1971 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 905 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState511 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv1969 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 914 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
        let (_endpos__9_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        ((let (((((((_menhir_stack, _endpos__1_, (_1 : 'tv_opt_logical_or_trusted), _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 921 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
        
# 639 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
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
# 949 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_object_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv1970)) : 'freshtv1972)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState511) : 'freshtv1974)

and _menhir_goto_idents_semi_sep : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_idents_semi_sep -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1967 * _menhir_state * 'tv_idents_semi_sep) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1965 * _menhir_state * 'tv_idents_semi_sep) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState312 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1961 * _menhir_state * 'tv_idents_semi_sep) * _menhir_state) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 980 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1959 * _menhir_state * 'tv_idents_semi_sep) * _menhir_state) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let ((_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 990 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 994 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos__3_ : Lexing.position) = _startpos in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_idents_semi_sep)), _) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_idents_semi_sep = 
# 392 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                     ( _3 :: _1 )
# 1002 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_idents_semi_sep _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1960)) : 'freshtv1962)
        | RBRACE ->
            _menhir_reduce217 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1963 * _menhir_state * 'tv_idents_semi_sep) * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1964)) : 'freshtv1966)
    | RBRACE ->
        _menhir_reduce216 _menhir_env (Obj.magic _menhir_stack) MenhirState312
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState312) : 'freshtv1968)

and _menhir_goto_layer_slots_plus : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_layer_slots_plus -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1957 * _menhir_state * 'tv_layer_slots_plus) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1955 * _menhir_state * 'tv_layer_slots_plus) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState383 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run361 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState384 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            _menhir_reduce217 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState384) : 'freshtv1956)
    | RBRACE ->
        _menhir_reduce216 _menhir_env (Obj.magic _menhir_stack) MenhirState383
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState383) : 'freshtv1958)

and _menhir_goto_layer_invariant_annotation : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_layer_invariant_annotation -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((('freshtv1953 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1058 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_layer_invariant_annotation) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((((('freshtv1951 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1066 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
    let (_endpos__7_ : Lexing.position) = _endpos in
    let ((_7 : 'tv_layer_invariant_annotation) : 'tv_layer_invariant_annotation) = _v in
    ((let (((((_menhir_stack, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1073 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )), _startpos__2_), _endpos__3_, _, (_3 : 'tv_annotations)), (_4 : 'tv_layer_signature_annotation)), _endpos__6_, _, (_6 : 'tv_layer_expression), _startpos__6_) = _menhir_stack in
    let _5 = () in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__7_ in
    let _v : 'tv_layer_declaration = let _endpos = _endpos__7_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 744 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _2, {  pLayerLoc = (make_loc _sloc); 
              pLayerAnnotations = _3;
              pLayerInvariant = _7;
              pLayerDesc = match _4 with
                | None -> _6
                | Some t -> mklayer ~loc:_sloc (PLrelax (_6, t));
             }
      )
# 1092 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1949) = _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_layer_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1947) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_layer_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1945) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let ((_1 : 'tv_layer_declaration) : 'tv_layer_declaration) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_declaration = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 229 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                       ( fst _1, mkdecl ~loc:_sloc (PDlayer (snd _1)) )
# 1116 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1946)) : 'freshtv1948)) : 'freshtv1950)) : 'freshtv1952)) : 'freshtv1954)

and _menhir_run396 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1123 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1943) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1134 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1138 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_proposition = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 822 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
            ( mkprop ~loc:_sloc (PPexternal _1) )
# 1148 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_proposition _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1944)

and _menhir_run397 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1155 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1941) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1166 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1170 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_proposition = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 821 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
           ( mkprop ~loc:_sloc (PPident _1) )
# 1180 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_proposition _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1942)

and _menhir_run393 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run390 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState393 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run360 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run359 _menhir_env (Obj.magic _menhir_stack) MenhirState393 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState393

and _menhir_run399 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run350 _menhir_env (Obj.magic _menhir_stack) MenhirState399 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState399

and _menhir_run401 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run390 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState401 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run360 _menhir_env (Obj.magic _menhir_stack) MenhirState401 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run359 _menhir_env (Obj.magic _menhir_stack) MenhirState401 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState401

and _menhir_reduce218 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_opt_type_annotation = 
# 730 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( None )
# 1233 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_opt_type_annotation _menhir_env _menhir_stack _menhir_s _v

and _menhir_run488 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState488 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ARRAY ->
        _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState488 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState488 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIST ->
        _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState488 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState488 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MAPPING ->
        _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState488 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState488

and _menhir_goto_layer_slots : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_layer_slots -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1939 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_slots) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1935 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_slots) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1933 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_slots) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_layer_slots)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_layer_expression = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 764 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      (  mklayer ~loc:_sloc (PLconstr (List.rev _2)) )
# 1287 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1934)) : 'freshtv1936)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1937 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_slots) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1938)) : 'freshtv1940)

and _menhir_run361 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1301 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1929 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1313 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1927) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState362 in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1923 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | INT _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv1909 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_v : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1343 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                    )) = _v in
                    let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | RPAREN ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (('freshtv1905 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1355 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        ((let _menhir_stack = (_menhir_stack, _endpos) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | COLONLESS ->
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((('freshtv1901 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1367 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                            ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                            ((let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            match _tok with
                            | CLONE ->
                                _menhir_run365 _menhir_env (Obj.magic _menhir_stack) MenhirState379 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | IDENT _v ->
                                _menhir_run364 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState379 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | LPAREN ->
                                _menhir_run363 _menhir_env (Obj.magic _menhir_stack) MenhirState379 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState379) : 'freshtv1902)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((('freshtv1903 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1389 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                            ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                            ((let ((((_menhir_stack, _, _menhir_s, _), _), _, _, _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1904)) : 'freshtv1906)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (('freshtv1907 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1400 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1908)) : 'freshtv1910)
                | UINT _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv1919 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_v : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1411 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                    )) = _v in
                    let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | RPAREN ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (('freshtv1915 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1423 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                        ((let _menhir_stack = (_menhir_stack, _endpos) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | COLONLESS ->
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((('freshtv1911 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1435 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                            ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                            ((let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            match _tok with
                            | CLONE ->
                                _menhir_run365 _menhir_env (Obj.magic _menhir_stack) MenhirState375 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | IDENT _v ->
                                _menhir_run364 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState375 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | LPAREN ->
                                _menhir_run363 _menhir_env (Obj.magic _menhir_stack) MenhirState375 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState375) : 'freshtv1912)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((('freshtv1913 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1457 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                            ) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                            ((let ((((_menhir_stack, _, _menhir_s, _), _), _, _, _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1914)) : 'freshtv1916)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (('freshtv1917 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 1468 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1918)) : 'freshtv1920)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ('freshtv1921 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1922)) : 'freshtv1924)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1925 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1926)) : 'freshtv1928)
        | CLONE ->
            _menhir_run365 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run364 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState362 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run363 _menhir_env (Obj.magic _menhir_stack) MenhirState362 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState362) : 'freshtv1930)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1931 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1503 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1932)

and _menhir_goto_command : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_command -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState509 | MenhirState499 | MenhirState203 | MenhirState116 | MenhirState119 | MenhirState192 | MenhirState126 | MenhirState172 | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1849 * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1843 * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState172 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState172 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState172) : 'freshtv1844)
        | BAR | COMMA | ELSE | END | IN | LET | RBRACE | RBRACKET | RPAREN | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1845 * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_c_, _menhir_s, (c : 'tv_command), _startpos_c_) = _menhir_stack in
            let _endpos = _endpos_c_ in
            let _v : 'tv_commands = let _endpos = _endpos_c_ in
            let _symbolstartpos = _startpos_c_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 599 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                     ( mkcmd ~loc:_sloc c.p_command_desc )
# 1583 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_commands _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1846)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1847 * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1848)) : 'freshtv1850)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv1859 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1598 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1851 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1608 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState180 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState180 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState180) : 'freshtv1852)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1853 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1666 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState176 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState176 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState176) : 'freshtv1854)
        | BAR | COMMA | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1855 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1724 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1729 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__3_), _, (_4 : 'tv_eq_or_assign)), _endpos_e1_, _, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_), _endpos__9_, _, (_9 : 'tv_command), _startpos__9_) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__9_ in
            let _v : 'tv_command_core = let _endpos = _endpos__9_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 566 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( PCfirst (_3, mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, None, None, _2) )
# 1742 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1856)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1857 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1752 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1858)) : 'freshtv1860)
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv1867 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1761 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv1861 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1771 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState178 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState178 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState178) : 'freshtv1862)
        | BAR | COMMA | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv1863 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1829 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1834 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
            
# 572 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( PCfirst (_3,mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, Some _11, None, _2) )
# 1848 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1864)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv1865 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1858 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1866)) : 'freshtv1868)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((((('freshtv1871 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1867 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((((('freshtv1869 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1873 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1878 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
        
# 575 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( PCfirst (_3, mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, Some _11, Some _13, _2) )
# 1893 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1870)) : 'freshtv1872)
    | MenhirState180 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv1875 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1901 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv1873 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1907 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1912 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
        
# 569 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( PCfirst (_3, mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, None, Some _11, _2) )
# 1926 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1874)) : 'freshtv1876)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((((('freshtv1879 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1934 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1938 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((((('freshtv1877 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1944 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1948 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((((((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1953 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__3_), _, (_4 : 'tv_eq_or_assign)), _endpos_e1_, _, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_), _endpos__9_, (_9 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1957 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
        
# 578 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( PCfold (_3, mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, mkexp_ ~loc:_sloc e3.p_expression_desc, _13, _2) )
# 1971 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1878)) : 'freshtv1880)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv1883 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1979 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv1881 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1985 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 1990 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__3_), _, (_4 : 'tv_eq_or_assign)), _endpos_e1_, _, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_), _endpos__9_, _, (_9 : 'tv_command), _startpos__9_) = _menhir_stack in
        let _8 = () in
        let _6 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__9_ in
        let _v : 'tv_command_core = let _endpos = _endpos__9_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 563 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( PCfor (_3, mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc, _9, _2) )
# 2003 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1882)) : 'freshtv1884)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1891 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1885 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState189 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState189 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState189 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState189 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState189) : 'freshtv1886)
        | BAR | COMMA | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1887 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_), _endpos__4_, _, (_4 : 'tv_command), _startpos__4_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_command_core = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 557 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                  ( PCcond (mkexp_ ~loc:_sloc e.p_expression_desc, _4, None) )
# 2080 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1888)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1889 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1890)) : 'freshtv1892)
    | MenhirState189 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1895 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1893 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_), _endpos__4_, _, (_4 : 'tv_command), _startpos__4_), _endpos__6_, _, (_6 : 'tv_command), _startpos__6_) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_command_core = let _endpos = _endpos__6_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 558 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                               ( PCcond (mkexp_ ~loc:_sloc e.p_expression_desc, _4, Some _6) )
# 2107 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1894)) : 'freshtv1896)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1899 * _menhir_state) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1897 * _menhir_state) * Lexing.position * _menhir_state * 'tv_command * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _endpos__2_, _, (_2 : 'tv_command), _startpos__2_) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_annotation = 
# 800 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                   ( PAexpr _2 )
# 2120 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1898)) : 'freshtv1900)
    | _ ->
        _menhir_fail ()

and _menhir_goto_annotated_command : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_annotated_command -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1829 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotated_command) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1827 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
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
        
# 584 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                              ( mkcmd ~loc:_sloc (PCassert _2) )
# 2150 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1828)) : 'freshtv1830)
    | MenhirState160 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1833 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotated_command) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1831 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
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
        
# 585 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                            ( mkcmd ~loc:_sloc (PCdeny _2) )
# 2174 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1832)) : 'freshtv1834)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1837 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 2182 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotated_command) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1835 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 2191 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) = Obj.magic _menhir_stack in
        let (_endpos__6_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_6 : 'tv_annotated_command) : 'tv_annotated_command) = _v in
        ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 2199 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__2_), _endpos__4_, _, (_4 : 'tv_commands)) = _menhir_stack in
        let _5 = () in
        let _3 = () in
        let _1 = () in
        let _endpos = _endpos__6_ in
        let _v : 'tv_annotated_command = let _endpos = _endpos__6_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 592 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                   ( mkcmd ~loc:_sloc (PClet (_2, _4, _6)) )
# 2211 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_annotated_command _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1836)) : 'freshtv1838)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1841 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotated_command) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1839 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
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
        
# 586 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                             ( mkcmd ~loc:_sloc (PCghost _2) )
# 2235 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1840)) : 'freshtv1842)
    | _ ->
        _menhir_fail ()

and _menhir_run219 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_annotations_plus -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run211 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState219 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EQUAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState219
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState219 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState219 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState219 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState219 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState219

and _menhir_goto_opt_semi : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_opt_semi -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1767 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1763 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1761 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_struct_fields)), _, (_3 : 'tv_opt_semi)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_expression = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 534 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                          ( mkexp_ ~loc:_sloc (PEstruct (List.rev _2)) )
# 2292 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1762)) : 'freshtv1764)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1765 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1766)) : 'freshtv1768)
    | MenhirState255 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1775 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1771 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1769 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_field_declarations)), _, (_3 : 'tv_opt_semi)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _endpos = _endpos__4_ in
            let _v : 'tv_type_definition = 
# 280 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                               ( PTsingleton (List.rev _2) )
# 2323 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_type_definition _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1770)) : 'freshtv1772)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1773 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1774)) : 'freshtv1776)
    | MenhirState294 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1783 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1779 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1777 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_object_signature_fields)), _, (_3 : 'tv_opt_semi)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_object_signature_expr = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 361 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                    ( mksig ~loc:_sloc (PSconstr (List.rev _2)) )
# 2358 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1778)) : 'freshtv1780)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1781 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1782)) : 'freshtv1784)
    | MenhirState312 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1813 * _menhir_state * 'tv_idents_semi_sep) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1811 * _menhir_state * 'tv_idents_semi_sep) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_idents_semi_sep)), _, (_2 : 'tv_opt_semi)) = _menhir_stack in
        let _v : 'tv_idents_semi = 
# 388 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                              ( List.rev _1 )
# 2377 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1809) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_idents_semi) = _v in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        match _menhir_s with
        | MenhirState310 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1791 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1787 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1785 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
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
                
# 365 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                                  ( mksig ~loc:_sloc (PSlogicize (_1, _5)) )
# 2412 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1786)) : 'freshtv1788)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1789 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1790)) : 'freshtv1792)
        | MenhirState320 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1799 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1795 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1793 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
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
                
# 363 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                                ( mksig ~loc:_sloc (PSghostize (_1, _5)) )
# 2449 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1794)) : 'freshtv1796)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (((('freshtv1797 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1798)) : 'freshtv1800)
        | MenhirState325 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1807 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
            ((assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1803 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1801 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
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
                
# 367 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                           ( mksig ~loc:_sloc (PSminus (_1, _4)) )
# 2485 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1802)) : 'freshtv1804)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1805 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * Lexing.position) * _menhir_state * 'tv_idents_semi) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1806)) : 'freshtv1808)
        | _ ->
            _menhir_fail ()) : 'freshtv1810)) : 'freshtv1812)) : 'freshtv1814)
    | MenhirState339 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1821 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1817 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1815 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_layer_signature_fields)), _, (_3 : 'tv_opt_semi)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _endpos = _endpos__4_ in
            let _v : 'tv_layer_signature = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 403 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                   ( mklayersign ~loc:_sloc (PLSconstr (List.rev _2)) )
# 2521 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_layer_signature _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1816)) : 'freshtv1818)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1819 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1820)) : 'freshtv1822)
    | MenhirState383 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1825 * _menhir_state * 'tv_layer_slots_plus) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1823 * _menhir_state * 'tv_layer_slots_plus) * _menhir_state * 'tv_opt_semi) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_layer_slots_plus)), _, (_2 : 'tv_opt_semi)) = _menhir_stack in
        let _v : 'tv_layer_slots = 
# 774 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                               ( _1 )
# 2540 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_slots _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1824)) : 'freshtv1826)
    | _ ->
        _menhir_fail ()

and _menhir_goto_event_declarations : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_event_declarations -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv1759) * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_event_declarations) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1747 * Lexing.position * _menhir_state * 'tv_event_declarations) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run436 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState447 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState447) : 'freshtv1748)
    | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1755) * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_event_declarations) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, (_2 : 'tv_opt_bar)), _endpos__3_, _, (_3 : 'tv_event_declarations)) = _menhir_stack in
        let _1 = () in
        let _endpos = _endpos__3_ in
        let _v : 'tv_events_declaration = 
# 321 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                     ( _3 )
# 2575 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1753) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_events_declaration) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1751 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_events_declaration) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1749 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let ((_2 : 'tv_events_declaration) : 'tv_events_declaration) = _v in
        ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_declarations)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_declarations = 
# 220 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                    ( _2 @ _1 )
# 2594 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_declarations _menhir_env _menhir_stack _endpos _v) : 'freshtv1750)) : 'freshtv1752)) : 'freshtv1754)) : 'freshtv1756)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1757) * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_event_declarations) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1758)) : 'freshtv1760)

and _menhir_goto_idents : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_idents -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState195 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1739 * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run201 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DOUBLEARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1735 * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_idents)) = _menhir_stack in
            let _v : 'tv_match_pattern_tail = 
# 631 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
           ( PPTother (List.rev _1) )
# 2624 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_match_pattern_tail _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1736)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1737 * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1738)) : 'freshtv1740)
    | MenhirState267 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1745 * Lexing.position) * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1741 * Lexing.position) * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState269 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ARRAY ->
                _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState269 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState269 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState269 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState269 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState269 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState269) : 'freshtv1742)
        | IDENT _v ->
            _menhir_run201 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1743 * Lexing.position) * _menhir_state * 'tv_idents) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1744)) : 'freshtv1746)
    | _ ->
        _menhir_fail ()

and _menhir_goto_object_field_or_method : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_object_field_or_method -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState511 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1729 * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_object_field_or_method) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1727 * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_object_field_or_method) : 'tv_object_field_or_method) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_object_fields_and_methods)) = _menhir_stack in
        let _v : 'tv_object_fields_and_methods = 
# 680 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( cons_either_double_list _2 _1 )
# 2690 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_object_fields_and_methods _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1728)) : 'freshtv1730)
    | MenhirState481 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1733) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_object_field_or_method) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1731) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_object_field_or_method) : 'tv_object_field_or_method) = _v in
        ((let _v : 'tv_object_fields_and_methods = 
# 678 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( either_to_double_list _1 )
# 2705 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_object_fields_and_methods _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1732)) : 'freshtv1734)
    | _ ->
        _menhir_fail ()

and _menhir_goto_separated_nonempty_list_COMMA_expression_ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_separated_nonempty_list_COMMA_expression_ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1721) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expression_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1719) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((x : 'tv_separated_nonempty_list_COMMA_expression_) : 'tv_separated_nonempty_list_COMMA_expression_) = _v in
        ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expression__ = 
# 144 "<standard.mly>"
    ( x )
# 2726 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1720)) : 'freshtv1722)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1725 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_separated_nonempty_list_COMMA_expression_) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1723 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((xs : 'tv_separated_nonempty_list_COMMA_expression_) : 'tv_separated_nonempty_list_COMMA_expression_) = _v in
        ((let (_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_expression), _startpos_x_) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_separated_nonempty_list_COMMA_expression_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 2743 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1724)) : 'freshtv1726)
    | _ ->
        _menhir_fail ()

and _menhir_goto_comma_sep_expressions : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_comma_sep_expressions -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1709 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1707 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_expression), _startpos__1_), _endpos__3_, _, (_3 : 'tv_comma_sep_expressions)) = _menhir_stack in
        let _2 = () in
        let _endpos = _endpos__3_ in
        let _v : 'tv_comma_sep_expressions = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 539 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                            (  mkexp_ ~loc:_sloc (PEpair (_1, _3)) )
# 2767 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_comma_sep_expressions _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1708)) : 'freshtv1710)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1717 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1713 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1711 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_comma_sep_expressions)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_atom = 
# 499 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                         ( _2 )
# 2792 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1712)) : 'freshtv1714)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1715 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_comma_sep_expressions) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1716)) : 'freshtv1718)
    | _ ->
        _menhir_fail ()

and _menhir_goto_struct_fields : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_struct_fields -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1705 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1703 * _menhir_state * 'tv_struct_fields) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState91 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run32 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState92 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            _menhir_reduce217 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState92) : 'freshtv1704)
    | RBRACE ->
        _menhir_reduce216 _menhir_env (Obj.magic _menhir_stack) MenhirState91
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState91) : 'freshtv1706)

and _menhir_run50 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState50 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50

and _menhir_run68 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState68 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run52 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState52 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run54 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState54 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run56 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState56 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run64 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState64 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run58 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState58 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run60 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState60 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run62 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState62 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run73 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState73 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState73 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState73

and _menhir_run75 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState75 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState75 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState75

and _menhir_run77 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState77 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState77 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState77

and _menhir_run79 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState79 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState79 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState79

and _menhir_run81 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState81 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81

and _menhir_run83 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState83 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState83 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState83

and _menhir_run85 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState85 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85

and _menhir_run66 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState66 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run70 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState70 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run311 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3344 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1701) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3355 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3359 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_idents_semi_sep = 
# 391 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
           ( [_1] )
# 3365 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_idents_semi_sep _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1702)

and _menhir_goto_object_declaration : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_object_declaration -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1699) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_object_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1697) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let ((_1 : 'tv_object_declaration) : 'tv_object_declaration) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_declaration = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 228 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                        ( fst _1, mkdecl ~loc:_sloc (PDobject (snd _1)) )
# 3388 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1698)) : 'freshtv1700)

and _menhir_goto_layer_obj_inst : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_layer_obj_inst -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1695 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3398 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position)) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_layer_obj_inst) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1693 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3406 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position)) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((_3 : 'tv_layer_obj_inst) : 'tv_layer_obj_inst) = _v in
    ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3413 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )), _startpos__1_) = _menhir_stack in
    let _2 = () in
    let _v : 'tv_layer_slot = 
# 781 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                ( _1, _3 )
# 3419 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1691) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_layer_slot) = _v in
    ((match _menhir_s with
    | MenhirState384 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1685 * _menhir_state * 'tv_layer_slots_plus) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_layer_slot) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1683 * _menhir_state * 'tv_layer_slots_plus) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_layer_slot) : 'tv_layer_slot) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_layer_slots_plus)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_layer_slots_plus = 
# 778 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                           ( _3 :: _1 )
# 3440 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_slots_plus _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1684)) : 'freshtv1686)
    | MenhirState360 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1689) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_layer_slot) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1687) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_layer_slot) : 'tv_layer_slot) = _v in
        ((let _v : 'tv_layer_slots_plus = 
# 777 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                ( [_1] )
# 3455 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_slots_plus _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1688)) : 'freshtv1690)
    | _ ->
        _menhir_fail ()) : 'freshtv1692)) : 'freshtv1694)) : 'freshtv1696)

and _menhir_run369 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run345 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState369 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState369 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState369

and _menhir_goto_layer_expression : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_layer_expression -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState359 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1661 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AT ->
            _menhir_run401 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run399 _menhir_env (Obj.magic _menhir_stack)
        | COLONGREATER ->
            _menhir_run393 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1657 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1655 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_layer_expression), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_layer_expression = 
# 771 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                    ( _2 )
# 3507 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1656)) : 'freshtv1658)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1659 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1660)) : 'freshtv1662)
    | MenhirState393 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1667 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AT ->
            _menhir_run401 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run399 _menhir_env (Obj.magic _menhir_stack)
        | COLONGREATER ->
            _menhir_run393 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1663 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run397 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState395 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run396 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState395 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState395) : 'freshtv1664)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1665 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1666)) : 'freshtv1668)
    | MenhirState401 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1673 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | AT ->
            _menhir_run401 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run399 _menhir_env (Obj.magic _menhir_stack)
        | COLONGREATER ->
            _menhir_run393 _menhir_env (Obj.magic _menhir_stack)
        | ASSERT | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | RPAREN | SIGNATURE | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1669 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_layer_expression), _startpos__1_), _endpos__3_, _, (_3 : 'tv_layer_expression), _startpos__3_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_layer_expression = let _endpos = _endpos__3_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 768 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      (  mklayer ~loc:_sloc (PLinst (_1, _3)) )
# 3575 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1670)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1671 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1672)) : 'freshtv1674)
    | MenhirState358 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1681 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3590 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSERT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1675) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run397 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState404 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | STRING _v ->
                _menhir_run396 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState404 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState404) : 'freshtv1676)
        | AT ->
            _menhir_run401 _menhir_env (Obj.magic _menhir_stack)
        | COLON ->
            _menhir_run399 _menhir_env (Obj.magic _menhir_stack)
        | COLONGREATER ->
            _menhir_run393 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1677) = Obj.magic _menhir_stack in
            ((let (_, _endpos) = Obj.magic _menhir_stack in
            let _v : 'tv_layer_invariant_annotation = 
# 758 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( None )
# 3624 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_layer_invariant_annotation _menhir_env _menhir_stack _endpos _v) : 'freshtv1678)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1679 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3634 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1680)) : 'freshtv1682)
    | _ ->
        _menhir_fail ()

and _menhir_run487 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3644 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        _menhir_run488 _menhir_env (Obj.magic _menhir_stack) MenhirState487
    | COMMA | RPAREN ->
        _menhir_reduce218 _menhir_env (Obj.magic _menhir_stack) MenhirState487
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState487

and _menhir_goto_method_param : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_method_param -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState484 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1651 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run488 _menhir_env (Obj.magic _menhir_stack) MenhirState497
        | EQUAL ->
            _menhir_reduce218 _menhir_env (Obj.magic _menhir_stack) MenhirState497
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState497) : 'freshtv1652)
    | MenhirState502 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1653 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3683 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            _menhir_run488 _menhir_env (Obj.magic _menhir_stack) MenhirState507
        | EQUAL ->
            _menhir_reduce218 _menhir_env (Obj.magic _menhir_stack) MenhirState507
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState507) : 'freshtv1654)
    | _ ->
        _menhir_fail ()

and _menhir_run359 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run390 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState359 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run360 _menhir_env (Obj.magic _menhir_stack) MenhirState359 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run359 _menhir_env (Obj.magic _menhir_stack) MenhirState359 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState359

and _menhir_run360 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run361 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState360 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1649) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState360 in
        ((let _v : 'tv_layer_slots = 
# 773 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( [] )
# 3731 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_slots _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1650)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState360

and _menhir_run390 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3742 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1647) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3753 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3757 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_layer_expression = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 762 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      (  mklayer ~loc:_sloc (PLname _1) )
# 3768 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1648)

and _menhir_goto_type_definition : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_type_definition -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv1645 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3778 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_type_definition) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv1643 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3787 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
    let (_endpos__5_ : Lexing.position) = _endpos in
    let (_ : _menhir_state) = _menhir_s in
    let ((_5 : 'tv_type_definition) : 'tv_type_definition) = _v in
    ((let (((_menhir_stack, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3795 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )), _startpos__2_), _endpos__3_, _, (_3 : 'tv_annotations)) = _menhir_stack in
    let _4 = () in
    let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__5_ in
    let _v : 'tv_type_declaration = let _endpos = _endpos__5_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 256 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _2, mkfotyp ~loc:_sloc (PTdata (_5, _3)) )
# 3807 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_type_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv1644)) : 'freshtv1646)

and _menhir_goto_annotation_arguments : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_annotation_arguments -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1637 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3819 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotation_arguments) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1635 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3827 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_annotation_arguments) : 'tv_annotation_arguments) = _v in
        ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3834 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__1_) = _menhir_stack in
        let _v : 'tv_annotation = 
# 805 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                ( PAclause (_1, _2) )
# 3839 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1636)) : 'freshtv1638)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1641 * Lexing.position * _menhir_state * (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 3847 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotation_arguments) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1639 * Lexing.position * _menhir_state * (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 3855 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_annotation_arguments) : 'tv_annotation_arguments) = _v in
        ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 3862 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__1_) = _menhir_stack in
        let _v : 'tv_annotation = 
# 806 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                 ( PAclause (_1, _2) )
# 3867 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1640)) : 'freshtv1642)
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_loption_separated_nonempty_list_COMMA_expression__ -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (('freshtv1633 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expression__) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1629 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expression__) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1627 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expression__) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _startpos__1_inlined1_), _, (xs : 'tv_loption_separated_nonempty_list_COMMA_expression__)) = _menhir_stack in
        let _3 = () in
        let _1_inlined1 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_command_core = let l =
          let _1 = _1_inlined1 in
          let x = 
# 232 "<standard.mly>"
    ( xs )
# 3900 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
           in
          
# 200 "<standard.mly>"
    ( x )
# 3905 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
          
        in
        
# 579 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                                               ( PCtransfer l )
# 3911 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1628)) : 'freshtv1630)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1631 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_loption_separated_nonempty_list_COMMA_expression__) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1632)) : 'freshtv1634)

and _menhir_goto_command_core : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_command_core -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    match _menhir_s with
    | MenhirState123 | MenhirState185 | MenhirState160 | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1621) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_command_core) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1619) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_command_core) : 'tv_command_core) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _endpos = _endpos_c_ in
        let _v : 'tv_annotated_command = let _endpos = _endpos_c_ in
        let _symbolstartpos = _startpos_c_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 591 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                    ( mkcmd ~loc:_sloc c)
# 3945 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_annotated_command _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1620)) : 'freshtv1622)
    | MenhirState509 | MenhirState499 | MenhirState24 | MenhirState203 | MenhirState116 | MenhirState119 | MenhirState192 | MenhirState189 | MenhirState122 | MenhirState126 | MenhirState136 | MenhirState148 | MenhirState180 | MenhirState178 | MenhirState176 | MenhirState156 | MenhirState172 | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1625) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_command_core) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1623) = Obj.magic _menhir_stack in
        let (_endpos_c_ : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((c : 'tv_command_core) : 'tv_command_core) = _v in
        let (_startpos_c_ : Lexing.position) = _startpos in
        ((let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_command = let _endpos = _endpos_c_ in
        let _symbolstartpos = _startpos_c_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 582 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                    ( mkcmd ~loc:_sloc c )
# 3969 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_command _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1624)) : 'freshtv1626)
    | _ ->
        _menhir_fail ()

and _menhir_run124 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1615 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 3988 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1611 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4000 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState126 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState126 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState126) : 'freshtv1612)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1613 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4060 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1614)) : 'freshtv1616)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1617 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1618)

and _menhir_goto_annotations_plus : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_annotations_plus -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1597 * _menhir_state * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            _menhir_run219 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1593 * _menhir_state * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1591 * _menhir_state * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_annotations_plus)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_annotation_arguments = 
# 817 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                    ( List.rev _2 )
# 4098 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1592)) : 'freshtv1594)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1595 * _menhir_state * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1596)) : 'freshtv1598)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1609 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COMMA ->
            _menhir_run219 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1605 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1601 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1599 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__5_ : Lexing.position) = _endpos in
                ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _, (_3 : 'tv_annotations_plus)), _endpos__4_) = _menhir_stack in
                let _5 = () in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _endpos = _endpos__5_ in
                let _v : 'tv_annotations = 
# 792 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                          ( List.rev _3 )
# 4141 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_annotations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1600)) : 'freshtv1602)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1603 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1604)) : 'freshtv1606)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1607 * _menhir_state * Lexing.position) * Lexing.position) * _menhir_state * 'tv_annotations_plus) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1608)) : 'freshtv1610)
    | _ ->
        _menhir_fail ()

and _menhir_goto_method_parameters : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_method_parameters -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1589 * _menhir_state * Lexing.position) * _menhir_state * 'tv_method_parameters) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1581 * _menhir_state * 'tv_method_parameters) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run487 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState493 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState493) : 'freshtv1582)
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1585 * _menhir_state * Lexing.position) * _menhir_state * 'tv_method_parameters) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1583 * _menhir_state * Lexing.position) * _menhir_state * 'tv_method_parameters) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _, (_2 : 'tv_method_parameters)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_method_param = 
# 720 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                     ( List.rev _2 )
# 4195 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_method_param _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1584)) : 'freshtv1586)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1587 * _menhir_state * Lexing.position) * _menhir_state * 'tv_method_parameters) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1588)) : 'freshtv1590)

and _menhir_goto_eq_or_assign : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_eq_or_assign -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1535 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4215 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState132 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState132 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState132) : 'freshtv1536)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1537 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4247 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState140 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState140 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState140) : 'freshtv1538)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv1539 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4279 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4283 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState146 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState146 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState146) : 'freshtv1540)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1541 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4315 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState152 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState152 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState152 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState152 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState152) : 'freshtv1542)
    | MenhirState410 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1555 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4347 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STRING _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1551 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4357 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4363 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | STRING _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv1545 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4375 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4379 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4385 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv1543 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4393 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4397 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__7_ : Lexing.position) = _endpos in
                let ((_7 : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4403 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4407 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) = _v in
                let (_startpos__7_ : Lexing.position) = _startpos in
                ((let ((((((_menhir_stack, _endpos__1_, _startpos__1_), _, _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4413 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )), _startpos__3_), _endpos__4_, _, (_4 : 'tv_annotations)), _, (_5 : 'tv_eq_or_assign)), _endpos__6_, (_6 : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4417 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )), _startpos__6_) = _menhir_stack in
                let _2 = () in
                let _1 = () in
                let _endpos = _endpos__7_ in
                let _v : 'tv_external_declaration = let _endpos = _endpos__7_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 241 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _3, mkdecl ~loc:_sloc (PDexternal_type (_6, Some _7, _4)) )
# 4428 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1544)) : 'freshtv1546)
            | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv1547 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4436 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4440 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((((((_menhir_stack, _endpos__1_, _startpos__1_), _, _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4445 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )), _startpos__3_), _endpos__4_, _, (_4 : 'tv_annotations)), _, (_5 : 'tv_eq_or_assign)), _endpos__6_, (_6 : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4449 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )), _startpos__6_) = _menhir_stack in
                let _2 = () in
                let _1 = () in
                let _endpos = _endpos__6_ in
                let _v : 'tv_external_declaration = let _endpos = _endpos__6_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 239 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _3, mkdecl ~loc:_sloc (PDexternal_type (_6, None, _4)) )
# 4460 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1548)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((('freshtv1549 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4470 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4474 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1550)) : 'freshtv1552)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1553 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4485 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1554)) : 'freshtv1556)
    | MenhirState420 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv1563 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4494 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STRING _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1559 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4504 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4510 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1557 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4518 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos__10_ : Lexing.position) = _endpos in
            let ((_10 : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4524 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4528 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos__10_ : Lexing.position) = _startpos in
            ((let ((((((((_menhir_stack, _endpos__1_, _startpos__1_), _endpos__2_, _, _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4534 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__3_), _endpos__4_, _, (_4 : 'tv_annotations)), _endpos__6_, _, (_6 : 'tv_type_expression), _startpos__6_), _), _endpos__8_, _, (_8 : 'tv_type_expression), _startpos__8_), _, (_9 : 'tv_eq_or_assign)) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _2 = () in
            let _1 = () in
            let _endpos = _endpos__10_ in
            let _v : 'tv_external_declaration = let _endpos = _endpos__10_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 246 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _3, mkdecl ~loc:_sloc (PDexternal_function (_10, _6, _8, _4)) )
# 4547 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1558)) : 'freshtv1560)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv1561 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4557 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1562)) : 'freshtv1564)
    | MenhirState418 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv1571 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4566 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STRING _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1567 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4576 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4582 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1565 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4590 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos__8_ : Lexing.position) = _endpos in
            let ((_8 : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4596 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4600 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos__8_ : Lexing.position) = _startpos in
            ((let ((((((_menhir_stack, _endpos__1_, _startpos__1_), _endpos__2_, _, _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4606 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__3_), _endpos__4_, _, (_4 : 'tv_annotations)), _endpos__6_, _, (_6 : 'tv_type_expression), _startpos__6_), _, (_7 : 'tv_eq_or_assign)) = _menhir_stack in
            let _5 = () in
            let _2 = () in
            let _1 = () in
            let _endpos = _endpos__8_ in
            let _v : 'tv_external_declaration = let _endpos = _endpos__8_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 243 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _3, mkdecl ~loc:_sloc (PDexternal_const (_8, _6, _4)) )
# 4618 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1566)) : 'freshtv1568)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1569 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4628 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1570)) : 'freshtv1572)
    | MenhirState428 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1579 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4637 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STRING _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1575 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4647 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4653 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1573 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4661 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            let (_endpos__7_ : Lexing.position) = _endpos in
            let ((_7 : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4667 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 4671 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos__7_ : Lexing.position) = _startpos in
            ((let (((((_menhir_stack, _endpos__1_, _startpos__1_), _, _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4677 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__3_), _endpos__5_, _, (_5 : 'tv_type_expression), _startpos__5_), _, (_6 : 'tv_eq_or_assign)) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _endpos = _endpos__7_ in
            let _v : 'tv_external_declaration = let _endpos = _endpos__7_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 248 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _3, mkdecl ~loc:_sloc (PDexternal_prop (_7, _5)) )
# 4689 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1574)) : 'freshtv1576)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1577 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4699 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1578)) : 'freshtv1580)
    | _ ->
        _menhir_fail ()

and _menhir_goto_object_signature_fields : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_object_signature_fields -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1533 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1531 * _menhir_state * 'tv_object_signature_fields) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState294 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONST ->
            _menhir_run292 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | CONSTRUCTOR ->
            _menhir_run287 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | GHOST ->
            _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState295 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LOGICAL ->
            _menhir_run284 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState295 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REFINED ->
            _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | RBRACE ->
            _menhir_reduce217 _menhir_env (Obj.magic _menhir_stack)
        | IDENT _ ->
            _menhir_reduce169 _menhir_env (Obj.magic _menhir_stack) MenhirState295
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState295) : 'freshtv1532)
    | RBRACE ->
        _menhir_reduce216 _menhir_env (Obj.magic _menhir_stack) MenhirState294
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState294) : 'freshtv1534)

and _menhir_reduce216 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_opt_semi = 
# 831 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
               ( () )
# 4752 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_opt_semi _menhir_env _menhir_stack _menhir_s _v

and _menhir_reduce217 : _menhir_env -> 'ttv_tail * _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _menhir_s) = _menhir_stack in
    let _1 = () in
    let _v : 'tv_opt_semi = 
# 832 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
               ( () )
# 4763 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_opt_semi _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_event_parameters : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_event_parameters -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1529 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4774 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * 'tv_event_parameters) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1513) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1509 * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4794 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1505 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4806 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState440 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | ARRAY ->
                    _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState440 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState440 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIST ->
                    _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState440 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState440 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MAPPING ->
                    _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState440 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState440) : 'freshtv1506)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1507 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4834 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv1508)) : 'freshtv1510)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1511 * Lexing.position) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv1512)) : 'freshtv1514)
    | BAR | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1525 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4848 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_event_parameters) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4853 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__1_), _endpos__2_, (_2 : 'tv_event_parameters)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_event_declaration = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 331 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( (_1, mkdecl ~loc:_sloc (PDevent (List.rev _2))) )
# 4862 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1523) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_event_declaration) = _v in
        ((match _menhir_s with
        | MenhirState447 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1517 * Lexing.position * _menhir_state * 'tv_event_declarations)) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_event_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1515 * Lexing.position * _menhir_state * 'tv_event_declarations)) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            let ((_3 : 'tv_event_declaration) : 'tv_event_declaration) = _v in
            ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_event_declarations)) = _menhir_stack in
            let _2 = () in
            let _endpos = _endpos__3_ in
            let _v : 'tv_event_declarations = 
# 326 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                              ( _3 :: _1 )
# 4887 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_event_declarations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1516)) : 'freshtv1518)
        | MenhirState435 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1521) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_event_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1519) = Obj.magic _menhir_stack in
            let (_endpos__1_ : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_event_declaration) : 'tv_event_declaration) = _v in
            ((let _endpos = _endpos__1_ in
            let _v : 'tv_event_declarations = 
# 325 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                       ( [_1] )
# 4905 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_event_declarations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1520)) : 'freshtv1522)
        | _ ->
            _menhir_fail ()) : 'freshtv1524)) : 'freshtv1526)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1527 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4917 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_event_parameters) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1528)) : 'freshtv1530)

and _menhir_run196 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4925 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1503) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4936 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4940 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_idents = 
# 316 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
           ( [_1] )
# 4946 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_idents _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1504)

and _menhir_goto_match_pattern_tail : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_match_pattern_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1501 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4956 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_match_pattern_tail) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1499 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4964 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position) = Obj.magic _menhir_stack in
    let (_ : _menhir_state) = _menhir_s in
    let ((_2 : 'tv_match_pattern_tail) : 'tv_match_pattern_tail) = _v in
    ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 4971 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )), _startpos__1_) = _menhir_stack in
    let _v : 'tv_match_pattern = 
# 623 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( match _2 with
        | PPTcons x -> ("CONS", [x; _1])
        | PPTother xs -> (_1, xs) )
# 4978 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1497) = _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_v : 'tv_match_pattern) = _v in
    ((let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1495 * _menhir_state * 'tv_match_pattern) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DOUBLEARROW ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1491 * _menhir_state * 'tv_match_pattern) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ASSERT ->
            _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BEGIN ->
            _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | DENY ->
            _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EMIT ->
            _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FAIL ->
            _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FIRST ->
            _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOLD ->
            _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | FOR ->
            _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | GHOST ->
            _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IF ->
            _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MATCH ->
            _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | TRANSFERETH ->
            _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState203 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState203 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState203) : 'freshtv1492)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1493 * _menhir_state * 'tv_match_pattern) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1494)) : 'freshtv1496)) : 'freshtv1498)) : 'freshtv1500)) : 'freshtv1502)

and _menhir_goto_expression : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_expression -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1241 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1237 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1235 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            ((let (((_menhir_stack, _endpos_a_, _menhir_s, (a : 'tv_atom), _startpos_a_), _, _startpos__2_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _startpos = _startpos_a_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_atom = let _endpos = _endpos__4_ in
            let _symbolstartpos = _startpos_a_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 500 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                           (  mkexp_ ~loc:_sloc (PEindex (a, e)) )
# 5105 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1236)) : 'freshtv1238)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1239 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1240)) : 'freshtv1242)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1247 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1243 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 517 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                     (  mkexp_ ~loc:_sloc (PEbin (OPxor, e1, e2 )) )
# 5162 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1244)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1245 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1246)) : 'freshtv1248)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1251 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1249 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : 'tv_expression = let _endpos = _endpos_e2_ in
        let _symbolstartpos = _startpos_e1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 523 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                      (  mkexp_ ~loc:_sloc (PEbin (OPtimes, e1, e2 )) )
# 5187 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1250)) : 'freshtv1252)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1255 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1253 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : 'tv_expression = let _endpos = _endpos_e2_ in
        let _symbolstartpos = _startpos_e1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 524 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                       (  mkexp_ ~loc:_sloc (PEbin (OPdivide, e1, e2 )) )
# 5205 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1254)) : 'freshtv1256)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1261 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1257 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 520 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                     (  mkexp_ ~loc:_sloc (PEbin (OPshr, e1, e2)) )
# 5237 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1258)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1259 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1260)) : 'freshtv1262)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1267 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | MINUS | OBJECT | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1263 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 521 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                      (  mkexp_ ~loc:_sloc (PEbin (OPplus, e1, e2 )) )
# 5272 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1264)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1265 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1266)) : 'freshtv1268)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1271 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1269 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_e1_ in
        let _endpos = _endpos_e2_ in
        let _v : 'tv_expression = let _endpos = _endpos_e2_ in
        let _symbolstartpos = _startpos_e1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 525 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                     (  mkexp_ ~loc:_sloc (PEbin (OPremainder, e1, e2 )) )
# 5297 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1270)) : 'freshtv1272)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1277 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | MINUS | OBJECT | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1273 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _startpos__2_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 522 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                       (  mkexp_ ~loc:_sloc (PEbin (OPminus, e1, e2 )) )
# 5325 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1274)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1275 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1276)) : 'freshtv1278)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1283 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1279 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 519 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                     (  mkexp_ ~loc:_sloc (PEbin (OPshl, e1, e2 )) )
# 5364 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1280)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1281 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1282)) : 'freshtv1284)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1289 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1285 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 518 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                        (  mkexp_ ~loc:_sloc (PEbin (OPbitand, e1, e2 )) )
# 5407 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1286)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1287 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1288)) : 'freshtv1290)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1295 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1291 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 529 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                         (  mkexp_ ~loc:_sloc (PEbin (OPne, e1, e2 )) )
# 5456 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1292)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1293 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1294)) : 'freshtv1296)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1301 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | BARBAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1297 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 516 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                        (  mkexp_ ~loc:_sloc (PEbin (OPbitor, e1, e2 )) )
# 5503 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1298)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1299 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1300)) : 'freshtv1302)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1307 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1303 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 531 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                        (  mkexp_ ~loc:_sloc (PEbin (OPle, e1, e2 )) )
# 5552 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1304)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1305 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1306)) : 'freshtv1308)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1313 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1309 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 530 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                      (  mkexp_ ~loc:_sloc (PEbin (OPlt, e1, e2 )) )
# 5601 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1310)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1311 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1312)) : 'freshtv1314)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1319 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1315 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 533 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                           (  mkexp_ ~loc:_sloc (PEbin (OPge, e1, e2 )) )
# 5650 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1316)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1317 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1318)) : 'freshtv1320)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1325 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1321 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 532 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                         (  mkexp_ ~loc:_sloc (PEbin (OPgt, e1, e2 )) )
# 5699 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1322)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1323 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1324)) : 'freshtv1326)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1331 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1327 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 528 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                       (  mkexp_ ~loc:_sloc (PEbin (OPeq, e1, e2 )) )
# 5748 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1328)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1329 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1330)) : 'freshtv1332)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1337 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONST | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1333 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 527 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                             (  mkexp_ ~loc:_sloc (PEbin (OPor,e1, e2 )) )
# 5813 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1334)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1335 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1336)) : 'freshtv1338)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1343 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | BAR | COMMA | CONST | DISJUNCTION | DO | ELSE | END | EOF | EVENT | EXTERNAL | IN | LAYER | LET | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | THEN | TO | TRUSTED | TYPE | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1339 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_expression = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 526 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                             (  mkexp_ ~loc:_sloc (PEbin (OPand, e1, e2 )) )
# 5876 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1340)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1341 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1342)) : 'freshtv1344)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1347 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1345 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_expression = let _endpos = _endpos_e_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 514 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                               (  mkexp_ ~loc:_sloc (PEun (OPnot, e)) )
# 5901 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1346)) : 'freshtv1348)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1351 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1349 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_expression = let _endpos = _endpos_e_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 515 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                 (  mkexp_ ~loc:_sloc (PEun (OPbitnot, e)) )
# 5919 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1350)) : 'freshtv1352)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1367 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 5927 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1363 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 5973 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 5978 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_struct_field = let _endpos = _endpos_e_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 545 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                              ( _1, mkexp_ ~loc:_sloc e.p_expression_desc )
# 5987 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1361) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_struct_field) = _v in
            ((match _menhir_s with
            | MenhirState92 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1355 * _menhir_state * 'tv_struct_fields) * _menhir_state) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_struct_field) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1353 * _menhir_state * 'tv_struct_fields) * _menhir_state) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : 'tv_struct_field) : 'tv_struct_field) = _v in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_struct_fields)), _) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_struct_fields = 
# 542 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                          ( _3 :: _1 )
# 6008 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_struct_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1354)) : 'freshtv1356)
            | MenhirState31 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1359) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_struct_field) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1357) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_struct_field) : 'tv_struct_field) = _v in
                ((let _v : 'tv_struct_fields = 
# 541 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                  ( [_1] )
# 6023 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_struct_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1358)) : 'freshtv1360)
            | _ ->
                _menhir_fail ()) : 'freshtv1362)) : 'freshtv1364)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1365 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6035 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1366)) : 'freshtv1368)
    | MenhirState98 | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1375 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1369 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState98 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState98 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState98) : 'freshtv1370)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1371 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _endpos = _endpos_e_ in
            let _v : 'tv_comma_sep_expressions = let _endpos = _endpos_e_ in
            let _symbolstartpos = _startpos_e_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 538 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                  ( mkexp_ ~loc:_sloc e.p_expression_desc )
# 6120 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_comma_sep_expressions _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1372)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1373 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1374)) : 'freshtv1376)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1379 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1377 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_expression = let _endpos = _endpos_e_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 513 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                (  mkexp_ ~loc:_sloc (PEun (OPneg, e)) )
# 6145 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1378)) : 'freshtv1380)
    | MenhirState107 | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1387 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1381 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState107 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState107 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState107) : 'freshtv1382)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1383 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_expression), _startpos_x_) = _menhir_stack in
            let _v : 'tv_separated_nonempty_list_COMMA_expression_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 6225 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expression_ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1384)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1385 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1386)) : 'freshtv1388)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1393 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1389 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BAR ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | IDENT _ | LBRACKET ->
                _menhir_reduce211 _menhir_env (Obj.magic _menhir_stack) MenhirState111
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState111) : 'freshtv1390)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1391 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1392)) : 'freshtv1394)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1399 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1395 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState122 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState122 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState122) : 'freshtv1396)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1397 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1398)) : 'freshtv1400)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1405 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6406 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1401 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6448 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState134 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState134 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState134) : 'freshtv1402)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1403 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6486 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1404)) : 'freshtv1406)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv1411 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6495 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1407 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6513 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState136 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState136 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState136) : 'freshtv1408)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1409 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6601 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1410)) : 'freshtv1412)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1417 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6610 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1413 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6652 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState142 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState142 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState142) : 'freshtv1414)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1415 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6690 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1416)) : 'freshtv1418)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv1427 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6699 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1423 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6709 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv1419 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6719 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6725 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ASSIGN ->
                    _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | EQUAL ->
                    _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState145
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState145) : 'freshtv1420)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((((((('freshtv1421 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6747 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1422)) : 'freshtv1424)
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1425 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6794 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1426)) : 'freshtv1428)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((((('freshtv1433 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6803 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6807 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv1429 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6825 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6829 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState148 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState148 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState148) : 'freshtv1430)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((((('freshtv1431 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6917 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6921 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1432)) : 'freshtv1434)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1439 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6930 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | TO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1435 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 6972 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState154 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState154 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState154) : 'freshtv1436)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1437 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7010 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1438)) : 'freshtv1440)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv1445 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7019 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | DO ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1441 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7037 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState156 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState156 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState156) : 'freshtv1442)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv1443 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7125 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1444)) : 'freshtv1446)
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1451 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | ELSE | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1447 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_e_ in
            let _v : 'tv_command_core = let _endpos = _endpos_e_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 560 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                      ( PCemit (mkexp_ ~loc:_sloc e.p_expression_desc) )
# 7184 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1448)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1449 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1450)) : 'freshtv1452)
    | MenhirState509 | MenhirState499 | MenhirState24 | MenhirState203 | MenhirState116 | MenhirState192 | MenhirState119 | MenhirState189 | MenhirState122 | MenhirState123 | MenhirState185 | MenhirState126 | MenhirState136 | MenhirState148 | MenhirState180 | MenhirState178 | MenhirState176 | MenhirState156 | MenhirState160 | MenhirState172 | MenhirState161 | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1459 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1453 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState164 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState164 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState164) : 'freshtv1454)
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | ELSE | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1455 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_e_, _menhir_s, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _startpos = _startpos_e_ in
            let _endpos = _endpos_e_ in
            let _v : 'tv_command_core = 
# 549 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                  ( PCyield e )
# 7273 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1456)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1457 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1458)) : 'freshtv1460)
    | MenhirState164 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1465 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | BAR | COMMA | ELSE | END | IN | LET | RBRACE | RBRACKET | RPAREN | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1461 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos_e1_, _menhir_s, (e1 : 'tv_expression), _startpos_e1_), _endpos_e2_, _, (e2 : 'tv_expression), _startpos_e2_) = _menhir_stack in
            let _2 = () in
            let _startpos = _startpos_e1_ in
            let _endpos = _endpos_e2_ in
            let _v : 'tv_command_core = let _endpos = _endpos_e2_ in
            let _symbolstartpos = _startpos_e1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 554 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                        ( (PCstore (mkexp_ ~loc:_sloc e1.p_expression_desc, mkexp_ ~loc:_sloc e2.p_expression_desc)) )
# 7338 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1462)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1463 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1464)) : 'freshtv1466)
    | MenhirState235 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1471 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1467 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ARRAY ->
                _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState237 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState237 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState237) : 'freshtv1468)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1469 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1470)) : 'freshtv1472)
    | MenhirState453 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1483) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7426 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1479) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7472 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7477 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__2_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _endpos = _endpos_e_ in
            let _v : 'tv_const_declaration = 
# 233 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( Hashtbl.add constant_table _2 e.p_expression_desc )
# 7485 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1477) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_v : 'tv_const_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1475 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_v : 'tv_const_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1473 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let ((_2 : 'tv_const_declaration) : 'tv_const_declaration) = _v in
            ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_declarations)) = _menhir_stack in
            let _endpos = _endpos__2_ in
            let _v : 'tv_declarations = 
# 221 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                    ( _1 )
# 7504 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_declarations _menhir_env _menhir_stack _endpos _v) : 'freshtv1474)) : 'freshtv1476)) : 'freshtv1478)) : 'freshtv1480)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv1481) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7514 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1482)) : 'freshtv1484)
    | MenhirState505 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv1489 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7523 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BARBAR ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | BITAND ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | CONJUNCTION ->
            _menhir_run85 _menhir_env (Obj.magic _menhir_stack)
        | DISJUNCTION ->
            _menhir_run83 _menhir_env (Obj.magic _menhir_stack)
        | EQUAL ->
            _menhir_run81 _menhir_env (Obj.magic _menhir_stack)
        | GREATER ->
            _menhir_run79 _menhir_env (Obj.magic _menhir_stack)
        | GREATEREQ ->
            _menhir_run77 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run75 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run73 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MOD ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | SHL ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | SHR ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | SLASH ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | STAR ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | UNEQUAL ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | XOR ->
            _menhir_run50 _menhir_env (Obj.magic _menhir_stack)
        | LET | RBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv1485 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7569 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)), _, (_3 : 'tv_method_kind)), _endpos__4_, (_4 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7574 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__4_), _), _endpos__6_, _, (_6 : 'tv_type_expression), _startpos__6_), _), _endpos_e_, _, (e : 'tv_expression), _startpos_e_) = _menhir_stack in
            let _7 = () in
            let _5 = () in
            let _1 = () in
            let _v : 'tv_object_field_or_method = let _endpos = _endpos_e_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 684 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
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
# 7599 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_object_field_or_method _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1486)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv1487 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7609 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1488)) : 'freshtv1490)
    | _ ->
        _menhir_fail ()

and _menhir_goto_method_kind : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_method_kind -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState281 | MenhirState295 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1225 * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1221 * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7633 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1217 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7645 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | ARRAY ->
                    _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState299 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIST ->
                    _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MAPPING ->
                    _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState299 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState299) : 'freshtv1218)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv1219 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7673 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1220)) : 'freshtv1222)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1223 * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1224)) : 'freshtv1226)
    | MenhirState483 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv1233 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1229 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7697 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ((('freshtv1227 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7709 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState502 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState503 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | ARRAY ->
                    _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState503 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState503 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIST ->
                    _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState503 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState503 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MAPPING ->
                    _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState503 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState503) : 'freshtv1228)
            | IDENT _v ->
                _menhir_run496 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState502 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run485 _menhir_env (Obj.magic _menhir_stack) MenhirState502 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState502) : 'freshtv1230)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1231 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1232)) : 'freshtv1234)
    | _ ->
        _menhir_fail ()

and _menhir_run482 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState482 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | CONST | CONSTRUCTOR | GHOST | IDENT _ | LOGICAL | REFINED ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState482
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState482

and _menhir_goto_base_slots : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_base_slots -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1215 * Lexing.position) * _menhir_state * 'tv_base_slots) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COMMA ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1207 * _menhir_state * 'tv_base_slots) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1205) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState472 in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7787 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COLON ->
                _menhir_run466 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv1203 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7803 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1204)) : 'freshtv1206)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState472) : 'freshtv1208)
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1211 * Lexing.position) * _menhir_state * 'tv_base_slots) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1209 * Lexing.position) * _menhir_state * 'tv_base_slots) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        ((let ((_menhir_stack, _startpos__1_), _, (_2 : 'tv_base_slots)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _v : 'tv_base_layer_signature = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 666 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                              ( mklayersign ~loc:_sloc  (PLSconstr (List.rev _2)) )
# 7828 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_base_layer_signature _menhir_env _menhir_stack _v) : 'freshtv1210)) : 'freshtv1212)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1213 * Lexing.position) * _menhir_state * 'tv_base_slots) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1214)) : 'freshtv1216)

and _menhir_goto_layer_signature_fields : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_layer_signature_fields -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1201 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1199 * _menhir_state * 'tv_layer_signature_fields) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState339 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run336 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState340 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            _menhir_reduce217 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState340) : 'freshtv1200)
    | RBRACE ->
        _menhir_reduce216 _menhir_env (Obj.magic _menhir_stack) MenhirState339
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState339) : 'freshtv1202)

and _menhir_goto_object_signature_declaration : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_object_signature_declaration -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1197) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_object_signature_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1195) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let ((_1 : 'tv_object_signature_declaration) : 'tv_object_signature_declaration) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_declaration = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 226 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                  ( fst _1, mkdecl ~loc:_sloc (PDsignature (snd _1)) )
# 7889 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv1196)) : 'freshtv1198)

and _menhir_run308 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | GHOST ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1181 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1175 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7913 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1173 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let ((_4 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7923 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7927 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
            
# 362 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                            ( mksig ~loc:_sloc (PSghostize (_1, [_4])) )
# 7941 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1174)) : 'freshtv1176)
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1177 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run311 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState320 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState320) : 'freshtv1178)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1179 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1180)) : 'freshtv1182)
    | LOGICAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1191 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _endpos, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1185 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7981 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1183 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__4_ : Lexing.position) = _endpos in
            let ((_4 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7991 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 7995 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
            
# 364 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                              ( mksig ~loc:_sloc (PSlogicize (_1, [_4])) )
# 8009 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1184)) : 'freshtv1186)
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1187 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run311 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState310 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState310) : 'freshtv1188)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv1189 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _, _), _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1190)) : 'freshtv1192)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1193 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1194)

and _menhir_run324 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _startpos ->
    let _menhir_stack = (_menhir_stack, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1167 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8054 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1165 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__3_ : Lexing.position) = _endpos in
        let ((_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8064 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8068 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) = _v in
        let (_startpos__3_ : Lexing.position) = _startpos in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_object_signature_expr), _startpos__1_), _startpos__2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_object_signature_expr = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 366 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                       ( mksig ~loc:_sloc (PSminus (_1, [_3])) )
# 8081 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1166)) : 'freshtv1168)
    | LBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1169 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run311 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState325 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState325) : 'freshtv1170)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1171 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1172)

and _menhir_goto_object_expression : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_object_expression -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState363 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1133 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run369 _menhir_env (Obj.magic _menhir_stack)
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1129 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1127 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_object_expression), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_object_expression = 
# 737 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                     ( _2 )
# 8134 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_object_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1128)) : 'freshtv1130)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1131 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1132)) : 'freshtv1134)
    | MenhirState375 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1139 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8149 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run369 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1135 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8161 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8166 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__3_), _endpos__4_), _endpos__6_, _, (_6 : 'tv_object_expression), _startpos__6_) = _menhir_stack in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_layer_obj_inst = let _endpos = _endpos__6_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 786 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                           ( mkobjinst ~loc:_sloc (POexternal ((CONaddress _3), _6)) )
# 8178 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_layer_obj_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1136)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1137 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8188 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1138)) : 'freshtv1140)
    | MenhirState379 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1145 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8197 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run369 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1141 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8209 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8214 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__3_), _endpos__4_), _endpos__6_, _, (_6 : 'tv_object_expression), _startpos__6_) = _menhir_stack in
            let _5 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_layer_obj_inst = let _endpos = _endpos__6_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 785 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                          ( mkobjinst ~loc:_sloc (POexternal ((CONaddress _3), _6)) )
# 8226 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_layer_obj_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1142)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1143 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8236 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1144)) : 'freshtv1146)
    | MenhirState362 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1151 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run369 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1147 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_object_expression), _startpos__1_) = _menhir_stack in
            let _v : 'tv_layer_obj_inst = let _endpos = _endpos__1_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 784 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                      ( mkobjinst ~loc:_sloc (POinternal _1) )
# 8258 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_layer_obj_inst _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1148)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1149 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1150)) : 'freshtv1152)
    | MenhirState476 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1157 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8273 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run369 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1153 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8285 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _endpos__1_, (_1 : 'tv_opt_logical_or_trusted), _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8290 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
            
# 659 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _3, { pObjectType = None; pObjectDesc = _5; pObjectLoc = (make_loc _sloc) } )
# 8305 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_object_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv1154)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1155 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8315 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1156)) : 'freshtv1158)
    | MenhirState515 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv1163 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8324 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLONGREATER ->
            _menhir_run369 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv1159 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8336 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _endpos__1_, (_1 : 'tv_opt_logical_or_trusted), _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8341 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
            
# 656 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _3, { pObjectType = Some { pObjectBase = _4; pObjectSignature = _6; pObjectTypLoc = (make_loc _sloc) };
              pObjectDesc = _8; pObjectLoc = (make_loc _sloc) } )
# 8358 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_object_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv1160)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv1161 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8368 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1162)) : 'freshtv1164)
    | _ ->
        _menhir_fail ()

and _menhir_goto_layer_type : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_layer_type -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    match _menhir_s with
    | MenhirState349 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1121) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_layer_type) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1119) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_layer_type) : 'tv_layer_type) = _v in
        ((let _1 = () in
        let _v : 'tv_layer_signature_annotation = 
# 755 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                      ( Some _2 )
# 8393 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_signature_annotation _menhir_env _menhir_stack _v) : 'freshtv1120)) : 'freshtv1122)
    | MenhirState399 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1125 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_layer_type) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1123 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
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
        
# 766 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      (  mklayer ~loc:_sloc (PLrelax (_1, _3)) )
# 8417 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1124)) : 'freshtv1126)
    | _ ->
        _menhir_fail ()

and _menhir_goto_declaration : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_declaration -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1117 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_declaration) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1115 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
    let (_endpos__2_ : Lexing.position) = _endpos in
    let ((_2 : 'tv_declaration) : 'tv_declaration) = _v in
    ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_declarations)) = _menhir_stack in
    let _endpos = _endpos__2_ in
    let _v : 'tv_declarations = 
# 219 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                              ( _2 :: _1 )
# 8438 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_declarations _menhir_env _menhir_stack _endpos _v) : 'freshtv1116)) : 'freshtv1118)

and _menhir_run485 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run487 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState485 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1113 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState485 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1111 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_method_param = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 719 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                   ( ["()", Some (mkfotyp ~loc:_sloc (PTbuiltin Tunit))] )
# 8469 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_method_param _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1112)) : 'freshtv1114)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState485

and _menhir_run496 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8480 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1109) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8491 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8495 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_method_param = 
# 718 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
           ( [_1, None] )
# 8501 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_method_param _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1110)

and _menhir_goto_external_declaration : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_external_declaration -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1107 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_external_declaration) = _v in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1105 * Lexing.position * 'tv_declarations) = Obj.magic _menhir_stack in
    let (_endpos__2_ : Lexing.position) = _endpos in
    let ((_2 : 'tv_external_declaration) : 'tv_external_declaration) = _v in
    ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_declarations)) = _menhir_stack in
    let _endpos = _endpos__2_ in
    let _v : 'tv_declarations = 
# 222 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                       ( _2 :: _1 )
# 8520 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_declarations _menhir_env _menhir_stack _endpos _v) : 'freshtv1106)) : 'freshtv1108)

and _menhir_goto_layer_signature_annotation : _menhir_env -> 'ttv_tail -> 'tv_layer_signature_annotation -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv1103 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8531 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1099 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8541 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run390 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState358 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run360 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run359 _menhir_env (Obj.magic _menhir_stack) MenhirState358 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState358) : 'freshtv1100)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv1101 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8563 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1102)) : 'freshtv1104)

and _menhir_run350 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run345 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState350 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState350 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1097 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState350 in
        ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run345 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState351 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState351 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState351) : 'freshtv1098)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState350

and _menhir_goto_constructor_declarations : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_constructor_declarations -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv1095 * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_constructor_declarations) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | BAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1089 * Lexing.position * _menhir_state * 'tv_constructor_declarations) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run263 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState274 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState274) : 'freshtv1090)
    | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1091 * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_constructor_declarations) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_opt_bar)), _endpos__2_, _, (_2 : 'tv_constructor_declarations)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_type_definition = 
# 281 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                               ( PTbranches (List.rev _2) )
# 8628 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_type_definition _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv1092)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1093 * _menhir_state * 'tv_opt_bar) * Lexing.position * _menhir_state * 'tv_constructor_declarations) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1094)) : 'freshtv1096)

and _menhir_run252 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8642 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1085 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8654 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState253 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ARRAY ->
            _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState253 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState253 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIST ->
            _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState253 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState253 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MAPPING ->
            _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState253 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState253) : 'freshtv1086)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1087 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8682 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1088)

and _menhir_reduce10 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_annotation_arguments = 
# 809 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( [] )
# 8692 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8699 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1083) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8710 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8714 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation_arguments = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    
# 811 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
          ( [PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc  (PEconstant (CONuint (int_of_string(_1)))))))] )
# 8723 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1084)

and _menhir_run10 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8730 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1081) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8741 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8745 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation_arguments = 
# 815 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
            ( [PAclause (_1, [])] )
# 8751 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1082)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run211 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EQUAL ->
        _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState11
    | IDENT _v ->
        _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1079 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState11 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1077 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_annotation_arguments = 
# 816 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                   ( [] )
# 8785 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1078)) : 'freshtv1080)
    | STRING _v ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState11 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11

and _menhir_run15 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8800 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1075) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8811 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8815 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation_arguments = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    
# 810 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
         ( [PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc (PEconstant (CONint (int_of_string(_1)))))))] )
# 8824 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1076)

and _menhir_run16 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8831 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1073) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8842 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 8846 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation_arguments = 
# 814 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
           ( [PAclause (_1, [])] )
# 8852 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1074)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1069 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1057 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8877 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1053 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8889 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1051 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8897 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8903 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_annotation_arguments = let _endpos = _endpos__4_ in
                let _startpos = _startpos__1_ in
                let _loc = (_startpos, _endpos) in
                
# 812 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                              ( [PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc (PEconstant (CONaddress _3)))))] )
# 8914 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1052)) : 'freshtv1054)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1055 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8924 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1056)) : 'freshtv1058)
        | UINT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1065 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8935 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1061 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8947 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1059 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8955 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8961 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_annotation_arguments = let _endpos = _endpos__4_ in
                let _startpos = _startpos__1_ in
                let _loc = (_startpos, _endpos) in
                
# 813 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                               ( [PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc (PEconstant (CONaddress _3)))))] )
# 8972 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_annotation_arguments _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1060)) : 'freshtv1062)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv1063 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 8982 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1064)) : 'freshtv1066)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1067 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1068)) : 'freshtv1070)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1071 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1072)

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1047 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState27 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState27 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1045) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState27 in
            ((let _v : 'tv_loption_separated_nonempty_list_COMMA_expression__ = 
# 142 "<standard.mly>"
    ( [] )
# 9040 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_loption_separated_nonempty_list_COMMA_expression__ _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1046)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState27) : 'freshtv1048)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1049 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1050)

and _menhir_run109 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState109 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState109 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState109

and _menhir_run117 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1041 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9097 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1037 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9109 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState119 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState119 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState119) : 'freshtv1038)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1039 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9169 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1040)) : 'freshtv1042)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1043 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1044)

and _menhir_run120 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState120 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState120 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState120

and _menhir_run123 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EMIT ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FAIL ->
        _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FIRST ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOLD ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MATCH ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRANSFERETH ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState123 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState123 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState123

and _menhir_run127 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState127 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _ ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState127
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState127

and _menhir_run137 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState137 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _ ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState137
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState137

and _menhir_run149 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState149 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _ ->
        _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState149
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState149

and _menhir_run157 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1035) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_command_core = 
# 555 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
          ( PCfail )
# 9318 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_command_core _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv1036)

and _menhir_run158 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState158 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState158 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState158

and _menhir_run160 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EMIT ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FAIL ->
        _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FIRST ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOLD ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MATCH ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRANSFERETH ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState160 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState160 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState160

and _menhir_run161 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ASSERT ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DENY ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EMIT ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FAIL ->
        _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FIRST ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOLD ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | GHOST ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MATCH ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRANSFERETH ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState161 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState161 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState161

and _menhir_run162 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EMIT ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FAIL ->
        _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FIRST ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOLD ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run124 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MATCH ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRANSFERETH ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState162 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState162 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState162

and _menhir_goto_annotation : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_annotation -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState219 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1029 * _menhir_state * 'tv_annotations_plus)) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotation) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1027 * _menhir_state * 'tv_annotations_plus)) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_annotation) : 'tv_annotation) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_annotations_plus)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_annotations_plus = 
# 796 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                       ( _3 :: _1 )
# 9521 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_annotations_plus _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1028)) : 'freshtv1030)
    | MenhirState6 | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1033) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_annotation) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1031) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_annotation) : 'tv_annotation) = _v in
        ((let _v : 'tv_annotations_plus = 
# 795 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                ( [_1] )
# 9536 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_annotations_plus _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1032)) : 'freshtv1034)
    | _ ->
        _menhir_fail ()

and _menhir_goto_opt_type_annotation : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_opt_type_annotation -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState487 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1013 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9551 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv1011 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9557 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9562 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__1_), _, (_2 : 'tv_opt_type_annotation)) = _menhir_stack in
        let _v : 'tv_method_parameter = 
# 727 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                               ( _1, _2 )
# 9567 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv1009) = _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_method_parameter) = _v in
        ((match _menhir_s with
        | MenhirState493 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1003 * _menhir_state * 'tv_method_parameters)) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_method_parameter) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv1001 * _menhir_state * 'tv_method_parameters)) = Obj.magic _menhir_stack in
            let (_ : _menhir_state) = _menhir_s in
            let ((_3 : 'tv_method_parameter) : 'tv_method_parameter) = _v in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_method_parameters)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_method_parameters = 
# 724 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                              ( _3 :: _1 )
# 9588 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_method_parameters _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1002)) : 'freshtv1004)
        | MenhirState485 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1007) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_method_parameter) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv1005) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_method_parameter) : 'tv_method_parameter) = _v in
            ((let _v : 'tv_method_parameters = 
# 723 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                     ( [_1] )
# 9603 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_method_parameters _menhir_env _menhir_stack _menhir_s _v) : 'freshtv1006)) : 'freshtv1008)
        | _ ->
            _menhir_fail ()) : 'freshtv1010)) : 'freshtv1012)) : 'freshtv1014)
    | MenhirState497 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv1019 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1015 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState499 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState499 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState499 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState499 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState499) : 'freshtv1016)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv1017 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1018)) : 'freshtv1020)
    | MenhirState507 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv1025 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9680 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1021 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9690 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState509 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState509 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState509 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState509 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState509) : 'freshtv1022)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv1023 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9750 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv1024)) : 'freshtv1026)
    | _ ->
        _menhir_fail ()

and _menhir_goto_indexed_opt : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_indexed_opt -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : (((('freshtv999 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9764 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_indexed_opt) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv995 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9774 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_indexed_opt) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv993 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9782 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_indexed_opt) = Obj.magic _menhir_stack in
        let (_endpos__6_ : Lexing.position) = _endpos in
        ((let ((((_menhir_stack, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9788 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__2_), _endpos__4_, _, (_4 : 'tv_type_expression), _startpos__4_), _, (_5 : 'tv_indexed_opt)) = _menhir_stack in
        let _6 = () in
        let _3 = () in
        let _1 = () in
        let _endpos = _endpos__6_ in
        let _v : 'tv_event_parameter = 
# 345 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( (_2, _4, _5) )
# 9797 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv991) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_event_parameter) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv989 * Lexing.position * 'tv_event_parameters) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_event_parameter) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv987 * Lexing.position * 'tv_event_parameters) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let ((_2 : 'tv_event_parameter) : 'tv_event_parameter) = _v in
        ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_event_parameters)) = _menhir_stack in
        let _endpos = _endpos__2_ in
        let _v : 'tv_event_parameters = 
# 335 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                      ( _2 :: _1 )
# 9816 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_event_parameters _menhir_env _menhir_stack _endpos _v) : 'freshtv988)) : 'freshtv990)) : 'freshtv992)) : 'freshtv994)) : 'freshtv996)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv997 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9826 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state * 'tv_indexed_opt) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv998)) : 'freshtv1000)

and _menhir_run130 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv985) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_eq_or_assign = 
# 595 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
          ( () )
# 9841 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_eq_or_assign _menhir_env _menhir_stack _menhir_s _v) : 'freshtv986)

and _menhir_run131 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv983) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_eq_or_assign = 
# 596 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
           ( () )
# 9855 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_eq_or_assign _menhir_env _menhir_stack _menhir_s _v) : 'freshtv984)

and _menhir_goto_object_signature_field : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_object_signature_field -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState295 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv977 * _menhir_state * 'tv_object_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_object_signature_field) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv975 * _menhir_state * 'tv_object_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_3 : 'tv_object_signature_field) : 'tv_object_signature_field) = _v in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_object_signature_fields)), _) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_object_signature_fields = 
# 370 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                              ( _3 :: _1 )
# 9876 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_object_signature_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv976)) : 'freshtv978)
    | MenhirState281 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv981) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_object_signature_field) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv979) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_object_signature_field) : 'tv_object_signature_field) = _v in
        ((let _v : 'tv_object_signature_fields = 
# 369 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                            ( [_1] )
# 9891 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_object_signature_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv980)) : 'freshtv982)
    | _ ->
        _menhir_fail ()

and _menhir_goto_constructor_parameters : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_constructor_parameters -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv973 * Lexing.position * 'tv_constructor_parameters) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv963) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run196 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState267 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState267) : 'freshtv964)
    | BAR | CONST | EOF | EVENT | EXTERNAL | LAYER | LBRACKET | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv969 * Lexing.position * 'tv_constructor_parameters) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_constructor_parameters)) = _menhir_stack in
        let _endpos = _endpos__1_ in
        let _v : 'tv_constructor_params = 
# 305 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( List.fold_left (fun a b -> List.rev_append b a) [] _1 )
# 9927 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv967) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_constructor_params) = _v in
        ((let _menhir_stack = (_menhir_stack, _endpos, _v) in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv965 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 9938 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_constructor_params) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LBRACKET ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState264 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BAR | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState264
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState264) : 'freshtv966)) : 'freshtv968)) : 'freshtv970)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv971 * Lexing.position * 'tv_constructor_parameters) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv972)) : 'freshtv974)

and _menhir_goto_field_declarations : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_field_declarations -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ('freshtv961 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | SEMICOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv959 * _menhir_state * 'tv_field_declarations) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState255 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run252 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState256 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACE ->
            _menhir_reduce217 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState256) : 'freshtv960)
    | RBRACE ->
        _menhir_reduce216 _menhir_env (Obj.magic _menhir_stack) MenhirState255
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState255) : 'freshtv962)

and _menhir_goto_type_declaration : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_type_declaration -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv957) = Obj.magic _menhir_stack in
    let (_endpos : Lexing.position) = _endpos in
    let (_v : 'tv_type_declaration) = _v in
    let (_startpos : Lexing.position) = _startpos in
    ((let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv955) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let ((_1 : 'tv_type_declaration) : 'tv_type_declaration) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _endpos = _endpos__1_ in
    let _v : 'tv_declaration = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 225 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                      ( fst _1, mkdecl ~loc:_sloc (PDtype (snd _1)) )
# 10008 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv956)) : 'freshtv958)

and _menhir_run241 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState241 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ARRAY ->
        _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState241 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState241 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIST ->
        _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState241 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState241 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MAPPING ->
        _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState241 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState241

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_run436 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10043 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv953) = Obj.magic _menhir_stack in
    ((let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_event_parameters = 
# 334 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( [] )
# 10054 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_event_parameters _menhir_env _menhir_stack _endpos _v) : 'freshtv954)

and _menhir_run263 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10061 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv951) = Obj.magic _menhir_stack in
    ((let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_constructor_parameters = 
# 308 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( [] )
# 10072 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_constructor_parameters _menhir_env _menhir_stack _endpos _v) : 'freshtv952)

and _menhir_run114 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv947 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        ((let _menhir_stack = (_menhir_stack, _endpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOUBLEARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv943 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ASSERT ->
                _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BEGIN ->
                _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | DENY ->
                _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | EMIT ->
                _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FAIL ->
                _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FIRST ->
                _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOLD ->
                _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | FOR ->
                _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | GHOST ->
                _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IF ->
                _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LET ->
                _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MATCH ->
                _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | TRANSFERETH ->
                _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState116 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState116 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState116) : 'freshtv944)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv945 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv946)) : 'freshtv948)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv949 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv950)

and _menhir_run195 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10162 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLONCOLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv939) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState195 in
        ((let _menhir_stack = (_menhir_stack, _menhir_s) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv935 * _menhir_state) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10184 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv933 * _menhir_state) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let ((_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10194 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10198 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos__2_ : Lexing.position) = _startpos in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_match_pattern_tail = 
# 630 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                     ( PPTcons _2 )
# 10206 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_match_pattern_tail _menhir_env _menhir_stack _menhir_s _v) : 'freshtv934)) : 'freshtv936)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv937 * _menhir_state) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv938)) : 'freshtv940)
    | IDENT _v ->
        _menhir_run196 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState195 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DOUBLEARROW ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv941) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState195 in
        ((let _v : 'tv_match_pattern_tail = 
# 629 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
          ( PPTother [] )
# 10225 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_match_pattern_tail _menhir_env _menhir_stack _menhir_s _v) : 'freshtv942)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState195

and _menhir_goto_nonempty_list_atom_ : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_nonempty_list_atom_ -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState509 | MenhirState505 | MenhirState499 | MenhirState453 | MenhirState235 | MenhirState24 | MenhirState203 | MenhirState116 | MenhirState192 | MenhirState119 | MenhirState189 | MenhirState122 | MenhirState123 | MenhirState185 | MenhirState126 | MenhirState136 | MenhirState148 | MenhirState180 | MenhirState178 | MenhirState176 | MenhirState156 | MenhirState160 | MenhirState172 | MenhirState161 | MenhirState164 | MenhirState162 | MenhirState158 | MenhirState154 | MenhirState152 | MenhirState146 | MenhirState142 | MenhirState140 | MenhirState134 | MenhirState132 | MenhirState120 | MenhirState109 | MenhirState107 | MenhirState27 | MenhirState28 | MenhirState98 | MenhirState29 | MenhirState33 | MenhirState36 | MenhirState85 | MenhirState83 | MenhirState81 | MenhirState79 | MenhirState77 | MenhirState75 | MenhirState73 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState50 | MenhirState48 | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv923 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DOT ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv917 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState45 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState45 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState45) : 'freshtv918)
        | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | MINUS | MOD | OBJECT | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | SLASH | STAR | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv919 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _endpos_a_, _menhir_s, (a : 'tv_nonempty_list_atom_), _startpos_a_) = _menhir_stack in
            let _startpos = _startpos_a_ in
            let _endpos = _endpos_a_ in
            let _v : 'tv_expression = let _endpos = _endpos_a_ in
            let _symbolstartpos = _startpos_a_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 506 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
            ( match a with
      | [hd] -> hd
      | _ -> mkexp_ ~loc:_sloc (PEapp a)
    )
# 10278 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv920)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv921 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv922)) : 'freshtv924)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv927 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position)) * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv925 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position)) * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_a1_, _menhir_s, (a1 : 'tv_nonempty_list_atom_), _startpos_a1_), _endpos_a2_, _, (a2 : 'tv_nonempty_list_atom_), _startpos_a2_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos_a1_ in
        let _endpos = _endpos_a2_ in
        let _v : 'tv_expression = let _endpos = _endpos_a2_ in
        let _symbolstartpos = _startpos_a1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 510 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                          ( let x = mkexp_ ~loc:_sloc (PEfield (a1, a2)) in
      (* print_endline ("PEfield: " ^ (string_of_p_expression x)); *) x
    )
# 10305 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv926)) : 'freshtv928)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv931 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv929 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_atom), _startpos_x_), _endpos_xs_, _, (xs : 'tv_nonempty_list_atom_), _startpos_xs_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_nonempty_list_atom_ = 
# 223 "<standard.mly>"
    ( x :: xs )
# 10319 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_nonempty_list_atom_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv930)) : 'freshtv932)
    | _ ->
        _menhir_fail ()

and _menhir_reduce169 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_method_kind = 
# 379 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
             ( MKnormal )
# 10330 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v

and _menhir_run282 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv915) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_method_kind = 
# 385 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
            ( MKrefined )
# 10344 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv916)

and _menhir_run284 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv913) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _v : 'tv_method_kind = 
# 384 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
             ( MKlogical )
# 10360 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv914)

and _menhir_run285 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv907 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv905 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_method_kind = 
# 383 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                ( MKconstghost )
# 10382 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv906)) : 'freshtv908)
    | IDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv909 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_method_kind = 
# 381 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
             ( MKghost )
# 10393 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv910)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv911 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv912)

and _menhir_run287 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv901 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ARRAY ->
            _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState288 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIST ->
            _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MAPPING ->
            _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState288 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState288) : 'freshtv902)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv903 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv904)

and _menhir_run292 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | GHOST ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv895 * _menhir_state) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv893 * _menhir_state) = Obj.magic _menhir_stack in
        let (_startpos__2_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _v : 'tv_method_kind = 
# 382 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                ( MKconstghost )
# 10460 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv894)) : 'freshtv896)
    | IDENT _ ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv897 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_method_kind = 
# 380 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
             ( MKconst )
# 10471 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_method_kind _menhir_env _menhir_stack _menhir_s _v) : 'freshtv898)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv899 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv900)

and _menhir_goto_object_signature_expr : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_object_signature_expr -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState280 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv845 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10491 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run324 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WITH ->
            _menhir_run308 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv841 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10505 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10510 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__2_), _endpos__4_, _, (_4 : 'tv_object_signature_expr), _startpos__4_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__4_ in
            let _v : 'tv_object_signature_declaration = 
# 351 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _2, _4 )
# 10519 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv842)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv843 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10529 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv844)) : 'freshtv846)
    | MenhirState337 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv861 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10538 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run324 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WITH ->
            _menhir_run308 _menhir_env (Obj.magic _menhir_stack)
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv857 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10552 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10557 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__1_), _endpos__3_, _, (_3 : 'tv_object_signature_expr), _startpos__3_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_layer_signature_field = 
# 409 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                       ( _1, _3 )
# 10563 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv855) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_layer_signature_field) = _v in
            ((match _menhir_s with
            | MenhirState340 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv849 * _menhir_state * 'tv_layer_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_layer_signature_field) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv847 * _menhir_state * 'tv_layer_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : 'tv_layer_signature_field) : 'tv_layer_signature_field) = _v in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_layer_signature_fields)), _) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_layer_signature_fields = 
# 406 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                            ( _3 :: _1 )
# 10584 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_layer_signature_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv848)) : 'freshtv850)
            | MenhirState334 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv853) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_layer_signature_field) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv851) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_layer_signature_field) : 'tv_layer_signature_field) = _v in
                ((let _v : 'tv_layer_signature_fields = 
# 405 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                           ( [_1] )
# 10599 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_layer_signature_fields _menhir_env _menhir_stack _menhir_s _v) : 'freshtv852)) : 'freshtv854)
            | _ ->
                _menhir_fail ()) : 'freshtv856)) : 'freshtv858)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv859 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10611 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv860)) : 'freshtv862)
    | MenhirState460 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv867 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10620 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run324 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WITH ->
            _menhir_run308 _menhir_env (Obj.magic _menhir_stack)
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv863 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10634 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _endpos__1_, (_1 : 'tv_opt_logical_or_trusted), _startpos__1_), _startpos__2_), _startpos__3_), _endpos__4_, (_4 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10639 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__4_), _endpos__6_, _, (_6 : 'tv_object_signature_expr), _startpos__6_) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _2 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__6_ in
            let _v : 'tv_object_signature_declaration = 
# 354 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( if _1 <> POnormal then
          print_endline "Warning: object signatures should not be marked logical or trusted";
        _4, _6 )
# 10651 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv864)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv865 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10661 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv866)) : 'freshtv868)
    | MenhirState466 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv883 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10670 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MINUS ->
            _menhir_run324 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WITH ->
            _menhir_run308 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv879 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10684 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10689 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__1_), _endpos__3_, _, (_3 : 'tv_object_signature_expr), _startpos__3_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_base_slot = 
# 673 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                       ( _1, _3 )
# 10695 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv877) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_base_slot) = _v in
            ((match _menhir_s with
            | MenhirState472 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv871 * _menhir_state * 'tv_base_slots)) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_base_slot) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv869 * _menhir_state * 'tv_base_slots)) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : 'tv_base_slot) : 'tv_base_slot) = _v in
                ((let (_menhir_stack, _menhir_s, (_1 : 'tv_base_slots)) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_base_slots = 
# 670 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                ( _3 :: _1 )
# 10716 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_base_slots _menhir_env _menhir_stack _menhir_s _v) : 'freshtv870)) : 'freshtv872)
            | MenhirState463 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv875) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_base_slot) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv873) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_base_slot) : 'tv_base_slot) = _v in
                ((let _v : 'tv_base_slots = 
# 669 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
               ( [_1] )
# 10731 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_base_slots _menhir_env _menhir_stack _menhir_s _v) : 'freshtv874)) : 'freshtv876)
            | _ ->
                _menhir_fail ()) : 'freshtv878)) : 'freshtv880)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv881 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10743 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv882)) : 'freshtv884)
    | MenhirState479 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv891 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10752 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv885 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10762 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | CLONE ->
                _menhir_run365 _menhir_env (Obj.magic _menhir_stack) MenhirState515 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run364 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState515 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run363 _menhir_env (Obj.magic _menhir_stack) MenhirState515 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState515) : 'freshtv886)
        | LBRACE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv887 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10782 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LET ->
                _menhir_run482 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState481 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState481) : 'freshtv888)
        | MINUS ->
            _menhir_run324 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | WITH ->
            _menhir_run308 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv889 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10806 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv890)) : 'freshtv892)
    | _ ->
        _menhir_fail ()

and _menhir_goto_base_layer_signature : _menhir_env -> 'ttv_tail -> 'tv_base_layer_signature -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = (_menhir_stack, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : ((('freshtv839 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10820 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    ) * Lexing.position) * 'tv_base_layer_signature) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv835 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10830 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run306 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState479 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run281 _menhir_env (Obj.magic _menhir_stack) MenhirState479 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState479) : 'freshtv836)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv837 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10850 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv838)) : 'freshtv840)

and _menhir_run466 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10857 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run306 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState466 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run281 _menhir_env (Obj.magic _menhir_stack) MenhirState466 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState466

and _menhir_run363 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CLONE ->
        _menhir_run365 _menhir_env (Obj.magic _menhir_stack) MenhirState363 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run364 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState363 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run363 _menhir_env (Obj.magic _menhir_stack) MenhirState363 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState363

and _menhir_run364 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10892 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv833) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10903 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10907 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_object_expression = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 734 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
           ( mkobj ~loc:_sloc (POname _1) )
# 10918 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_object_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv834)

and _menhir_run365 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv829 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10935 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) = _v in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv827 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let ((_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10945 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10949 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) = _v in
        let (_startpos__2_ : Lexing.position) = _startpos in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_object_expression = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 735 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                 (mkobj ~loc:_sloc (POclone _2))
# 10962 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_object_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv828)) : 'freshtv830)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv831 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv832)

and _menhir_goto_layer_signature : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_layer_signature -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState333 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv799 * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10982 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv797 * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10988 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 10993 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__3_), _endpos__5_, _, (_5 : 'tv_layer_signature)) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_layer_signature_declaration = 
# 398 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _3, _5 )
# 11003 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv795) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_layer_signature_declaration) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv793) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_v : 'tv_layer_signature_declaration) = _v in
        let (_startpos : Lexing.position) = _startpos in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv791) = Obj.magic _menhir_stack in
        let (_endpos__1_ : Lexing.position) = _endpos in
        let ((_1 : 'tv_layer_signature_declaration) : 'tv_layer_signature_declaration) = _v in
        let (_startpos__1_ : Lexing.position) = _startpos in
        ((let _endpos = _endpos__1_ in
        let _v : 'tv_declaration = let _endpos = _endpos__1_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 227 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                 ( fst _1, mkdecl ~loc:_sloc  (PDlayer_sig (snd _1)) )
# 11027 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv792)) : 'freshtv794)) : 'freshtv796)) : 'freshtv798)) : 'freshtv800)
    | MenhirState351 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv803 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv801 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _), _endpos__3_, _, (_3 : 'tv_layer_signature)) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos__3_ in
        let _v : 'tv_layer_type = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 417 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( { pLayerBase = mklayersign ~loc:_sloc (PLSconstr []); pLayerSignature = _3; pLayerLoc=(make_loc _sloc) } )
# 11045 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_type _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv802)) : 'freshtv804)
    | MenhirState350 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv809 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv805 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_stack = (_menhir_stack, _endpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run345 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState354 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState354 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState354) : 'freshtv806)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv807 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv808)) : 'freshtv810)
    | MenhirState354 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv813 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv811 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_layer_signature)), _endpos__3_), _endpos__4_, _, (_4 : 'tv_layer_signature)) = _menhir_stack in
        let _3 = () in
        let _1 = () in
        let _endpos = _endpos__4_ in
        let _v : 'tv_layer_type = let _endpos = _endpos__4_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 415 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( { pLayerBase = _2; pLayerSignature = _4; pLayerLoc =(make_loc _sloc) } )
# 11092 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_type _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv812)) : 'freshtv814)
    | MenhirState369 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv817 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv815 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_object_expression), _startpos__1_), _endpos__3_, _, (_3 : 'tv_layer_signature)) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_object_expression = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 736 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                    ( mkobj ~loc:_sloc (POrelax (_1, _3)) )
# 11110 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_object_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv816)) : 'freshtv818)
    | MenhirState463 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv825 * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv821 * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv819 * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            ((let ((_menhir_stack, _startpos__1_), _endpos__2_, _, (_2 : 'tv_layer_signature)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_base_layer_signature = 
# 665 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                   ( _2 )
# 11133 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_base_layer_signature _menhir_env _menhir_stack _v) : 'freshtv820)) : 'freshtv822)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv823 * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv824)) : 'freshtv826)
    | _ ->
        _menhir_fail ()

and _menhir_run336 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11149 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | COLON ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv787 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11161 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run306 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState337 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run281 _menhir_env (Obj.magic _menhir_stack) MenhirState337 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState337) : 'freshtv788)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv789 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11181 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv790)

and _menhir_reduce143 : _menhir_env -> 'ttv_tail * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11189 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) * Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11195 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )), _startpos__1_) = _menhir_stack in
    let _endpos = _endpos__1_ in
    let _v : 'tv_layer_signature = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 401 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
           ( mklayersign ~loc:_sloc (PLSname _1) )
# 11204 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_layer_signature _menhir_env _menhir_stack _endpos _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11211 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv785) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11222 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11226 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    
# 802 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
          ( PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc  (PEconstant (CONuint (int_of_string(_1))))))) )
# 11235 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv786)

and _menhir_run8 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11242 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState8 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
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
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv717 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv713 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11285 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | EQUAL ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState129
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState129) : 'freshtv714)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv715 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv716)) : 'freshtv718)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv723 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv719 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11320 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | EQUAL ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState139
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState139) : 'freshtv720)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv721 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv722)) : 'freshtv724)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv729 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv725 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11355 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | EQUAL ->
                _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState151
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState151) : 'freshtv726)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv727 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv728)) : 'freshtv730)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv737 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11382 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv733 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11392 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | BAR ->
                _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | LBRACE ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv731) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState250 in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run252 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState251 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState251) : 'freshtv732)
            | IDENT _ ->
                _menhir_reduce211 _menhir_env (Obj.magic _menhir_stack) MenhirState250
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState250) : 'freshtv734)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv735 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11427 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv736)) : 'freshtv738)
    | MenhirState264 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv751 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11436 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_constructor_params) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv749 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11442 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_constructor_params) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11447 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )), _startpos__1_), _endpos__2_, (_2 : 'tv_constructor_params)), _endpos__3_, _, (_3 : 'tv_annotations)) = _menhir_stack in
        let _endpos = _endpos__3_ in
        let _v : 'tv_constructor_declaration = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 298 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( { pTypeConstrName = _1;
          pTypeConstrArgs = _2;
          pTypeConstrAnnotations = _3;
          pTypeConstrLoc = (make_loc _sloc ) } )
# 11459 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv747) = _menhir_stack in
        let (_endpos : Lexing.position) = _endpos in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_constructor_declaration) = _v in
        ((match _menhir_s with
        | MenhirState274 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv741 * Lexing.position * _menhir_state * 'tv_constructor_declarations)) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_constructor_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv739 * Lexing.position * _menhir_state * 'tv_constructor_declarations)) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            let ((_3 : 'tv_constructor_declaration) : 'tv_constructor_declaration) = _v in
            ((let (_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_constructor_declarations)) = _menhir_stack in
            let _2 = () in
            let _endpos = _endpos__3_ in
            let _v : 'tv_constructor_declarations = 
# 294 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                          ( _3 :: _1 )
# 11484 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_constructor_declarations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv740)) : 'freshtv742)
        | MenhirState262 ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv745) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_constructor_declaration) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv743) = Obj.magic _menhir_stack in
            let (_endpos__1_ : Lexing.position) = _endpos in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : 'tv_constructor_declaration) : 'tv_constructor_declaration) = _v in
            ((let _endpos = _endpos__1_ in
            let _v : 'tv_constructor_declarations = 
# 293 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                             ( [_1] )
# 11502 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_constructor_declarations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv744)) : 'freshtv746)
        | _ ->
            _menhir_fail ()) : 'freshtv748)) : 'freshtv750)) : 'freshtv752)
    | MenhirState347 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv759 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11512 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv753) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACKET ->
                _menhir_run350 _menhir_env (Obj.magic _menhir_stack) MenhirState349 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState349) : 'freshtv754)
        | EQUAL ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv755) = Obj.magic _menhir_stack in
            ((let _v : 'tv_layer_signature_annotation = 
# 754 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( None )
# 11535 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_layer_signature_annotation _menhir_env _menhir_stack _v) : 'freshtv756)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv757 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11545 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv758)) : 'freshtv760)
    | MenhirState409 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv761 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11554 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState410
        | EQUAL ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState410
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState410) : 'freshtv762)
    | MenhirState415 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv767 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11572 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv763 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11582 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState417 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ARRAY ->
                _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState417 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState417 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState417) : 'freshtv764)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv765 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11610 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv766)) : 'freshtv768)
    | MenhirState407 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv779 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | WITH ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv775 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | STRING _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv771 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11633 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv769 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                let ((_4 : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11643 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) : (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11647 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) = _v in
                let (_startpos__4_ : Lexing.position) = _startpos in
                ((let ((_menhir_stack, _endpos__1_, _startpos__1_), _endpos__2_, _, (_2 : 'tv_annotations)) = _menhir_stack in
                let _3 = () in
                let _1 = () in
                let _endpos = _endpos__4_ in
                let _v : 'tv_external_declaration = let _endpos = _endpos__4_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 237 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( "", mkdecl ~loc:_sloc (PDexternal_with (_4, _2)) )
# 11660 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_external_declaration _menhir_env _menhir_stack _endpos _v) : 'freshtv770)) : 'freshtv772)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv773 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv774)) : 'freshtv776)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv777 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv778)) : 'freshtv780)
    | MenhirState482 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv783 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | CONST ->
            _menhir_run292 _menhir_env (Obj.magic _menhir_stack) MenhirState483
        | CONSTRUCTOR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv781 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState483 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                _menhir_run496 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState484 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run485 _menhir_env (Obj.magic _menhir_stack) MenhirState484 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState484) : 'freshtv782)
        | GHOST ->
            _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState483 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LOGICAL ->
            _menhir_run284 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState483 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | REFINED ->
            _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState483
        | IDENT _ ->
            _menhir_reduce169 _menhir_env (Obj.magic _menhir_stack) MenhirState483
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState483) : 'freshtv784)
    | _ ->
        _menhir_fail ()

and _menhir_run13 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11719 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv711) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11730 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11734 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _v : 'tv_annotation = let _endpos = _endpos__1_ in
    let _startpos = _startpos__1_ in
    let _loc = (_startpos, _endpos) in
    
# 801 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
         ( PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc  (PEconstant (CONint (int_of_string(_1))))))) )
# 11743 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv712)

and _menhir_run14 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 11750 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState14 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | STRING _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run9 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState14 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | COMMA | RBRACKET | RPAREN ->
        _menhir_reduce10 _menhir_env (Obj.magic _menhir_stack) MenhirState14
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14

and _menhir_run24 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ASSERT ->
        _menhir_run162 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BEGIN ->
        _menhir_run161 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | DENY ->
        _menhir_run160 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | EMIT ->
        _menhir_run158 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FAIL ->
        _menhir_run157 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FIRST ->
        _menhir_run149 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOLD ->
        _menhir_run137 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | FOR ->
        _menhir_run127 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | GHOST ->
        _menhir_run123 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState24 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IF ->
        _menhir_run120 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState24 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LET ->
        _menhir_run117 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MATCH ->
        _menhir_run109 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | TRANSFERETH ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState24 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState24 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run211 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv707 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv695 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11852 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv691 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11864 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv689 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11872 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11878 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_annotation = let _endpos = _endpos__4_ in
                let _startpos = _startpos__1_ in
                let _loc = (_startpos, _endpos) in
                
# 803 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                              ( PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc (PEconstant (CONaddress _3))))) )
# 11889 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv690)) : 'freshtv692)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv693 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11899 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv694)) : 'freshtv696)
        | UINT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv703 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11910 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv699 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11922 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv697 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11930 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11936 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _v : 'tv_annotation = let _endpos = _endpos__4_ in
                let _startpos = _startpos__1_ in
                let _loc = (_startpos, _endpos) in
                
# 804 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                               ( PAexpr (mkcmd ~loc:_loc (PCyield (mkexp_ ~loc:_loc (PEconstant (CONaddress _3))))) )
# 11947 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv698)) : 'freshtv700)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv701 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 11957 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv702)) : 'freshtv704)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv705 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv706)) : 'freshtv708)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv709 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv710)

and _menhir_goto_type_expression : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_type_expression -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    match _menhir_s with
    | MenhirState237 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv595 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv593 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos_e_, _, (e : 'tv_expression), _startpos_e_), _endpos__4_), _endpos__5_, _, (_5 : 'tv_type_expression), _startpos__5_) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _1 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_type_expression = let _endpos = _endpos__5_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 272 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( mkfotyp ~loc:_sloc (PTarray (int_of_p_expression e.p_expression_desc, _5)) )
# 11997 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv594)) : 'freshtv596)
    | MenhirState232 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv599 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState240
        | ARROW | ASSIGN | COMMA | CONST | EOF | EQUAL | EVENT | EXTERNAL | INDEXED | LAYER | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv597 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_type_expression), _startpos__2_) = _menhir_stack in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__2_ in
            let _v : 'tv_type_expression = let _endpos = _endpos__2_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 276 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( mkfotyp ~loc:_sloc (PTlist _2) )
# 12021 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv598)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState240) : 'freshtv600)
    | MenhirState241 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv603 * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv601 * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _endpos__1_, _menhir_s, (_1 : 'tv_type_expression), _startpos__1_), _), _endpos__3_, _, (_3 : 'tv_type_expression), _startpos__3_) = _menhir_stack in
        let _2 = () in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_type_expression = let _endpos = _endpos__3_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 270 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( mkfotyp ~loc:_sloc (PTprod (_1, _3)) )
# 12043 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv602)) : 'freshtv604)
    | MenhirState231 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv609 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv607 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState243 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv605 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__3_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            ((let ((_menhir_stack, _menhir_s, _startpos__1_), _endpos__2_, _, (_2 : 'tv_type_expression), _startpos__2_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__3_ in
            let _v : 'tv_type_expression = 
# 268 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _2 )
# 12070 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv606)) : 'freshtv608)
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState243
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState243) : 'freshtv610)
    | MenhirState230 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv613 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv611 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState245 in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ARRAY ->
                _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState246 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState246 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState246) : 'freshtv612)
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState245
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState245) : 'freshtv614)
    | MenhirState246 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv617 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState247
        | ARROW | ASSIGN | COMMA | CONST | EOF | EQUAL | EVENT | EXTERNAL | INDEXED | LAYER | LOGICAL | OBJECT | RBRACE | RBRACKET | RPAREN | SEMICOLON | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv615 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, _, (_3 : 'tv_type_expression), _startpos__3_), _endpos__4_, _), _endpos__5_, _, (_5 : 'tv_type_expression), _startpos__5_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos__5_ in
            let _v : 'tv_type_expression = let _endpos = _endpos__5_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 274 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( mkfotyp ~loc:_sloc (PTmapping (_3, _5)) )
# 12139 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv616)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState247) : 'freshtv618)
    | MenhirState228 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv621 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12151 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState248
        | CONST | EOF | EVENT | EXTERNAL | LAYER | LOGICAL | OBJECT | SIGNATURE | TRUSTED | TYPE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv619 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12163 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _startpos__1_), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12168 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__2_), _), _endpos_te_, _, (te : 'tv_type_expression), _startpos_te_) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _startpos = _startpos__1_ in
            let _endpos = _endpos_te_ in
            let _v : 'tv_type_declaration = let _endpos = _endpos_te_ in
            let _symbolstartpos = _startpos__1_ in
            let _sloc = (_symbolstartpos, _endpos) in
            
# 254 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      (   _2,  mkfotyp ~loc:_sloc te.p_type_FO_desc )
# 12180 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_type_declaration _menhir_env _menhir_stack _endpos _v _startpos) : 'freshtv620)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState248) : 'freshtv622)
    | MenhirState253 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv635 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12192 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState254
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv633 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12204 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _endpos__1_, _menhir_s, (_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12209 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__1_), _endpos__3_, _, (_3 : 'tv_type_expression), _startpos__3_) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_field_declaration = 
# 289 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                 ( _1, _3 )
# 12215 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv631) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : 'tv_field_declaration) = _v in
            ((match _menhir_s with
            | MenhirState256 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv625 * _menhir_state * 'tv_field_declarations) * _menhir_state) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_field_declaration) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv623 * _menhir_state * 'tv_field_declarations) * _menhir_state) = Obj.magic _menhir_stack in
                let (_ : _menhir_state) = _menhir_s in
                let ((_3 : 'tv_field_declaration) : 'tv_field_declaration) = _v in
                ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_field_declarations)), _) = _menhir_stack in
                let _2 = () in
                let _v : 'tv_field_declarations = 
# 286 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                    ( _3 :: _1 )
# 12236 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_field_declarations _menhir_env _menhir_stack _menhir_s _v) : 'freshtv624)) : 'freshtv626)
            | MenhirState251 ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv629) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let (_v : 'tv_field_declaration) = _v in
                ((let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv627) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = _menhir_s in
                let ((_1 : 'tv_field_declaration) : 'tv_field_declaration) = _v in
                ((let _v : 'tv_field_declarations = 
# 285 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                       ( [_1] )
# 12251 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_field_declarations _menhir_env _menhir_stack _menhir_s _v) : 'freshtv628)) : 'freshtv630)
            | _ ->
                _menhir_fail ()) : 'freshtv632)) : 'freshtv634)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState254) : 'freshtv636)
    | MenhirState269 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv647 * Lexing.position) * _menhir_state * 'tv_idents)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv645 * Lexing.position) * _menhir_state * 'tv_idents)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState270 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv643 * Lexing.position) * _menhir_state * 'tv_idents)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos__5_ : Lexing.position) = _endpos in
            let (_ : _menhir_state) = _menhir_s in
            ((let (((_menhir_stack, _startpos__1_), _, (_2 : 'tv_idents)), _endpos__4_, _, (_4 : 'tv_type_expression), _startpos__4_) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _endpos = _endpos__5_ in
            let _v : 'tv_constructor_parameter = 
# 313 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( List.map (fun i -> i, _4) _2 )
# 12284 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv641) = _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_v : 'tv_constructor_parameter) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv639 * Lexing.position * 'tv_constructor_parameters) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _endpos in
            let (_v : 'tv_constructor_parameter) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv637 * Lexing.position * 'tv_constructor_parameters) = Obj.magic _menhir_stack in
            let (_endpos__2_ : Lexing.position) = _endpos in
            let ((_2 : 'tv_constructor_parameter) : 'tv_constructor_parameter) = _v in
            ((let (_menhir_stack, _endpos__1_, (_1 : 'tv_constructor_parameters)) = _menhir_stack in
            let _endpos = _endpos__2_ in
            let _v : 'tv_constructor_parameters = 
# 309 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                                  ( _2 :: _1 )
# 12303 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_constructor_parameters _menhir_env _menhir_stack _endpos _v) : 'freshtv638)) : 'freshtv640)) : 'freshtv642)) : 'freshtv644)) : 'freshtv646)
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState270
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState270) : 'freshtv648)
    | MenhirState288 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv651 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv649 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState289 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState290 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ARRAY ->
                _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState290 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState290 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState290 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState290 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState290 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState290) : 'freshtv650)
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState289
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState289) : 'freshtv652)
    | MenhirState290 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv655 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState291
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv653 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _endpos__3_, _, (_3 : 'tv_type_expression), _startpos__3_), _), _endpos__5_, _, (_5 : 'tv_type_expression), _startpos__5_) = _menhir_stack in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_object_signature_field = 
# 376 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      (  "constructor", _3, _5, MKconstructor  )
# 12366 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_field _menhir_env _menhir_stack _menhir_s _v) : 'freshtv654)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState291) : 'freshtv656)
    | MenhirState299 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv659 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12378 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv657 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12388 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState300 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ARRAY ->
                _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState301 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState301 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState301) : 'freshtv658)
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState300
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState300) : 'freshtv660)
    | MenhirState301 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv663 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12422 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState302
        | RBRACE | SEMICOLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv661 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12434 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s, (_1 : 'tv_method_kind)), _endpos__2_, (_2 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12439 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )), _startpos__2_), _endpos__4_, _, (_4 : 'tv_type_expression), _startpos__4_), _), _endpos__6_, _, (_6 : 'tv_type_expression), _startpos__6_) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _v : 'tv_object_signature_field = 
# 374 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( _2, _4, _6, _1 )
# 12446 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_object_signature_field _menhir_env _menhir_stack _menhir_s _v) : 'freshtv662)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState302) : 'freshtv664)
    | MenhirState417 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv667 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12458 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv665 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12468 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState418 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState419 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | ARRAY ->
                _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState419 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState419 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LIST ->
                _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState419 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState419 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MAPPING ->
                _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState419 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState419) : 'freshtv666)
        | ASSIGN ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState418
        | EQUAL ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState418
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState418
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState418) : 'freshtv668)
    | MenhirState419 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv669 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12506 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState420
        | EQUAL ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState420
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState420
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState420) : 'freshtv670)
    | MenhirState427 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv671 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12526 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run131 _menhir_env (Obj.magic _menhir_stack) MenhirState428
        | EQUAL ->
            _menhir_run130 _menhir_env (Obj.magic _menhir_stack) MenhirState428
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState428
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState428) : 'freshtv672)
    | MenhirState440 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv679 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12546 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INDEXED ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv675) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState441 in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv673) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            ((let _1 = () in
            let _v : 'tv_indexed_opt = 
# 340 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
          ( true )
# 12563 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_indexed_opt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv674)) : 'freshtv676)
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState441
        | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv677) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState441 in
            ((let _v : 'tv_indexed_opt = 
# 339 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
  ( false )
# 12575 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_indexed_opt _menhir_env _menhir_stack _menhir_s _v) : 'freshtv678)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState441) : 'freshtv680)
    | MenhirState488 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv683 * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState489
        | COMMA | EQUAL | RPAREN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv681 * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _endpos__2_, _, (_2 : 'tv_type_expression), _startpos__2_) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_opt_type_annotation = 
# 731 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                           ( Some _2 )
# 12598 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
             in
            _menhir_goto_opt_type_annotation _menhir_env _menhir_stack _menhir_s _v) : 'freshtv682)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState489) : 'freshtv684)
    | MenhirState503 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv687 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12610 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv685 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12620 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState504 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ADDRESS ->
                _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState505 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BANG ->
                _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState505 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | BITNOT ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState505 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | IDENT _v ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState505 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | INT _v ->
                _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState505 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LBRACE ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState505 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | LPAREN ->
                _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState505 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | MINUS ->
                _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState505 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | UINT _v ->
                _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState505 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState505) : 'freshtv686)
        | STAR ->
            _menhir_run241 _menhir_env (Obj.magic _menhir_stack) MenhirState504
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState504) : 'freshtv688)
    | _ ->
        _menhir_fail ()

and _menhir_goto_opt_bar : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_opt_bar -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv587 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run195 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState113 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACKET ->
            _menhir_run114 _menhir_env (Obj.magic _menhir_stack) MenhirState113 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState113) : 'freshtv588)
    | MenhirState250 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv589 * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run263 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState262 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState262) : 'freshtv590)
    | MenhirState434 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv591) * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            _menhir_run436 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState435 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState435) : 'freshtv592)
    | _ ->
        _menhir_fail ()

and _menhir_run32 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12706 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | EQUAL ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv583 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12718 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState33 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33) : 'freshtv584)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv585 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12752 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv586)

and _menhir_goto_atom : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> 'tv_atom -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv581 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv577 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = MenhirState47 in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState48 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48) : 'freshtv578)
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState47 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState47 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ASSIGN | BAR | BARBAR | BITAND | COMMA | CONJUNCTION | CONST | DISJUNCTION | DO | DOT | ELSE | END | EOF | EQUAL | EVENT | EXTERNAL | GREATER | GREATEREQ | IN | LAYER | LESS | LESSEQ | LET | LOGICAL | MINUS | MOD | OBJECT | PLUS | RBRACE | RBRACKET | RPAREN | SEMICOLON | SHL | SHR | SIGNATURE | SLASH | STAR | THEN | TO | TRUSTED | TYPE | UNEQUAL | WITH | XOR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv579 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _endpos_x_, _menhir_s, (x : 'tv_atom), _startpos_x_) = _menhir_stack in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_nonempty_list_atom_ = 
# 221 "<standard.mly>"
    ( [ x ] )
# 12815 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_nonempty_list_atom_ _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv580)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState47) : 'freshtv582)

and _menhir_run281 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | CONST ->
        _menhir_run292 _menhir_env (Obj.magic _menhir_stack) MenhirState281
    | CONSTRUCTOR ->
        _menhir_run287 _menhir_env (Obj.magic _menhir_stack) MenhirState281
    | GHOST ->
        _menhir_run285 _menhir_env (Obj.magic _menhir_stack) MenhirState281 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LOGICAL ->
        _menhir_run284 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState281 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv575 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState281 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv573 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
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
        
# 360 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                   ( mksig ~loc:_sloc (PSconstr []) )
# 12858 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv574)) : 'freshtv576)
    | REFINED ->
        _menhir_run282 _menhir_env (Obj.magic _menhir_stack) MenhirState281
    | IDENT _ ->
        _menhir_reduce169 _menhir_env (Obj.magic _menhir_stack) MenhirState281
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState281

and _menhir_run306 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12873 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv571) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12884 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12888 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_object_signature_expr = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 359 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
           ( mksig ~loc:_sloc (PSname _1) )
# 12899 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_object_signature_expr _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv572)

and _menhir_goto_opt_logical_or_trusted : _menhir_env -> 'ttv_tail -> Lexing.position -> 'tv_opt_logical_or_trusted -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv569 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | OBJECT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv565 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | IDENT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv551 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12926 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv535 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12938 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | CLONE ->
                    _menhir_run365 _menhir_env (Obj.magic _menhir_stack) MenhirState476 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run364 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState476 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run363 _menhir_env (Obj.magic _menhir_stack) MenhirState476 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState476) : 'freshtv536)
            | LPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv545) = Obj.magic _menhir_stack in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv539) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_menhir_s : _menhir_state) = MenhirState463 in
                    let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12969 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                    )) = _v in
                    let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                    ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | COLON ->
                        _menhir_run466 _menhir_env (Obj.magic _menhir_stack)
                    | RPAREN ->
                        _menhir_reduce143 _menhir_env (Obj.magic _menhir_stack)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : 'freshtv537 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 12987 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                        ) * Lexing.position) = Obj.magic _menhir_stack in
                        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv538)) : 'freshtv540)
                | LBRACE ->
                    _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState463 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | RPAREN ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv543 * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                    let (_menhir_s : _menhir_state) = MenhirState463 in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : 'freshtv541 * Lexing.position) = Obj.magic _menhir_stack in
                    let (_endpos__2_ : Lexing.position) = _endpos in
                    let (_ : _menhir_state) = _menhir_s in
                    ((let (_menhir_stack, _startpos__1_) = _menhir_stack in
                    let _2 = () in
                    let _1 = () in
                    let _v : 'tv_base_layer_signature = let _endpos = _endpos__2_ in
                    let _symbolstartpos = _startpos__1_ in
                    let _sloc = (_symbolstartpos, _endpos) in
                    
# 664 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                   ( mklayersign ~loc:_sloc (PLSconstr []) )
# 13012 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                     in
                    _menhir_goto_base_layer_signature _menhir_env _menhir_stack _v) : 'freshtv542)) : 'freshtv544)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState463) : 'freshtv546)
            | COLON ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : 'freshtv547) = Obj.magic _menhir_stack in
                ((let (_, _endpos__0_) = Obj.magic _menhir_stack in
                let _v : 'tv_base_layer_signature = let _endpos = _endpos__0_ in
                let _symbolstartpos = _endpos in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 663 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( mklayersign ~loc:_sloc (PLSconstr []) )
# 13029 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_base_layer_signature _menhir_env _menhir_stack _v) : 'freshtv548)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv549 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13039 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv550)) : 'freshtv552)
        | SIGNATURE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv561 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | IDENT _v ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv557 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                let (_v : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13057 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | EQUAL ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv553 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13069 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | IDENT _v ->
                        _menhir_run306 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState460 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LBRACE ->
                        _menhir_run281 _menhir_env (Obj.magic _menhir_stack) MenhirState460 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState460) : 'freshtv554)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv555 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13089 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    (raise _eRR : 'freshtv556)) : 'freshtv558)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv559 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
                (raise _eRR : 'freshtv560)) : 'freshtv562)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv563 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            (raise _eRR : 'freshtv564)) : 'freshtv566)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv567 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv568)) : 'freshtv570)

and _menhir_run334 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run336 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState334 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RBRACE ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv533 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState334 in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv531 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos__2_ : Lexing.position) = _endpos in
        let (_ : _menhir_state) = _menhir_s in
        ((let (_menhir_stack, _menhir_s, _startpos__1_) = _menhir_stack in
        let _2 = () in
        let _1 = () in
        let _endpos = _endpos__2_ in
        let _v : 'tv_layer_signature = let _endpos = _endpos__2_ in
        let _symbolstartpos = _startpos__1_ in
        let _sloc = (_symbolstartpos, _endpos) in
        
# 402 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                   ( mklayersign ~loc:_sloc (PLSconstr []) )
# 13139 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_layer_signature _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv532)) : 'freshtv534)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState334

and _menhir_run345 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13150 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _v, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    _menhir_reduce143 _menhir_env (Obj.magic _menhir_stack)

and _menhir_reduce19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let (_, _endpos) = Obj.magic _menhir_stack in
    let _v : 'tv_annotations = 
# 790 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( [] )
# 13163 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
        let (_menhir_stack : 'freshtv527 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run211 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | EQUAL ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack) MenhirState6
        | IDENT _v ->
            _menhir_run14 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run13 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | RBRACKET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv525 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState6 in
            ((let _menhir_stack = (_menhir_stack, _endpos, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RBRACKET ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv521 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv519 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, _) = _menhir_stack in
                let _4 = () in
                let _3 = () in
                let _2 = () in
                let _1 = () in
                let _endpos = _endpos__4_ in
                let _v : 'tv_annotations = 
# 791 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                                         ( [] )
# 13215 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_annotations _menhir_env _menhir_stack _endpos _menhir_s _v) : 'freshtv520)) : 'freshtv522)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv523 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv524)) : 'freshtv526)
        | STRING _v ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState6 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6) : 'freshtv528)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv529 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv530)

and _menhir_run229 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv515 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | ARRAY ->
            _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState230 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LIST ->
            _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MAPPING ->
            _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState230 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState230) : 'freshtv516)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv517 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv518)

and _menhir_run231 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState231 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ARRAY ->
        _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState231 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIST ->
        _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MAPPING ->
        _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState231 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState231

and _menhir_run232 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState232 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | ARRAY ->
        _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState232 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LIST ->
        _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MAPPING ->
        _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState232 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState232

and _menhir_run233 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13328 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv513) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13339 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13343 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_type_expression = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 261 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( let x = try Hashtbl.find builtin_type_table _1
        with Not_found -> PTname _1
        in 
        mkfotyp ~loc:_sloc (x)  
       )
# 13358 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv514)

and _menhir_run234 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LBRACKET ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv509 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ADDRESS ->
            _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState235 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BANG ->
            _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState235 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | BITNOT ->
            _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState235 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | IDENT _v ->
            _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState235 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | INT _v ->
            _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState235 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LBRACE ->
            _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState235 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LPAREN ->
            _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState235 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | MINUS ->
            _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState235 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | UINT _v ->
            _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState235 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState235) : 'freshtv510)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv511 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv512)

and _menhir_run238 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv507) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _1 = () in
    let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_type_expression = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 266 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
            ( mkfotyp ~loc:_sloc (PTbuiltin Taddress) )
# 13423 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_type_expression _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv508)

and _menhir_reduce211 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_opt_bar = 
# 835 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
         ( () )
# 13432 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_opt_bar _menhir_env _menhir_stack _menhir_s _v

and _menhir_run112 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv505) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_opt_bar = 
# 836 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
         ( () )
# 13446 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_opt_bar _menhir_env _menhir_stack _menhir_s _v) : 'freshtv506)

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState515 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv131 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13458 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState511 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv133 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13467 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * _menhir_state * 'tv_object_fields_and_methods) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState509 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv135 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13476 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState507 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv137 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13485 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_method_param) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv138)
    | MenhirState505 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv139 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13494 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv140)
    | MenhirState504 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv141 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13503 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv142)
    | MenhirState503 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv143 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13512 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv144)
    | MenhirState502 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv145 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13521 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv146)
    | MenhirState499 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv147 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) * _menhir_state * 'tv_opt_type_annotation)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv148)
    | MenhirState497 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv149 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) * _menhir_state * 'tv_method_param) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)
    | MenhirState493 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv151 * _menhir_state * 'tv_method_parameters)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv152)
    | MenhirState489 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv153 * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)
    | MenhirState488 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv155 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv156)
    | MenhirState487 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv157 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13555 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv158)
    | MenhirState485 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv160)
    | MenhirState484 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv161 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv162)
    | MenhirState483 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv163 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv164)
    | MenhirState482 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv165 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv166)
    | MenhirState481 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv167 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13584 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv168)
    | MenhirState479 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv169 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13593 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * 'tv_base_layer_signature)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv170)
    | MenhirState476 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv171 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13601 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv172)
    | MenhirState472 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv173 * _menhir_state * 'tv_base_slots)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv174)
    | MenhirState466 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13614 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv176)
    | MenhirState463 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv177 * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv178)
    | MenhirState460 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv179 * Lexing.position * 'tv_opt_logical_or_trusted * Lexing.position) * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13627 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv180)
    | MenhirState453 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv181) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13635 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv182)
    | MenhirState447 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * Lexing.position * _menhir_state * 'tv_event_declarations)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv184)
    | MenhirState441 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv185 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13648 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv186)
    | MenhirState440 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv187 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13657 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv188)
    | MenhirState435 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv189) * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv190)
    | MenhirState434 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv191) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv192)
    | MenhirState428 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv193 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13674 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv194)
    | MenhirState427 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv195 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13683 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv196)
    | MenhirState420 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv197 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13692 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv198)
    | MenhirState419 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv199 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13701 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)
    | MenhirState418 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv201 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13710 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv202)
    | MenhirState417 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv203 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13719 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv204)
    | MenhirState415 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv205 * Lexing.position * Lexing.position) * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13728 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)
    | MenhirState410 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv207 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13737 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv208)
    | MenhirState409 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv209 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13746 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv210)
    | MenhirState407 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv211 * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv212)
    | MenhirState404 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv213 * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv214)
    | MenhirState401 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv215 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv216)
    | MenhirState399 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv217 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv218)
    | MenhirState395 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv219 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv220)
    | MenhirState393 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv221 * Lexing.position * _menhir_state * 'tv_layer_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv222)
    | MenhirState384 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv223 * _menhir_state * 'tv_layer_slots_plus) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv224)
    | MenhirState383 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv225 * _menhir_state * 'tv_layer_slots_plus) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)
    | MenhirState379 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv227 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 13793 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _, _menhir_s, _), _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv228)
    | MenhirState375 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv229 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 13802 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((((_menhir_stack, _, _menhir_s, _), _), _, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv230)
    | MenhirState369 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv231 * Lexing.position * _menhir_state * 'tv_object_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)
    | MenhirState363 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv233 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv234)
    | MenhirState362 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv235 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13821 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv236)
    | MenhirState360 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv237 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)
    | MenhirState359 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv239 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv240)
    | MenhirState358 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv241 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13840 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * 'tv_layer_signature_annotation)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv242)
    | MenhirState354 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv243 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_layer_signature) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)
    | MenhirState351 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv245 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv246)
    | MenhirState350 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv247 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv248)
    | MenhirState349 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv249) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv250)
    | MenhirState347 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv251 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13868 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv252)
    | MenhirState340 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv253 * _menhir_state * 'tv_layer_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv254)
    | MenhirState339 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv255 * _menhir_state * Lexing.position) * _menhir_state * 'tv_layer_signature_fields) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)
    | MenhirState337 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv257 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13886 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv258)
    | MenhirState334 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv259 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv260)
    | MenhirState333 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv261 * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13900 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv262)
    | MenhirState325 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv263 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _, _menhir_s, _, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv264)
    | MenhirState320 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv265 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _, _menhir_s, _, _), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv266)
    | MenhirState312 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv267 * _menhir_state * 'tv_idents_semi_sep) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)
    | MenhirState310 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv269 * Lexing.position * _menhir_state * 'tv_object_signature_expr * Lexing.position)) * Lexing.position * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _, _menhir_s, _, _), _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv270)
    | MenhirState302 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv271 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13928 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv272)
    | MenhirState301 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv273 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13937 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)
    | MenhirState300 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv275 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13946 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv276)
    | MenhirState299 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv277 * _menhir_state * 'tv_method_kind) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13955 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv278)
    | MenhirState295 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv279 * _menhir_state * 'tv_object_signature_fields) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)
    | MenhirState294 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv281 * _menhir_state * Lexing.position) * _menhir_state * 'tv_object_signature_fields) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv282)
    | MenhirState291 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv283 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv284)
    | MenhirState290 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv285 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)
    | MenhirState289 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv287 * _menhir_state)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv288)
    | MenhirState288 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv289 * _menhir_state)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv290)
    | MenhirState281 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv291 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv292)
    | MenhirState280 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv293 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 13999 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv294)
    | MenhirState274 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv295 * Lexing.position * _menhir_state * 'tv_constructor_declarations)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv296)
    | MenhirState270 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv297 * Lexing.position) * _menhir_state * 'tv_idents)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv298)
    | MenhirState269 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv299 * Lexing.position) * _menhir_state * 'tv_idents)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv300)
    | MenhirState267 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv301 * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv302)
    | MenhirState264 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv303 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14026 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * 'tv_constructor_params) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)
    | MenhirState262 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv305 * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv306)
    | MenhirState256 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv307 * _menhir_state * 'tv_field_declarations) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv308)
    | MenhirState255 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv309 * _menhir_state * Lexing.position) * _menhir_state * 'tv_field_declarations) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv310)
    | MenhirState254 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv311 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14050 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv312)
    | MenhirState253 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv313 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14059 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv314)
    | MenhirState251 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv315 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)
    | MenhirState250 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv317 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14073 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv318)
    | MenhirState248 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv319 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14082 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv320)
    | MenhirState247 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv321 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * Lexing.position * _menhir_state) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)
    | MenhirState246 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv323 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * Lexing.position * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv324)
    | MenhirState245 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv325 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv326)
    | MenhirState243 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv327 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)
    | MenhirState241 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv329 * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv330)
    | MenhirState240 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv331 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_type_expression * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv332)
    | MenhirState237 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv333 * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv334)
    | MenhirState235 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv335 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv336)
    | MenhirState232 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv337 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv338)
    | MenhirState231 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv339 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)
    | MenhirState230 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv341 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv342)
    | MenhirState228 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv343 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14146 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv344)
    | MenhirState219 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv345 * _menhir_state * 'tv_annotations_plus)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv346)
    | MenhirState207 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv347 * _menhir_state * 'tv_match_clauses)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv348)
    | MenhirState203 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv349 * _menhir_state * 'tv_match_pattern)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)
    | MenhirState195 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv351 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14170 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv352)
    | MenhirState192 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv353 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14179 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv354)
    | MenhirState189 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv355 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv356)
    | MenhirState185 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv357 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14193 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) * Lexing.position * _menhir_state * 'tv_commands)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv358)
    | MenhirState180 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv359 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14202 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv360)
    | MenhirState178 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((('freshtv361 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14211 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv362)
    | MenhirState176 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv363 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14220 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv364)
    | MenhirState172 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv365 * Lexing.position * _menhir_state * 'tv_command * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv366)
    | MenhirState164 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv368)
    | MenhirState162 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv369 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv370)
    | MenhirState161 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv371 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv372)
    | MenhirState160 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv373 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv374)
    | MenhirState158 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv375 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv376)
    | MenhirState156 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv377 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14259 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv378)
    | MenhirState154 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv379 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14268 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv380)
    | MenhirState152 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv381 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14277 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv382)
    | MenhirState151 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv383 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14286 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv384)
    | MenhirState149 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv385 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv386)
    | MenhirState148 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((((('freshtv387 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14300 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14304 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv388)
    | MenhirState146 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv389 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14313 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14317 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv390)
    | MenhirState145 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv391 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14326 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14330 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv392)
    | MenhirState142 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv393 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14339 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv394)
    | MenhirState140 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv395 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14348 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv396)
    | MenhirState139 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv397 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14357 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv398)
    | MenhirState137 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv399 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv400)
    | MenhirState136 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv401 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14371 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv402)
    | MenhirState134 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv403 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14380 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv404)
    | MenhirState132 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv405 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14389 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) * _menhir_state * 'tv_eq_or_assign) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv406)
    | MenhirState129 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv407 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_annotations) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14398 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv408)
    | MenhirState127 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv409 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv410)
    | MenhirState126 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv411 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14412 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv412)
    | MenhirState123 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv413 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv414)
    | MenhirState122 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv415 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv416)
    | MenhirState120 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv417 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv418)
    | MenhirState119 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv419 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14436 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _), _, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv420)
    | MenhirState116 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv421 * _menhir_state * Lexing.position) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv422)
    | MenhirState113 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv423 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) * _menhir_state * 'tv_opt_bar) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv424)
    | MenhirState111 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv425 * _menhir_state * Lexing.position) * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv426)
    | MenhirState109 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv427 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv428)
    | MenhirState107 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv429 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv430)
    | MenhirState98 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv431 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv432)
    | MenhirState92 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv433 * _menhir_state * 'tv_struct_fields) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv434)
    | MenhirState91 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv435 * _menhir_state * Lexing.position) * _menhir_state * 'tv_struct_fields) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv436)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv437 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv438)
    | MenhirState83 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv439 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv440)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv441 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv442)
    | MenhirState79 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv443 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv444)
    | MenhirState77 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv445 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv446)
    | MenhirState75 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv447 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv448)
    | MenhirState73 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv449 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv450)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv451 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv452)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv453 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv454)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv455 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv456)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv457 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv458)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv459 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _, _menhir_s, _, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv460)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv461 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv462)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv463 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv464)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv465 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv466)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv467 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv468)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv469 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv470)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv471 * Lexing.position * _menhir_state * 'tv_expression * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv472)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv473 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv474)
    | MenhirState47 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv475 * Lexing.position * _menhir_state * 'tv_atom * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv476)
    | MenhirState45 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv477 * Lexing.position * _menhir_state * 'tv_nonempty_list_atom_ * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv478)
    | MenhirState37 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv479 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv480)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv481 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv482)
    | MenhirState33 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv483 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14600 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv484)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv485 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv486)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv487 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv488)
    | MenhirState28 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv489 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv490)
    | MenhirState27 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv491 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv492)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv493 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv494)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv495 * Lexing.position * _menhir_state * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14634 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv496)
    | MenhirState11 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv497 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv498)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv499 * Lexing.position * _menhir_state * (
# 100 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14648 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv500)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv501 * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv502)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv503 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14662 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) * Lexing.position) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv504)

and _menhir_run25 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14669 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv129) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14680 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14684 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_atom = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 495 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
          (  mkexp_ ~loc:_sloc (PEconstant (CONuint (int_of_string(_1)))) )
# 14695 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv130)

and _menhir_run28 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState28 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28

and _menhir_run29 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | RPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv127 * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
        let (_menhir_s : _menhir_state) = MenhirState29 in
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
        
# 498 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                   (  mkexp_ ~loc:_sloc (PEconstant CONunit) )
# 14771 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv126)) : 'freshtv128)
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState29 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | IDENT _v ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState31 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run34 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14797 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv123) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14808 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14812 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_atom = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 494 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
         (  mkexp_ ~loc:_sloc (PEconstant (CONint (int_of_string(_1)))) )
# 14823 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv124)

and _menhir_run35 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14830 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
) -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _v _startpos ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv121) = Obj.magic _menhir_stack in
    let (_endpos__1_ : Lexing.position) = _endpos in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14841 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) : (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 14845 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
    )) = _v in
    let (_startpos__1_ : Lexing.position) = _startpos in
    ((let _startpos = _startpos__1_ in
    let _endpos = _endpos__1_ in
    let _v : 'tv_atom = let _endpos = _endpos__1_ in
    let _symbolstartpos = _startpos__1_ in
    let _sloc = (_symbolstartpos, _endpos) in
    
# 493 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
           ( let x = try Hashtbl.find constant_table _1 with Not_found -> PEglob _1 in  mkexp_ ~loc:_sloc (x) )
# 14856 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv122)

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState36 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_run37 : _menhir_env -> 'ttv_tail -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ADDRESS ->
        _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BANG ->
        _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | BITNOT ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | IDENT _v ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | INT _v ->
        _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LBRACE ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | LPAREN ->
        _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | MINUS ->
        _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState37 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | UINT _v ->
        _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState37 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState37

and _menhir_run38 : _menhir_env -> 'ttv_tail -> Lexing.position -> _menhir_state -> Lexing.position -> 'ttv_return =
  fun _menhir_env _menhir_stack _endpos _menhir_s _startpos ->
    let _menhir_stack = (_menhir_stack, _endpos, _menhir_s, _startpos) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAREN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv117 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
        ((let _menhir_stack = (_menhir_stack, _startpos) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | INT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv105 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14939 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv101 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14951 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv99 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14959 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14965 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__4_ in
                let _v : 'tv_atom = let _endpos = _endpos__4_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 496 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                              (  mkexp_ ~loc:_sloc (PEconstant (CONaddress _3)) )
# 14978 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv100)) : 'freshtv102)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv103 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 92 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14988 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)) : 'freshtv106)
        | UINT _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv113 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_v : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 14999 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | RPAREN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv109 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 15011 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv107 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 15019 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_endpos__4_ : Lexing.position) = _endpos in
                ((let (((_menhir_stack, _endpos__1_, _menhir_s, _startpos__1_), _startpos__2_), _endpos__3_, (_3 : (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 15025 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )), _startpos__3_) = _menhir_stack in
                let _4 = () in
                let _2 = () in
                let _1 = () in
                let _startpos = _startpos__1_ in
                let _endpos = _endpos__4_ in
                let _v : 'tv_atom = let _endpos = _endpos__4_ in
                let _symbolstartpos = _startpos__1_ in
                let _sloc = (_symbolstartpos, _endpos) in
                
# 497 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                               (  mkexp_ ~loc:_sloc (PEconstant (CONaddress _3)) )
# 15038 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                 in
                _menhir_goto_atom _menhir_env _menhir_stack _endpos _menhir_s _v _startpos) : 'freshtv108)) : 'freshtv110)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv111 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) * Lexing.position * (
# 93 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (string)
# 15048 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let (((_menhir_stack, _, _menhir_s, _), _), _, _, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)) : 'freshtv114)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv115 * Lexing.position * _menhir_state * Lexing.position) * Lexing.position) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _, _menhir_s, _), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)) : 'freshtv118)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv119 * Lexing.position * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)

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
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15088 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv5) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15100 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run38 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState453 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | BANG ->
                    _menhir_run37 _menhir_env (Obj.magic _menhir_stack) MenhirState453 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | BITNOT ->
                    _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState453 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run35 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState453 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | INT _v ->
                    _menhir_run34 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState453 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LBRACE ->
                    _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState453 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run29 _menhir_env (Obj.magic _menhir_stack) MenhirState453 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MINUS ->
                    _menhir_run28 _menhir_env (Obj.magic _menhir_stack) MenhirState453 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | UINT _v ->
                    _menhir_run25 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState453 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState453) : 'freshtv6)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv7) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15134 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
# 208 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 15153 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        ) = 
# 214 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
                      ( List.rev _1 )
# 15157 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
         in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv19) = _menhir_stack in
        let (_v : (
# 208 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 15164 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv17) = Obj.magic _menhir_stack in
        let (_v : (
# 208 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 15171 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv15) = Obj.magic _menhir_stack in
        let ((_1 : (
# 208 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 15178 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) : (
# 208 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 15182 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
        )) = _v in
        (Obj.magic _1 : 'freshtv16)) : 'freshtv18)) : 'freshtv20)) : 'freshtv22)) : 'freshtv24)
    | EVENT ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv25) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | BAR ->
            _menhir_run112 _menhir_env (Obj.magic _menhir_stack) MenhirState434
        | IDENT _ ->
            _menhir_reduce211 _menhir_env (Obj.magic _menhir_stack) MenhirState434
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState434) : 'freshtv26)
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
            let (_menhir_s : _menhir_state) = MenhirState407 in
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
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15224 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | COLON ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv27 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15236 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | ADDRESS ->
                        _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState427 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | ARRAY ->
                        _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState427 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | IDENT _v ->
                        _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState427 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LIST ->
                        _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState427 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LPAREN ->
                        _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState427 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | MAPPING ->
                        _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState427 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState427) : 'freshtv28)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv29 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15264 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState407 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
        | LET ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv41 * Lexing.position * Lexing.position) = Obj.magic _menhir_stack in
            let (_endpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_curr_p in
            let (_menhir_s : _menhir_state) = MenhirState407 in
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
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15294 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LBRACKET ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState415 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | COLON ->
                    _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState415
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState415) : 'freshtv38)
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
            let (_menhir_s : _menhir_state) = MenhirState407 in
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
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15332 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | LBRACKET ->
                    _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState409 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | ASSIGN | EQUAL ->
                    _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState409
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState409) : 'freshtv44)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv45 * Lexing.position * Lexing.position) * _menhir_state * Lexing.position) = Obj.magic _menhir_stack in
                ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv46)) : 'freshtv48)
        | WITH ->
            _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState407
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState407) : 'freshtv50)
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
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15375 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LBRACKET ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState347 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
            | COLON | EQUAL ->
                _menhir_reduce19 _menhir_env (Obj.magic _menhir_stack) MenhirState347
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState347) : 'freshtv52)
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
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15405 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                )) = _v in
                let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
                ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | EQUAL ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv53 * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15417 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                    ) * Lexing.position) = Obj.magic _menhir_stack in
                    ((let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | IDENT _v ->
                        _menhir_run345 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState333 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | LBRACE ->
                        _menhir_run334 _menhir_env (Obj.magic _menhir_stack) MenhirState333 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState333) : 'freshtv54)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : (('freshtv55 * Lexing.position) * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15437 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
# 827 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
             ( POlogical )
# 15468 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15486 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | EQUAL ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv71 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15498 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | IDENT _v ->
                    _menhir_run306 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState280 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LBRACE ->
                    _menhir_run281 _menhir_env (Obj.magic _menhir_stack) MenhirState280 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState280) : 'freshtv72)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv73 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15518 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
# 828 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
             ( POtrusted )
# 15543 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15561 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
            )) = _v in
            let (_startpos : Lexing.position) = _menhir_env._menhir_lexbuf.Lexing.lex_start_p in
            ((let _menhir_stack = (_menhir_stack, _endpos, _v, _startpos) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | ASSIGN ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : ('freshtv85 * Lexing.position) * Lexing.position * (
# 91 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
       (Astcommon.ident)
# 15573 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
                ) * Lexing.position) = Obj.magic _menhir_stack in
                let (_menhir_s : _menhir_state) = MenhirState4 in
                ((let _menhir_stack = (_menhir_stack, _menhir_s) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | ADDRESS ->
                    _menhir_run238 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | ARRAY ->
                    _menhir_run234 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | IDENT _v ->
                    _menhir_run233 _menhir_env (Obj.magic _menhir_stack) _menhir_env._menhir_lexbuf.Lexing.lex_curr_p MenhirState228 _v _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LIST ->
                    _menhir_run232 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | LPAREN ->
                    _menhir_run231 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | MAPPING ->
                    _menhir_run229 _menhir_env (Obj.magic _menhir_stack) MenhirState228 _menhir_env._menhir_lexbuf.Lexing.lex_start_p
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState228) : 'freshtv86)
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
# 826 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
             ( POnormal )
# 15618 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
# 208 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
     (Parsetree.p_file_structure)
# 15643 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
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
# 218 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
      ( [] )
# 15666 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
     in
    _menhir_goto_declarations _menhir_env _menhir_stack _endpos _v) : 'freshtv2)) : 'freshtv4))

# 838 "/Users/zachpage/Documents/deepsea/Edsger/parser.mly"
  

# 15673 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"

# 269 "<standard.mly>"
  

# 15678 "/Users/zachpage/Documents/deepsea/Edsger/parser.ml"
