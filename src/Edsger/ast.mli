open Astcommon

type a_ctype =
  | ACtint
  | ACtchar
  | ACtvoid
  | ACtpointer of a_ctype
  | ACtarray of int * a_ctype
  | ACtmapping of a_ctype * a_ctype			
  | ACtstruct of ident * (ident * a_ctype) list

type a_cimpl = {
  aImplDesc : a_cimpl_desc;
  aImplType : a_ctype
}

and a_cimpl_desc =
  | ACdefault
  | ACint of int
  | ACvar of ident
  | ACplus of a_cimpl * a_cimpl  (* or binop? *)
  | ACtimes of a_cimpl * a_cimpl  (* or binop? *)
  | ACcond of a_cimpl * a_cimpl * a_cimpl
  | ACarray of a_cimpl  (* do we really need this? *)
  | ACstruct of (ident * a_cimpl) list

type a_external_type = {
  aEXTypeName : ident;

  aEXTypeString : string;
  aEXTypeLowLevelInv : string option;
  mutable aEXTypeDefault : (string * ident) option  (* String & Name *)
}

type a_type = {
  aTypeDesc : a_type_desc;
  aTypeCtype : a_ctype;
  aTypePairIdent : ident;
  aTypeProvable : bool
}

and a_type_desc =
  | ATbuiltin of builtin_type
  | ATdata of ident * a_type_data
  | ATprod of a_type * a_type
  | ATarray of int * a_type
  | ATmapping of a_type * a_type
  | ATlist of a_type
  | ATexternal of a_external_type

and a_type_data =
  | ATsingleton of a_type_constr
  | ATbranches of a_type_constr list

and a_type_constr = {
  aTypeConstrName : ident;
  aTypeConstrArgs : (ident * a_type) list;
  aTypeConstrImpl : a_cimpl option
}

type a_event_type = {
  aEventName : ident;
  aEventArgs : (ident * a_type * bool) list;
}

type a_method_type = {
  aMethodArgumentTypes : a_type list;
  aMethodReturnType : a_type;
  aMethodKind : method_kind
}

type a_signature = {
  aSignatureName : ident;
  aSignatureMethods : (ident * a_method_type) list;
  aSignatureSuppressError : bool
}

type a_layer_signature = {
  aLayerSignatureName : ident;
  aLayerSignatureSlots : (ident * a_signature) list;
  aLayerSignatureSuppressError : bool
}

type a_object_type = {
  aObjectBase : a_layer_signature;
  aObjectSignature : a_signature
}

type a_layer_type = {
  aLayerBase : a_layer_signature;
  aLayerSignature : a_layer_signature
}

type tmp_id_t = int

type a_rexpr = {
  aRexprDesc : a_rexpr_desc;
  aRexprType : a_type
}

and a_rexpr_desc =
  | AEconst of constant
  | AEconstr_val of a_type_constr * (ident * a_rexpr) list
  | AEtemp of tmp_id_t * ident
  | AEunop of unop * a_rexpr
  | AEbinop of binop * a_rexpr * a_rexpr
  (* builtins in the EVM backend: *)
  | AEbuiltin of string * a_rexpr list

type a_lexpr = {
  aLexprDesc : a_lexpr_desc;
  aLexprType : a_type;

  aLexprIsGhost : bool
}

and a_lexpr_desc =
  | AEglob of ident
  | AEfield of a_lexpr * ident
  | AEindex of a_lexpr * a_rexpr

type a_big_expr = {
  aBigExprDesc : a_big_expr_desc;
  aBigExprType : a_type
}

and a_big_expr_desc =
  | AErexpr of a_rexpr
  | AEstruct of a_type_constr * (ident * a_big_expr) list
  | AEconstr of a_type_constr * (ident * a_big_expr) list
  | AEexternal_const of string * ident
  (*| AEexternal_call of string * ident * a_big_expr list*)

type a_matchable = {
  aMatchableDesc : a_matchable_desc;
  aMatchableType : a_type
}

and a_matchable_desc =
  | AMtemp of tmp_id_t * ident
  | AMlexpr of a_lexpr

type a_external_call_arg =
  | AEXrexer of a_rexpr
  | AEXlexpr of a_lexpr

type a_compile_time_constant =
  | ACONconst of constant
  | ACONstruct of (ident * a_compile_time_constant) list
  | ACONconstr of a_type_constr * (ident * a_compile_time_constant) list
  | ACONexternal_const of string * ident
  (*| ACONexternal_call of string * ident * a_compile_time_constant list*)

type a_external_const = {
  aExtConstName : string;
  aExtConstUnfolding : bool;
  aExtConstPartial : bool
}

type a_object_field = {
  aObjectFieldName : ident;
  aObjectFieldType : a_type;
  aObjectFieldInitial : a_compile_time_constant;
  aObjectFieldIsLogical : bool  (* aka aObjectFieldNoImplementation *)
}

type tmp_env_t = (ident * (tmp_id_t * a_type)) list

type side_effect_t = {
  affectsAbstraction : bool;
  affectsImplementation : bool;
  dependsOnAbstraction : bool;
  invokesLogical : bool
}

type a_captured_command = {
  aCapturedName : ident;
  aCapturedTemp : tmp_env_t
}

type a_command = {
  aCmdDesc : a_command_desc;
  aCmdType : a_type;
  aCmdEnv : tmp_env_t;

  aCmdEffect : side_effect_t
}

and a_command_desc =
  | ACskip
  | ACyield of a_rexpr
  | AClet of tmp_id_t * ident * a_command * a_command
  | ACsequence of a_command * a_command
  | ACcall of ident * ident * a_rexpr list * a_rexpr option * a_rexpr option
  | ACtransfer of a_rexpr * a_rexpr
  | ACcond of a_rexpr * a_command * a_command
  | ACfor of tmp_id_t * ident * a_rexpr * tmp_id_t * a_rexpr * a_command
      (* FOR (*NUM*)    i     = 0      TO (*HOLDER*) n      DO s.run i *)
           * a_captured_command option
  | ACmatch of a_matchable * a_clause list
  | ACemit of a_event_type * a_rexpr list

  (* extension for certified programming *)
  | ACload of a_lexpr
  | ACstore of a_lexpr * a_rexpr
  | ACconstr of a_lexpr * a_big_expr
  | ACfail
  | ACassert of a_command
  | ACdeny of a_command
  | ACghost of a_command
  | ACfirst of tmp_id_t * ident * a_rexpr * tmp_id_t * a_rexpr *
      (* FIRST (*NUM*)    i     = 0      TO (*HOLDER*) n       *)
               tmp_id_t * a_command * a_command * a_command
         (* DO (*HOLDER*) s.test i THEN i    ELSE FAIL *)
             * a_captured_command option
  | ACfold of tmp_id_t * ident * a_rexpr * tmp_id_t * a_rexpr *
      (* FOLD (*NUM*)    i     = 0      TO (*HOLDER*) n       *)
              tmp_id_t * ident * a_rexpr * tmp_id_t * a_command
         (* | (*NUM*)    sum   = 0      DO (*HOLDER*) sum + i *)
            * a_captured_command option
  | ACexternal of a_lexpr option * string * ident * a_external_call_arg list

and a_clause = a_type_constr option * tmp_env_t * a_command

(* Following two types are mCertiKOS specific for low level integration
   with CompCert *)
type a_method_semantics =
  | ASdefault
  | ASsetCR3
  | ASassembly
  | ASassembly'  (* Actually defined in assembly? Used by [pt_in] and [pt_out] *)
  | AStrap_info_get
  | AStrap_info_ret

type a_accessor =
  | AAnone
  | AALoadStoreSem1
  | AALoadStoreSem2
  | AALoadStoreSem3

type a_proposition = string

type a_method_definition = {
  aMethodName : ident;
  aMethodArguments : ident list;
  aMethodType : a_method_type;
  aMethodSemantics : a_method_semantics;
  aMethodBody : a_command;
  aMethodParamEnv : tmp_env_t
}

type a_object = {
  aObjectName : ident;
  aObjectAddress : string option;
  (*aObjectSerial : int;*)
  aObjectCoreType : a_object_type;
  aObjectType : a_object_type;
  aObjectRequireImpl : bool;
  aObjectIsTrusted : bool;
  aObjectFields : a_object_field list;
  aObjectMethods : a_method_definition list;

  aObjectProcedures : a_method_definition list;
  aObjectFunctions : a_method_definition list
}

module IdentSet : Set.S with type elt := ident

type a_checked_layer = {
  aCheckedLayerName : ident;
  (*aCheckedLayerSerial : int;*)
  (*aCheckedLayerCoreType : a_layer_type;*)
  aCheckedLayerType : a_layer_type;
  aCheckedLayerAccessor : a_accessor;
  aCheckedLayerInvariant : a_proposition option;

  aCheckedLayerPassthroughs : a_layer_signature;

  aCheckedLayerTipObjectSet : IdentSet.t;  (* object names *)
  aCheckedLayerKeelObjectSet : IdentSet.t; (* object names *)

  aCheckedLayerDesc : a_checked_layer_desc
}

and a_checked_layer_desc =
  | ALconstr of (ident * a_object) list  (* slot id -> object *)
  | ALrelax of a_checked_layer
  | ALinst of a_checked_layer * a_checked_layer
  | ALrefine of a_checked_layer * a_checked_layer * a_proposition

type a_layer = {
  aLayerName : ident;
  (*aLayerSerial : int;*)
  (*aLayerCoreType : a_layer_type;*)
  aLayerType : a_layer_type;
  aLayerAccessor : a_accessor;
  aLayerInvariant : a_proposition option;

  aLayerFreshObjects : (ident * a_object) list;  (* slot id -> object *)
  aLayerPassthroughObjects : (ident * a_object) list;
  aLayerAllObjects : (ident * a_object) list;

  aLayerTipObjectSet : IdentSet.t;  (* object names *)
  aLayerKeelObjectSet : IdentSet.t; (* object names *)

  aLayerDesc : a_layer_desc
}

and a_layer_desc =
  | ALbottom
  | ALontop of a_layer                   (* { s |-> w } : I[I] @ L *)
  | ALrefine_bottom of a_layer * a_proposition
                                         (* { s |-> w }        :> L with p *)
  (*
  | ALrefine_ontop of a_layer * a_layer  (* ({ s |-> w } @ L1) :> L2 *)
  *)

type a_declaration =
  | ADtype of a_type
  | ADevent of a_event_type
  (*
  | ADobject of a_object
  *)
  | ADlayer of a_layer
  (*
  | ADexternal_with of string * string list
  *)

type a_file = {
  aFileDeclarations : (ident * a_declaration) list;
  aFileGlobalAbstractDataType : a_external_type option;
  aFileExternalVerbatim : (string * string list) list;
  aFileExternalSymbols : a_external_const list
}

(* Behaviors of these three match [Coqlib.align], [Ctypes.alignof], and
   [Ctypes.sizeof] in CompCert, respectively *)
val calign : int -> int -> int
val calignof : a_ctype -> int
val csizeof : a_ctype -> int

val a_type_desc_to_ident : a_type_desc -> ident
val a_type_contains_raw_type : a_type -> bool

val string_of_a_ctype : a_ctype -> string
val string_of_a_type : bool -> a_type -> string
val string_of_tmp_env : tmp_env_t -> string
val string_of_method_arg_types : a_type list -> string
val string_of_a_matchable : a_matchable -> string

val string_of_a_signature : a_signature -> string
val string_of_a_layer_signature : a_layer_signature -> string
val string_of_a_object_type : a_object_type -> string
val string_of_a_layer_type : a_layer_type -> string

val string_of_a_object : a_object -> string
val string_of_a_layer : a_layer -> string

val string_of_a_checked_layer : a_checked_layer -> string
val string_of_a_layer_desc : a_layer_desc -> string
