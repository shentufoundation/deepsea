open Astcommon
open Lexing

type location = {
  loc_start : Lexing.position;
  loc_end : Lexing.position;
}

val in_file : string -> location
val none : location


type p_expression = {
  p_expression_desc : p_expression_desc;
  p_expression_loc : location;
}
and p_expression_desc =
    PEglob of Astcommon.ident
  | PEconstant of Astcommon.constant
  | PEun of Astcommon.unop * p_expression
  | PEbin of Astcommon.binop * p_expression * p_expression
  | PEpair of p_expression * p_expression
  | PEapp of p_expression list
  | PEstruct of (Astcommon.ident * p_expression) list
  | PEfield of p_expression list * p_expression list
  | PEindex of p_expression * p_expression


type p_command = {
  p_command_desc : p_command_desc;
  p_command_loc : location;
}
and p_command_desc =
    PCyield of p_expression
  | PClet of Astcommon.ident * p_command * p_command
  | PCsequence of p_command * p_command
  | PCcond of p_expression * p_command * p_command option
  | PCfor of Astcommon.ident * p_expression * p_expression * p_command * p_annotations
        (* FOR i     = 0           TO n           DO s.run i *)
  | PCmatch of p_expression * p_clause list
  | PCemit of p_expression
  | PCtransfer of p_expression list

  (* extension for certified programming *)
  | PCstore of p_expression * p_expression
  | PCfail
  | PCassert of p_command
  | PCdeny of p_command
  | PCghost of p_command
  | PCfirst of Astcommon.ident * p_expression * p_expression * p_command *
        (* FIRST i     = 0           TO n           DO s.test i  *)
      p_command option * p_command option * p_annotations
             (* THEN i             ELSE FAIL *)

  | PCfold of Astcommon.ident * p_expression * p_expression *
        (* FOLD i     = 0           TO n            *)
      Astcommon.ident * p_expression * p_command * p_annotations
          (* | sum   = 0           DO sum + i      *)

and p_clause = Astcommon.ident * Astcommon.ident list * p_command

(* and p_annotation = 
  { p_annotation_desc: p_annotation_desc; p_annotation_loc: location;} *)
and p_annotation =
  | PAexpr of p_command
  | PAclause of ident * p_annotation list
  (*| PArecord of (ident * p_annotation) list*)

and p_annotations = p_annotation list

type p_pattern_tail =
    PPTcons of Astcommon.ident
  | PPTother of Astcommon.ident list

type p_type_FO = {
  p_type_FO_desc : p_type_FO_desc;
  p_type_FO_loc : location;
}

and p_type_FO_desc =
    PTname of Astcommon.ident
  | PTbuiltin of Astcommon.builtin_type
  | PTdata of p_type_data * p_annotations
  | PTprod of p_type_FO * p_type_FO
  | PTarray of int * p_type_FO
  | PTmapping of p_type_FO * p_type_FO
  | PTlist of p_type_FO

and p_type_data =
    PTsingleton of (Astcommon.ident * p_type_FO) list
  | PTbranches of p_type_constr list

and p_type_constr = {
  pTypeConstrName : Astcommon.ident;
  pTypeConstrArgs : (Astcommon.ident * p_type_FO) list;
  pTypeConstrAnnotations : p_annotations;
  pTypeConstrLoc : location;
}

type p_signature = {
  p_signature_desc : p_signature_desc;
  p_signature_loc : location;
}

and p_signature_desc =
    PSname of Astcommon.ident
  | PSconstr of
      (Astcommon.ident * p_type_FO * p_type_FO * Astcommon.method_kind) list
  | PSghostize of p_signature * Astcommon.ident list
  | PSlogicize of p_signature * Astcommon.ident list
  | PSminus of p_signature * Astcommon.ident list

type p_layer_signature = {
  p_layer_signature_desc : p_layer_signature_desc;
  p_layer_signature_loc : location;
}

and p_layer_signature_desc =
    PLSname of Astcommon.ident
  | PLSconstr of (Astcommon.ident * p_signature) list

type p_object_type = {
  pObjectBase : p_layer_signature;
  pObjectSignature : p_signature;
  pObjectTypLoc : location;
}

type p_layer_type = {
  pLayerBase : p_layer_signature;
  pLayerSignature : p_layer_signature;
  pLayerLoc : location;
}

type p_method_definition = {
  pMethodArguments : (Astcommon.ident * p_type_FO option) list;
  pMethodReturnType : p_type_FO option;
  pMethodKind : Astcommon.method_kind;
  pMethodBody : p_command;
  pMethodAnnotations : p_annotations;
  pMethodLoc : location;
}

type p_object_kind = POnormal | POlogical | POtrusted

type p_object_construct = {
  pObjType : p_object_type;
  pObjKind : p_object_kind;
  pObjFields : (Astcommon.ident * p_type_FO * p_expression * bool (* is_ghost *) ) list;
  pObjMethods : (Astcommon.ident * p_method_definition) list;
  pObjLoc : location;
}

type p_object = { p_object_desc : p_object_desc; p_object_loc : location; }

and p_object_desc =
    POname of Astcommon.ident
  | POclone of Astcommon.ident
  | POconstr of p_object_construct
  | POrelax of p_object * p_layer_signature

type p_object_definition = {
  pObjectType : p_object_type option;
  pObjectDesc : p_object;
  pObjectLoc : location;
}

type p_object_inst =
  {
    p_object_inst_desc: p_object_inst_desc;
    p_object_inst_loc: location
  }
and p_object_inst_desc = 
  | POinternal of p_object
  | POexternal of constant * p_object

type p_layer_construct = (Astcommon.ident * p_object_inst) list

type p_proposition = {
  p_proposition_desc : p_proposition_desc;
  p_proposition_loc : location;
}
and p_proposition_desc = PPident of Astcommon.ident | PPexternal of string

type p_layer = { p_layer_desc : p_layer_desc; p_layer_loc : location; }
and p_layer_desc =
    PLname of Astcommon.ident
  | PLconstr of p_layer_construct
  | PLrelax of p_layer * p_layer_type
  | PLinst of p_layer * p_layer
  | PLrefine of p_layer * p_layer * p_proposition

type p_invariant = Astcommon.ident * location

type p_layer_definition = {
  (* pLayerType : p_layer_type option; *)
  pLayerDesc : p_layer;
  pLayerInvariant : p_proposition option;
  pLayerAnnotations : p_annotations;
  pLayerLoc : location;
}

type p_declaration = {
  p_declaration_desc : p_declaration_desc;
  p_declaration_loc : location;
}
and p_declaration_desc =
    PDtype of p_type_FO
  | PDevent of (Astcommon.ident * p_type_FO * bool) list
  | PDsignature of p_signature
  | PDlayer_sig of p_layer_signature
  | PDobject of p_object_definition
  | PDlayer of p_layer_definition
  | PDexternal_with of string * p_annotations
  | PDexternal_type of string * string option * p_annotations
  | PDexternal_const of string * p_type_FO * p_annotations
  | PDexternal_function of string * p_type_FO * p_type_FO * p_annotations
  | PDexternal_prop of string * p_type_FO


type p_file_structure = (Astcommon.ident * p_declaration) list

(* Evaluating a constant value expression to an integer raising exception
   [Invalid_argument] for ill-typed or non-constant expressions.  Used by
   array type parsing. *)
(* TODO: Change [PTarray] to store an expression instead of an [int] to remove
         this (failing gracefully at typecheck instead of raising exception
         during parsing). *)
val int_of_p_expression : p_expression_desc -> int
val string_of_location : location -> string
val string_of_p_expression : p_expression -> string
val string_of_p_expression_desc : p_expression_desc -> string
val string_of_p_command : p_command -> Astcommon.ident
val string_of_p_command_desc : p_command_desc -> string
val string_of_p_clause_list : p_clause list -> string
val string_of_p_annotations : p_annotations -> string
val string_of_p_type_FO : p_type_FO -> string
val string_of_p_type_FO_desc : p_type_FO_desc -> string
val string_of_p_type_constr : p_type_constr -> string
val string_of_p_type_data : p_type_data -> string
val string_of_p_signature_aux : string -> p_signature -> string
val string_of_p_signature_aux_desc : string -> p_signature_desc -> Astcommon.ident
val string_of_p_signature : p_signature -> string
val string_of_p_layer_signature : p_layer_signature -> string
val string_of_p_layer_signature_desc : p_layer_signature_desc -> Astcommon.ident
val string_of_p_object_type : p_object_type -> string
val string_of_p_layer_type : p_layer_type -> string
val string_of_pMethodArguments : (string * p_type_FO option) list -> string
val string_of_p_object_kind : p_object_kind -> string
val string_of_p_object : p_object -> string
val string_of_p_object_desc : p_object_desc -> string
val string_of_p_object_definition : p_object_definition -> string
val string_of_p_proposition : p_proposition -> string
val string_of_p_proposition_desc : p_proposition_desc -> Astcommon.ident
val string_of_p_layer : p_layer -> string
val string_of_p_layer_desc : p_layer_desc -> string
val string_of_p_layer_definition : p_layer_definition -> string
