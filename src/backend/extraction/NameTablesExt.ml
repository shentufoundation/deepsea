open AST

type ident_table = (ident, string) Hashtbl.t

type name_tables = {
  mutable funcs_tbl            : ident_table;
  mutable methods_tbl          : (BinNums.coq_Z, string) Hashtbl.t;
  mutable vars_tbl             : ident_table;
  mutable funcs_tmps_tbl       : (ident, ident_table) Hashtbl.t;
  mutable methods_tmps_tbl     : (BinNums.coq_Z, ident_table) Hashtbl.t;
  mutable constructor_tmps_tbl : ident_table;
}

let empty_name_tables = {
  funcs_tbl            = Hashtbl.create 0;
  methods_tbl          = Hashtbl.create 0;
  vars_tbl             = Hashtbl.create 0;
  funcs_tmps_tbl       = Hashtbl.create 0;
  methods_tmps_tbl     = Hashtbl.create 0;
  constructor_tmps_tbl = Hashtbl.create 0;
}

let copy_name_tables nt1 nt2 =
  nt1.funcs_tbl            <- nt2.funcs_tbl;
  nt1.methods_tbl          <- nt2.methods_tbl;
  nt1.vars_tbl             <- nt2.vars_tbl;
  nt1.funcs_tmps_tbl       <- nt2.funcs_tmps_tbl;
  nt1.methods_tmps_tbl     <- nt2.methods_tmps_tbl;
  nt1.constructor_tmps_tbl <- nt2.constructor_tmps_tbl;
