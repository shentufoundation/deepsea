open StmtClinear
open TempModelLow

(** val ftype : ftemps **)

let ftype =
  { f_temps = (Obj.magic StmtClinear.fn_temps); f_args =
    (Obj.magic StmtClinear.fn_params) }
