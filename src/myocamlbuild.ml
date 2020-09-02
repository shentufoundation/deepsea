let () =
  Ocamlbuild_plugin.dispatch
    (fun hook ->
      Ocamlbuild_cppo.dispatcher hook ;
    )
