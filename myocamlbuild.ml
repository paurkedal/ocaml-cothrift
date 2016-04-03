(* OASIS_START *)
(* OASIS_STOP *)

let () = dispatch begin function

  | Before_options ->
    Options.use_ocamlfind := true

  | After_rules as e ->
    flag ["doc"; "ocaml"; "extension:html"] &
      S[A"-charset"; A"utf8"; A"-t"; A"OCaml runtime for the Apache Thrift RPC system"];
    dispatch_default e

  | e ->
    dispatch_default e

end
