(* OASIS_START *)
(* OASIS_STOP *)

let () = mark_tag_used "tests"

let () = dispatch begin function

  | Before_options ->
    Options.use_ocamlfind := true

  | After_rules as e ->
    flag ["doc"; "ocaml"; "extension:html"] &
      S[A"-charset"; A"utf8"; A"-t"; A"OCaml runtime for the Apache Thrift RPC system"];

    dep ["ocaml"; "compile"; "ppx_deriving_thrift"]
      ["ppx/ppx_deriving_thrift.cma"];
    flag ["ocaml"; "compile"; "ppx_deriving_thrift"] &
      S[A"-ppxopt"; A"ppx_deriving,ppx/ppx_deriving_thrift.cma"];

    dispatch_default e

  | e ->
    dispatch_default e

end
