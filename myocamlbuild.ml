open Ocamlbuild_plugin

let () =
  rule "pkg/META -> lib/META"
    ~dep:"pkg/META" ~prod:"lib/META"
    begin fun env build ->
      Cmd (S[A"sed"; A"/^\\s*requires =/ s/\\<cothrift\\>/lib/g";
             P"pkg/META"; Sh">"; Px"lib/META"])
    end;
  rule "%.mli & %.idem -> %.ml"
    ~deps:["%.mli"; "%.idem"] ~prod:"%.ml"
    begin fun env build ->
      let src = env "%.mli" and dst = env "%.ml" in
      cp src dst
    end

let () = dispatch begin function

  | Before_options ->
    Options.use_ocamlfind := true

  | After_rules ->
    flag ["doc"; "ocaml"; "extension:html"] &
      S[A"-charset"; A"utf8"; A"-t"; A"OCaml runtime for the Apache Thrift RPC system"];

    dep ["ocaml"; "compile"; "ppx_deriving_thrift"]
      ["ppx/ppx_deriving_thrift.cma"];
    flag ["ocaml"; "compile"; "ppx_deriving_thrift"] &
      S[A"-ppxopt"; A"ppx_deriving,ppx/ppx_deriving_thrift.cma"];

  | _ -> ()

end
