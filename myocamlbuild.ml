open Ocamlbuild_plugin

let ppx_deriving = lazy begin
  Filename.concat (String.trim (run_and_read "ocamlfind -query ppx_deriving"))
                  "ppx_deriving"
end

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
    end;
  rule "%.mli -> %_gen.mli (testing)"
    ~deps:["%.mli"; "ppx/ppx_deriving_thrift.cma"] ~prod:"%_gen.mli"
    begin fun env build ->
      let src = env "%.mli" and dst = env "%_gen.mli" in
      Cmd (S[
        A"ocamlfind"; A"ppx_tools/rewriter";
        A"-ppx"; A(Lazy.force ppx_deriving ^ " ppx/ppx_deriving_thrift.cma");
        A"-o"; Px dst; A"-intf"; P src
      ])
    end;
  rule "%.ml -> %_gen.ml (testing)"
    ~deps:["%.ml"; "ppx/ppx_deriving_thrift.cma"] ~prod:"%_gen.ml"
    begin fun env build ->
      let src = env "%.ml" and dst = env "%_gen.ml" in
      Cmd (S[
        A"ocamlfind"; A"ppx_tools/rewriter";
        A"-ppx"; A(Lazy.force ppx_deriving ^ " ppx/ppx_deriving_thrift.cma");
        A"-o"; Px dst; P src
      ])
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
