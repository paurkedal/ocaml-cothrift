(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Ast_helper
open Ast_convenience
open Asttypes
open Location
open Longident
open Parsetree
open Ppx_thrift_helper
open Ppx_thrift_read
open Ppx_thrift_write

let processor_sig_of_module_type ~env pmtd =
  let loc = pmtd.pmtd_loc in
  let name = pmtd.pmtd_name in
  let funct_name = mkloc (name.txt ^ "_processor") name.loc in
  let arg_mty = Mty.ident (mkloc (Lident name.txt) name.loc) in
  let result_mty =
    Mty.with_
      (Mty.ident (mkloc (Ldot (Lident "Thrift_sig", "Processor")) loc))
      [Pwith_typesubst
        (Type.mk ~params:[[%type: 'a], Invariant] ~manifest:[%type: 'a io]
          (mknoloc "io"))] in
  let funct_mty = Mty.functor_ ~loc (mknoloc "X") (Some arg_mty) result_mty in
  Sig.module_ ~loc (Md.mk ~loc funct_name funct_mty)

let extract_arguments t =
  let rec loop arg_fields = function
    | [%type: unit -> [%t? rt] io] ->
      (arg_fields, rt)
    | {ptyp_desc = Ptyp_arrow (label, at, rt)} ->
      let attrs = at.ptyp_attributes in
      let arg_field = Type.field ~attrs (mknoloc label) at in
      loop (arg_field :: arg_fields) rt
    | _ ->
      raise_errorf "RPC functions must be reducible to unit -> 'a io." in
  loop [] t

let handler_of_signature_item ~env psig =
  match psig.psig_desc with

  | Psig_value pval ->
    let name = pval.pval_name in
    let common_handler arg_fields cont =
      let mk_arg pld =
        let arg_lid = mkloc (Lident pld.pld_name.txt) pld.pld_name.loc in
        (pld.pld_name.txt, Exp.ident arg_lid) in
      let func_name = mkloc (Ldot (Lident "X", name.txt)) name.loc in
      let struct_name = name.txt ^ "_args" in
      let call = Exp.apply (Exp.ident func_name) (List.map mk_arg arg_fields) in
      checked_reader_expr_of_record ~env ~struct_name arg_fields (cont call) in

    let arg_fields, result_type = extract_arguments pval.pval_type in
    let name_expr = ExpC.string ~loc:name.loc name.txt in
    begin match result_type with
    | [%type: unit] ->
      let is_oneway = false in (* TODO: Use it to complain. *)
      let handler = common_handler arg_fields (fun call -> call) in
      [%expr ([%e name_expr],
              Handler_unit ([%e ExpC.bool is_oneway], [%e handler]))]
    | _ ->
      let union_name = name.txt ^ "_result" in
      let result_writer =
        writer_expr_of_core_type ~env ~union_name result_type in
      let handler =
        if is_exception_variant result_type then
          let cont call =
            [%expr
              [%e call] () >>= fun r ->
              let is_exn = match r with `Ok _ -> false | _ -> true in
              return (is_exn, (fun () -> [%e result_writer] r))
            ] in
          common_handler arg_fields cont
        else
          let cont call =
            [%expr
              [%e call] () >>= fun r ->
              let write_result () =
                write_struct_begin [%e ExpC.string union_name] >>= fun () ->
                [%e result_writer] r >>= fun () ->
                write_struct_end () in
              return (false, write_result)
            ] in
          common_handler arg_fields cont in
      [%expr ([%e name_expr], Handler [%e handler])]
    end

  | _ ->
    raise_errorf "Thrift interfaces may only contain values."

let partial_arg_type_of_signature_item ~env psig =
  match psig.psig_desc with
  | Psig_value pval ->
    let arg_fields, result_type = extract_arguments pval.pval_type in
    let name = mkloc ("partial_" ^ pval.pval_name.txt ^ "_args")
                     pval.pval_name.loc in
    Str.type_ [partial_type_of_record name arg_fields]
  | _ ->
    raise_errorf "Thrift interfaces may only contain values."

let processor_str_of_module_type ~env pmtd =
  let loc = pmtd.pmtd_loc in
  match pmtd.pmtd_type with
  | Some {pmty_desc = Pmty_signature psigs} ->
    let name = pmtd.pmtd_name in
    let funct_name = mkloc (name.txt ^ "_processor") name.loc in
    let arg_mty = Mty.ident (mkloc (Lident name.txt) name.loc) in
    let result_mod =
      List.map (partial_arg_type_of_signature_item ~env) psigs @
      [%str
        type handler =
          | Handler of (unit -> (bool * (unit -> unit Io.io)) Io.io)
          | Handler_unit of bool * (unit -> unit Io.io)
        let handlers = Hashtbl.create 11
        let () = Array.iter (fun (k, v) -> Hashtbl.add handlers k v)
          [%e Exp.array (List.map (handler_of_signature_item ~env) psigs)]
      ] in
    let funct_mod =
      Mod.functor_ (mknoloc "X") (Some arg_mty) (Mod.structure result_mod) in
    Str.module_ ~loc (Mb.mk ~loc funct_name funct_mod)
  | Some _ ->
    raise_errorf "Can only derive thrift client from simple signatures."
  | None ->
    raise_errorf "Cannot derive thrift client without a signature."
