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

let client_sig_of_module_type ~env pmtd =
  let loc = pmtd.pmtd_loc in
  let name = pmtd.pmtd_name in
  let lid = mkloc (Lident name.txt) name.loc in
  Sig.module_ ~loc (Md.mk ~loc name (Mty.ident lid))

let label_of_exception_type ptyp =
  match ptyp.ptyp_desc with
  | Ptyp_constr (lid, []) ->
    begin match lid.txt with
    | Lident label -> String.capitalize label
    | Ldot (Lident label, "t") -> label
    | _ -> raise_errorf "Expecting a simpler path to the error type."
    end
  | _ ->
    raise_errorf "Error type must be named."

let method_impl ~env = function
  | {psig_desc = Psig_value pval} ->
    let rec aux arg_fields = function
      | [%type: unit -> [%t? rt] io] ->
        let name = pval.pval_name in
        let atd_kind = Ptype_record (List.rev arg_fields) in
        let atd = Type.mk ~kind:atd_kind name in
        let get_field name = Exp.ident name in
        let arg_writer =
          List.fold_right (writer_expr_of_field ~env get_field) arg_fields
                          [%expr write_field_stop ()] in
        let res_reader =
          reader_expr_of_core_type ~env ~union_name:name.txt rt in
        let res_reader_callback =
          if is_exception_variant rt then
            [%expr fun is_exn -> [%e res_reader] ()]
          else
            [%expr fun is_exn ->
              read_struct_begin () >>= fun _ ->
              [%e res_reader] () >>= fun r ->
              read_struct_end () >|= fun () -> r
            ] in
        [%expr
          fun () ->
            call [%e ExpC.string name.txt]
                 (fun () -> [%e arg_writer]) [%e res_reader_callback]
        ]
      | {ptyp_desc = Ptyp_arrow (label, at, rt)} ->
        let attrs = at.ptyp_attributes in
        let at = {at with ptyp_attributes = []} in
        let arg_field = Type.field ~attrs (mknoloc label) at in
        let body = aux (arg_field :: arg_fields) rt in
        Exp.fun_ label None (Pat.var (mknoloc label)) body
      | _ ->
        raise_errorf "RPC functions must be reducible to unit -> 'a io." in
    [%stri let [%p Pat.var pval.pval_name] = [%e aux [] pval.pval_type]]
  | _ ->
    raise_errorf "Thrift interfaces may only contain values."

let client_str_of_module_type ~env pmtd =
  match pmtd.pmtd_type with
  | Some {pmty_desc = Pmty_signature psigs} ->
    let loc = pmtd.pmtd_loc in
    let me = Mod.structure (List.map (method_impl ~env) psigs) in
    Str.module_ ~loc (Mb.mk ~loc pmtd.pmtd_name me)
  | Some _ ->
    raise_errorf "Can only derive thrift client from simple signatures."
  | None ->
    raise_errorf "Cannot derive thrift client without a signature."
