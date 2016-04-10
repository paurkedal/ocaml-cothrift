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

let sig_of_type ~options ~path type_decl =
  let () = parse_options options in
  [Sig.value (Val.mk (mknoloc (mangle_type_decl "read" type_decl))
                     (reader_type_of_type_decl type_decl));
   Sig.value (Val.mk (mknoloc (mangle_type_decl "write" type_decl))
                     (writer_type_of_type_decl type_decl))]

let str_of_type ~env ~options ~path type_decl =
  let () = parse_options options in
  let reader_type = reader_type_of_type_decl type_decl in
  let reader_expr = reader_expr_of_type_decl ~env type_decl in
  let reader_var = pvar (mangle_type_decl "read" type_decl) in
  let writer_type = writer_type_of_type_decl type_decl in
  let writer_expr = writer_expr_of_type_decl ~env type_decl in
  let writer_var = pvar (mangle_type_decl "write" type_decl) in
  [Vb.mk (Pat.constraint_ reader_var reader_type)
         (Ppx_deriving.poly_fun_of_type_decl type_decl reader_expr);
   Vb.mk (Pat.constraint_ writer_var writer_type)
         (Ppx_deriving.poly_fun_of_type_decl type_decl writer_expr)]

let type_decl_str ~env ~options ~path type_decls =
  List.iter
    (fun type_decl -> Hashtbl.add env type_decl.ptype_name.txt type_decl)
    type_decls;
  match List.concat (List.map partial_type_of_type_decl type_decls),
        List.concat (List.map (str_of_type ~env ~options ~path) type_decls) with
  | [], [] -> []
  | [], vbs -> [Str.value Nonrecursive vbs]
  | tds, [] -> [Str.type_ tds]
  | tds, vbs -> [Str.type_ tds; Str.value Nonrecursive vbs]

let () =
  let env = Hashtbl.create 31 in
  Ppx_deriving.register @@
  Ppx_deriving.create deriver
    ~type_decl_sig: (fun ~options ~path type_decls ->
      List.concat (List.map (sig_of_type ~options ~path) type_decls))
    ~type_decl_str: (type_decl_str ~env) ()
