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

let writer_type_of_type_decl type_decl =
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: [%t var] -> unit io]) type_decl
    [%type: [%t typ] -> unit io]

let rec writer_expr_of_core_type ~env ptyp =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({txt = lid; loc}, ptyps) ->
    let f = Exp.ident (mkloc (mangle_lid "write" lid) loc) in
    let tagged_writer ptyp =
      let tag = tag_expr_of_core_type ~env ptyp in
      let w = writer_expr_of_core_type ~env ptyp in
      [tag; w] in
    app f (List.flatten (List.map tagged_writer ptyps))
  | _ ->
    raise_errorf ~loc:ptyp.ptyp_loc "Cannot derive thrift writer for %s."
                 (Ppx_deriving.string_of_core_type ptyp)

let writer_expr_of_type_decl ~env type_decl =
  match type_decl.ptype_kind, type_decl.ptype_manifest with
  | Ptype_abstract, Some ptyp ->
    writer_expr_of_core_type ~env ptyp
  | Ptype_record fields, _ ->
    let mk_writefield pld cont =
      let field_id = Attr.id ~loc:pld.pld_loc pld.pld_attributes in
      let field_name = mkloc (Lident pld.pld_name.txt) pld.pld_name.loc in
      let field_tag = tag_expr_of_core_type ~env pld.pld_type in
      let writefield _x =
        [%expr
          write_field_begin
            [%e Exp.constant (Const_string (pld.pld_name.txt, None))]
            [%e field_tag]
            [%e Exp.constant (Const_int field_id)] >>= fun () ->
          [%e writer_expr_of_core_type ~env (strip_option pld.pld_type)]
            [%e _x] >>= fun () ->
          write_field_end ()
        ] in
      if is_option pld.pld_type then
        [%expr
          begin match [%e Exp.field [%expr _r] field_name] with
          | None -> return ()
          | Some _x -> [%e writefield [%expr _x]]
          end >>= fun () ->
          [%e cont]
        ]
      else
        [%expr
          [%e writefield (Exp.field [%expr _r] field_name)] >>= fun () ->
          [%e cont]
        ] in
    let cont =
      List.fold_right mk_writefield fields
        [%expr write_field_stop () >>= fun () -> write_struct_end ()] in
    let name = type_decl.ptype_name in
    let name_expr = Exp.constant (Const_string (name.txt, None)) in
    let lid = mkloc (Lident name.txt) name.loc in
    [%expr
      fun (_r : [%t Typ.constr lid []]) ->
      write_struct_begin [%e name_expr] >>= fun () -> [%e cont]
    ]
  | _ ->
    raise_errorf ~loc:type_decl.ptype_loc
                 "Cannot derive thrift methods for this kind of type."
