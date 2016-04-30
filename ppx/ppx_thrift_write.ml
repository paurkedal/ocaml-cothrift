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

let rec writer_expr_of_core_type ~env ?union_name ptyp =
  let loc = ptyp.ptyp_loc in
  match ptyp.ptyp_desc with
  | Ptyp_constr ({txt = lid; loc}, ptyps) ->
    let f = Exp.ident (mkloc (mangle_io_lid ~env ~loc "write" lid) loc) in
    let tagged_writer ptyp =
      let tag = tag_expr_of_core_type ~env ptyp in
      let w = writer_expr_of_core_type ~env ptyp in
      [tag; w] in
    app f (List.flatten (List.map tagged_writer ptyps))
  | Ptyp_variant (rows, Closed, None) ->
    let union_name =
      match union_name with
      | None -> raise_errorf ~loc "Cannot create writer for anonymous variant."
      | Some name -> name in
    let mk_field_case = function
      | Rtag ("Ok", [], _, [arg_type]) ->
        let writer = writer_expr_of_core_type ~env arg_type in
        Exp.case [%pat? `Ok _x]
          [%expr
            write_union_begin
              [%e ExpC.string union_name] "success"
              [%e tag_expr_of_core_type ~env arg_type] 0 >>=
            fun () -> [%e writer] _x
          ]
      | Rtag (row_label, row_attributes, _, [arg_type]) ->
        let field_id = Attr.id ~loc row_attributes in
        let writer = writer_expr_of_core_type ~env arg_type in
        Exp.case (Pat.variant row_label (Some [%pat? _x]))
          [%expr
            write_union_begin
              [%e ExpC.string union_name] [%e ExpC.string row_label]
              [%e tag_expr_of_core_type ~env arg_type]
              [%e ExpC.int field_id] >>=
            fun () -> [%e writer] _x
          ]
      | Rtag (_, _, _, _) ->
        raise_errorf ~loc "Thrift union constructors must take a single \
                           argument."
      | Rinherit t ->
        raise_errorf ~loc "Only expanded variant types supported" in
    [%expr
      fun v ->
        [%e Exp.match_ [%expr v] (List.map mk_field_case rows)] >>=
        write_union_end
    ]
  | _ ->
    raise_errorf ~loc:ptyp.ptyp_loc "Cannot derive thrift writer for %s."
                 (Ppx_deriving.string_of_core_type ptyp)

let writer_expr_of_field ~env get_field pld cont =
  let field_id = Attr.id ~loc:pld.pld_loc pld.pld_attributes in
  let field_name = mkloc (Lident pld.pld_name.txt) pld.pld_name.loc in
  let field_tag = tag_expr_of_core_type ~env pld.pld_type in
  let writefield _x =
    [%expr
      write_field_begin
        [%e ExpC.string pld.pld_name.txt]
        [%e field_tag]
        [%e ExpC.int field_id] >>= fun () ->
      [%e writer_expr_of_core_type ~env (strip_option pld.pld_type)]
        [%e _x] >>= fun () ->
      write_field_end ()
    ] in
  if is_option pld.pld_type then
    [%expr
      begin match [%e get_field field_name] with
      | None -> return ()
      | Some _x -> [%e writefield [%expr _x]]
      end >>= fun () ->
      [%e cont]
    ]
  else
    [%expr
      [%e writefield (get_field field_name)] >>= fun () ->
      [%e cont]
    ]

let writer_expr_of_type_decl ~env type_decl =
  let name = type_decl.ptype_name in
  match type_decl.ptype_kind, type_decl.ptype_manifest with
  | Ptype_abstract, Some ptyp ->
    writer_expr_of_core_type ~env ptyp
  | Ptype_record fields, _ ->
    let get_field field_name = Exp.field [%expr _r] field_name in
    let cont =
      List.fold_right (writer_expr_of_field ~env get_field) fields
        [%expr write_field_stop () >>= fun () -> write_struct_end ()] in
    let name_lid = mkloc (Lident name.txt) name.loc in
    [%expr
      fun (_r : [%t Typ.constr name_lid []]) ->
      write_struct_begin [%e ExpC.string name.txt] >>= fun () -> [%e cont]
    ]
  | Ptype_variant pcds, _ ->
    let mk_case pcd =
      let field_type =
        match pcd.pcd_args with
        | [t] -> t
        | _ -> raise_errorf ~loc:pcd.pcd_loc
                "Constructors for thrift unions takes a single argument." in
      let c = mkloc (Lident pcd.pcd_name.txt) pcd.pcd_name.loc in
      let field_tag = tag_expr_of_core_type ~env field_type in
      let field_id = Attr.id ~loc:pcd.pcd_loc pcd.pcd_attributes in
      Exp.case (Pat.construct c (Some [%pat? _x]))
        [%expr
          write_union_begin
            [%e ExpC.string name.txt] [%e ExpC.string pcd.pcd_name.txt]
            [%e field_tag] [%e ExpC.int field_id] >>= fun () ->
          [%e writer_expr_of_core_type ~env field_type] _x
        ] in
    [%expr
      fun (_v : [%t Typ.constr (mkloc (Lident name.txt) name.loc) []]) ->
      [%e Exp.match_ [%expr _v] (List.map mk_case pcds)] >>= write_union_end]
  | _ ->
    raise_errorf ~loc:type_decl.ptype_loc
                 "Cannot derive thrift methods for this kind of type."
