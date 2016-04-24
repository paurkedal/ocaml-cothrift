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
open Printf

let reader_type_of_type_decl type_decl =
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  Ppx_deriving.poly_arrow_of_type_decl
    (fun var -> [%type: unit -> [%t var] io]) type_decl
    [%type: unit -> [%t typ] io]

let partial_field_ident pld =
  mkloc (Lident ("p_" ^ pld.pld_name.txt)) pld.pld_name.loc

let partial_type_of_type_decl type_decl =
  match type_decl.ptype_kind with
  | Ptype_record fields ->
    let to_raw pld =
      let pld_type =
        if Attr.default pld.pld_attributes = None then
          match pld.pld_type.ptyp_desc with
          | Ptyp_constr ({txt = Lident "option"}, _) -> pld.pld_type
          | _ -> [%type: [%t pld.pld_type] option]
        else
          pld.pld_type in
      { pld_name = mkloc ("p_" ^ pld.pld_name.txt) pld.pld_name.loc;
        pld_mutable = Mutable;
        pld_type;
        pld_loc = pld.pld_loc;
        pld_attributes = []; } in
    [Type.mk ~kind:(Ptype_record (List.map to_raw fields))
             (mkloc ("partial_" ^ type_decl.ptype_name.txt)
                    type_decl.ptype_name.loc)]
  | _ -> []

let rec reader_expr_of_core_type ~env ptyp =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({txt = lid; loc}, ptyps) ->
    let f = Exp.ident (mkloc (mangle_io_lid ~loc "read" lid) loc) in
    let tagged_reader ptyp =
      let tag = tag_expr_of_core_type ~env ptyp in
      let r = reader_expr_of_core_type ~env ptyp in
      [tag; r] in
    app f (List.flatten (List.map tagged_reader ptyps))
  | _ ->
    raise_errorf ~loc:ptyp.ptyp_loc "Cannot derive thrift for %s."
                 (Ppx_deriving.string_of_core_type ptyp)

let reader_expr_of_type_decl ~env type_decl =
  match type_decl.ptype_kind, type_decl.ptype_manifest with
  | Ptype_abstract, Some ptyp ->
    reader_expr_of_core_type ~env ptyp
  | Ptype_record fields, _ ->
    let _r = [%expr _r] in
    let mk_field pld =
      let name = mkloc (Lident ("p_" ^ pld.pld_name.txt)) pld.pld_name.loc in
      match Attr.default pld.pld_attributes with
      | None -> (name, [%expr None])
      | Some e -> (name, e) in
    let mk_setfield pld =
      let field_id = Attr.id ~loc:pld.pld_loc pld.pld_attributes in
      let msg = sprintf "Received wrong field type for %s.%s"
                        type_decl.ptype_name.txt pld.pld_name.txt in
      let msg_expr = Exp.constant (Const_string (msg, None)) in
      let reader_expr =
        reader_expr_of_core_type ~env (strip_option pld.pld_type) in
      let is_opt = Attr.default pld.pld_attributes = None in
      Exp.case
        (Pat.constant (Const_int field_id))
        [%expr
          if field_type <> [%e tag_expr_of_core_type ~env pld.pld_type] then
            fail (Protocol_error (Invalid_data, [%e msg_expr]))
          else begin
            [%e reader_expr] () >>= fun _x ->
            [%e Exp.setfield _r (partial_field_ident pld)
                             (if is_opt then [%expr Some _x] else [%expr _x])];
            _loop ()
          end
        ] in
    let mk_retcheck pld cont =
      let name = mkloc (Lident pld.pld_name.txt) pld.pld_name.loc in
      let _x = Exp.field _r (partial_field_ident pld) in
      match Attr.default pld.pld_attributes, pld.pld_type.ptyp_desc with
      | Some _, _ | _, Ptyp_constr ({txt = Lident "option"}, _) ->
        [%expr
          let [%p Pat.var pld.pld_name] = [%e _x] in
          [%e cont]
        ]
      | _ ->
        [%expr
          match [%e _x] with
          | Some [%p Pat.var pld.pld_name] -> [%e cont]
          | None ->
            fail (Protocol_error (Invalid_data, "Missing required field."))
        ] in
    let mk_retfield pld =
      let name = mkloc (Lident pld.pld_name.txt) pld.pld_name.loc in
      (name, Exp.ident name) in
    let struct_name = type_decl.ptype_name.txt in
    let struct_name_expr = Exp.constant (Const_string (struct_name, None)) in
    let msg = sprintf "Received wrong name for struct %s." struct_name in
    let msg_expr = Exp.constant (Const_string (msg, None)) in
    let ret_expr =
      List.fold_right mk_retcheck fields
        [%expr return [%e (Exp.record (List.map mk_retfield fields) None)]] in
    let field_id_cases =
      List.rev (Exp.case (Pat.any ()) [%expr _loop ()] ::
                List.rev_map mk_setfield fields) in
    [%expr
      fun () ->
      read_struct_begin () >>= fun name ->
      if name <> "" && name <> [%e struct_name_expr] then
        fail (Protocol_error (Invalid_data, [%e msg_expr])) else
      let _r = [%e Exp.record (List.map mk_field fields) None] in
      let rec _loop () =
        read_field_begin () >>= function
        | None -> [%e ret_expr]
        | Some (_, field_type, field_id) ->
          [%e Exp.match_ [%expr field_id] field_id_cases] in
      _loop ()]
  | Ptype_variant pcds, _ ->
    let mk_field_case pcd =
      let field_type =
        match pcd.pcd_args with
        | [t] -> t
        | _ -> raise_errorf ~loc:pcd.pcd_loc
                "Constructors for thrift unions takes a single argument." in
      let reader = reader_expr_of_core_type ~env field_type in
      let c = mkloc (Lident pcd.pcd_name.txt) pcd.pcd_name.loc in
      let field_id = Attr.id ~loc:pcd.pcd_loc pcd.pcd_attributes in
      Exp.case (Pat.constant (Const_int field_id))
               [%expr [%e reader] () >|= fun r ->
                      [%e Exp.construct c (Some [%expr r])]] in
    let default_case =
      let fmt_str = sprintf "Union %s, field %%d not implemented."
                            type_decl.ptype_name.txt in
      let fmt_expr = Exp.constant (Const_string (fmt_str, None)) in
      Exp.case (Pat.var (mknoloc "field_id"))
        [%expr fail (Protocol_error
                      (Not_implemented, sprintf [%e fmt_expr] field_id))] in
    let field_id_cases =
      List.rev (default_case :: List.rev_map mk_field_case pcds) in
    let struct_name = type_decl.ptype_name.txt in
    let struct_name_expr = Exp.constant (Const_string (struct_name, None)) in
    let msg = sprintf "Received wrong name for struct %s." struct_name in
    let msg_expr = Exp.constant (Const_string (msg, None)) in
    [%expr
      fun () ->
      read_struct_begin () >>= fun name ->
      if name <> "" && name <> [%e struct_name_expr] then
        fail (Protocol_error (Invalid_data, [%e msg_expr])) else
      read_field_begin () >>= function
      | None ->
        fail (Protocol_error (Invalid_data, "Missing union field."))
      | Some (field_name, field_tag, field_id) ->
        [%e Exp.match_ [%expr field_id] field_id_cases]
    ]
  | _ ->
    raise_errorf ~loc:type_decl.ptype_loc
                 "Cannot derive thrift methods for this kind of type."
