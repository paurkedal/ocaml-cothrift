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

let partial_type_of_record name plds =
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
  Type.mk ~kind:(Ptype_record (List.map to_raw plds)) name

let partial_type_of_type_decl type_decl =
  match type_decl.ptype_kind with
  | Ptype_record plds ->
    let name = mkloc ("partial_" ^ type_decl.ptype_name.txt)
                     type_decl.ptype_name.loc in
    [partial_type_of_record name plds]
  | _ -> []

let rec reader_expr_of_core_type ~env ?union_name ptyp =
  let loc = ptyp.ptyp_loc in
  match ptyp.ptyp_desc with
  | Ptyp_constr ({txt = lid; loc}, ptyps) ->
    let f = Exp.ident (mkloc (mangle_io_lid ~env ~loc "read" lid) loc) in
    let tagged_reader ptyp =
      let tag = tag_expr_of_core_type ~env ptyp in
      let r = reader_expr_of_core_type ~env ptyp in
      [tag; r] in
    app f (List.flatten (List.map tagged_reader ptyps))
  | Ptyp_variant (rows, Closed, None) ->
    let union_name =
      match union_name with
      | None -> raise_errorf ~loc "Cannot create reader for anonymous variant."
      | Some name -> name in
    let mk_field_case = function
      | Rtag ("Ok", [], _, [arg_type]) ->
        let reader = reader_expr_of_core_type ~env arg_type in
        Exp.case (PatC.int ~loc 0) [%expr [%e reader] () >|= fun _x -> `Ok _x]
      | Rtag (row_label, row_attributes, _, [arg_type]) ->
        let field_id = Attr.id ~loc row_attributes in
        let reader = reader_expr_of_core_type ~env arg_type in
        Exp.case (PatC.int ~loc field_id)
          [%expr [%e reader] () >|= fun _x ->
                 [%e Exp.variant row_label (Some [%expr _x])]]
      | Rtag (_, _, _, _) ->
        raise_errorf ~loc "Can only derive thrift union serializers for \
                           single-argument constructors."
      | Rinherit t ->
        raise_errorf ~loc "Only expanded variant types supported." in
    let default_case =
      let msg = sprintf "Union %s, field %%d not implemented." union_name in
      Exp.case (Pat.var (mknoloc "field_id"))
        [%expr fail (Protocol_error
                (Not_implemented, sprintf [%e ExpC.string msg] field_id))] in
    let field_id_cases =
      List.rev (default_case :: List.rev_map mk_field_case rows) in
    let msg = sprintf "Received wrong name for union %s." union_name in
    [%expr fun () ->
      read_union
        (fun name field_name field_tag field_id ->
          if name <> "" && name <> [%e ExpC.string union_name] then
            fail (Protocol_error (Invalid_data, [%e ExpC.string msg])) else
          [%e Exp.match_ [%expr field_id] field_id_cases])
    ]
  | _ ->
    raise_errorf ~loc:ptyp.ptyp_loc "Cannot derive thrift for %s."
                 (Ppx_deriving.string_of_core_type ptyp)

let partial_reader_expr_of_record ~env ~struct_name plds cont =

  let mk_field pld =
    let name = mkloc (Lident ("p_" ^ pld.pld_name.txt)) pld.pld_name.loc in
    match Attr.default pld.pld_attributes with
    | None -> (name, [%expr None])
    | Some e -> (name, e) in

  let mk_setfield pld =
    let field_id = Attr.id ~loc:pld.pld_loc pld.pld_attributes in
    let msg = sprintf "Received wrong field type for %s.%s"
                      struct_name pld.pld_name.txt in
    let reader_expr =
      reader_expr_of_core_type ~env (strip_option pld.pld_type) in
    let is_opt = Attr.default pld.pld_attributes = None in
    Exp.case (PatC.int field_id)
      [%expr
        if field_type <> [%e tag_expr_of_core_type ~env pld.pld_type] then
          fail (Protocol_error (Invalid_data, [%e ExpC.string msg]))
        else begin
          [%e reader_expr] () >>= fun _x ->
          [%e Exp.setfield [%expr _r] (partial_field_ident pld)
                           (if is_opt then [%expr Some _x] else [%expr _x])];
          _loop ()
        end
      ] in

  let field_id_cases =
    List.rev @@
      Exp.case (Pat.any ()) [%expr _loop ()] ::
      List.rev_map mk_setfield plds in
  let msg = sprintf "Received wrong name for struct %s." struct_name in
  [%expr
    fun () ->
    read_struct_begin () >>= fun name ->
    if name <> "" && name <> [%e ExpC.string struct_name] then
      fail (Protocol_error (Invalid_data, [%e ExpC.string msg])) else
    let _r = [%e Exp.record (List.map mk_field plds) None] in
    let rec _loop () =
      read_field_begin () >>= function
      | None -> [%e cont [%expr _r]]
      | Some (_, field_type, field_id) ->
        [%e Exp.match_ [%expr field_id] field_id_cases] in
    _loop ()]

let checked_reader_expr_of_record ~env ~struct_name plds scoped =
  let mk_retcheck partial_record pld cont =
    let _x = Exp.field partial_record (partial_field_ident pld) in
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
  partial_reader_expr_of_record ~env ~struct_name plds
    (fun partial -> List.fold_right (mk_retcheck partial) plds scoped)

let reader_expr_of_type_decl ~env type_decl =
  match type_decl.ptype_kind, type_decl.ptype_manifest with
  | Ptype_abstract, Some ptyp ->
    reader_expr_of_core_type ~env ptyp
  | Ptype_record plds, _ ->
    let mk_retfield pld =
      let name = mkloc (Lident pld.pld_name.txt) pld.pld_name.loc in
      (name, Exp.ident name) in
    let struct_name = type_decl.ptype_name.txt in
    let ret_expr =
      [%expr return [%e (Exp.record (List.map mk_retfield plds) None)]] in
    checked_reader_expr_of_record ~env ~struct_name plds ret_expr
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
      Exp.case (PatC.int field_id)
               [%expr [%e reader] () >|= fun r ->
                      [%e Exp.construct c (Some [%expr r])]] in
    let default_case =
      let msg = sprintf "Union %s, field %%d not implemented."
                        type_decl.ptype_name.txt in
      Exp.case (Pat.var (mknoloc "field_id"))
        [%expr fail (Protocol_error (Not_implemented,
                                     sprintf [%e ExpC.string msg] field_id))] in
    let field_id_cases =
      List.rev (default_case :: List.rev_map mk_field_case pcds) in
    let union_name = type_decl.ptype_name.txt in
    let msg = sprintf "Received wrong name for struct %s." union_name in
    [%expr fun () ->
      read_union
        (fun name field_name field_tag field_id ->
          if name <> "" && name <> [%e ExpC.string union_name] then
            fail (Protocol_error (Invalid_data, [%e ExpC.string msg])) else
          [%e Exp.match_ [%expr field_id] field_id_cases])
    ]
  | _ ->
    raise_errorf ~loc:type_decl.ptype_loc
                 "Cannot derive thrift methods for this kind of type."
