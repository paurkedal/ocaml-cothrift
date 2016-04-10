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

type env = {
  env_type_aliases : (string, type_declaration) Hashtbl.t;
  env_defined_modules : (string, unit) Hashtbl.t;
}

let deriver = "thrift"

let mangle_type_decl pfx = Ppx_deriving.mangle_type_decl (`Prefix pfx)
let mangle_lid pfx = Ppx_deriving.mangle_lid (`Prefix pfx)

let parse_options = List.iter @@ fun (name, pexp) ->
  raise_errorf ~loc:pexp.pexp_loc "The thrift deriver takes no option %s." name

module Attr = struct

  let id ~loc attrs =
    match attrs |> Ppx_deriving.attr ~deriver "id"
                |> Ppx_deriving.Arg.(get_attr ~deriver int) with
    | Some field_id -> field_id
    | None -> raise_errorf ~loc "Missing [@thrift.id _]."

  let default attrs =
    attrs |> Ppx_deriving.attr ~deriver "default"
          |> Ppx_deriving.Arg.(get_attr ~deriver expr)
end

let rec collection_module_name = function
  | Lident m -> m
  | Lapply (Ldot (Lident f, "Make"), m) ->
    f ^ "_of_" ^ collection_module_name m
  | _ -> assert false

let mangle_io_lid mthd = function
  | Lident _ as lid -> mangle_lid mthd lid
  | Ldot (m, "t") -> Ldot (Lident (collection_module_name m ^ "_io"), mthd)

let rec manifest_lid = function
  | Lident _ | Ldot _ as lid -> Mod.ident (mknoloc lid)
  | Lapply (f, a) -> Mod.apply (manifest_lid f) (manifest_lid a)

let tag_expr_of_constr_name = function
  | "unit"   -> Some ([%expr Tag_unit], 0)
  | "bool"   -> Some ([%expr Tag_bool], 0)
  | "int8"   -> Some ([%expr Tag_byte], 0)
  | "int16"  -> Some ([%expr Tag_i16], 0)
  | "int32"  -> Some ([%expr Tag_i32], 0)
  | "uint64" -> Some ([%expr Tag_u64], 0)
  | "int64"  -> Some ([%expr Tag_i64], 0)
  | "float"  -> Some ([%expr Tag_double], 0)
  | "string" -> Some ([%expr Tag_string], 0)
  | "binary" -> Some ([%expr Tag_binary], 0)
  | "map"    -> Some ([%expr Tag_map], 1)
  | "set"    -> Some ([%expr Tag_set], 1)
  | "list"   -> Some ([%expr Tag_list], 1)
  | "utf8"   -> Some ([%expr Tag_utf8], 0)
  | "utf16"  -> Some ([%expr Tag_utf16], 0)
  | _ -> None

let rec tag_expr_of_core_type ~env ptyp =
  let not_inferable () =
    raise_errorf ~loc:ptyp.ptyp_loc "Cannot infer thrift type tag for %s."
                 (Ppx_deriving.string_of_core_type ptyp) in
  match ptyp.ptyp_desc with
  | Ptyp_constr ({txt = Lident name; loc}, ptyps) ->
    if name = "option" then
      match ptyps with
      | [ptyp] -> tag_expr_of_core_type ~env ptyp
      | _ -> raise_errorf ~loc:ptyp.ptyp_loc "option takes one parameter."
    else begin
      try tag_expr_of_type_decl ~env (Hashtbl.find env.env_type_aliases name)
      with Not_found ->
        match tag_expr_of_constr_name name with
        | Some (tag, r) ->
          if r <> List.length ptyps then
            raise_errorf ~loc:ptyp.ptyp_loc "%s expects %d parameters." name r;
          tag
        | None -> not_inferable ()
    end
  | Ptyp_constr ({txt = Ldot (Lapply (fct, mdl), "t")}, ptyps) ->
    begin match fct with
    | Ldot (Lident "Set", "Make") -> [%expr Tag_set]
    | Ldot (Lident "Map", "Make") -> [%expr Tag_map]
    | _ -> not_inferable ()
    end
  | _ -> not_inferable ()

and tag_expr_of_type_decl ~env type_decl =
  match type_decl.ptype_kind, type_decl.ptype_manifest with
  | Ptype_abstract, Some ptyp -> tag_expr_of_core_type ~env ptyp
  | Ptype_record _, _ -> [%expr Tag_struct]
  | _ -> assert false

let is_option ptyp =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({txt = Lident "option"}, _) -> true
  | _ -> false

let strip_option ptyp =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({txt = Lident "option"}, [ptyp]) -> ptyp
  | _ -> ptyp
