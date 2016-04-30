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
  env_path : string list;
  env_type_aliases : (string, type_declaration) Hashtbl.t;
  env_defined_modules : (string, string list) Hashtbl.t;
}

let deriver = "thrift"

let rec relativize_path current_path target_path =
  match current_path, target_path with
  | x :: xs, y :: ys when x = y -> relativize_path xs ys
  | _, _ -> target_path

let qualify_name path name =
  let rec loop acc = function
    | [] -> Ldot (acc, name)
    | pc :: pcs -> loop (Ldot (acc, pc)) pcs in
  match path with
  | [] -> Lident name
  | pc :: pcs -> loop (Lident pc) pcs

let mangle_type_decl pfx = Ppx_deriving.mangle_type_decl (`Prefix pfx)
let mangle_lid pfx = Ppx_deriving.mangle_lid (`Prefix pfx)

let parse_options = List.iter @@ fun (name, pexp) ->
  raise_errorf ~loc:pexp.pexp_loc "The thrift deriver takes no option %s." name

module ExpC = struct
  let int ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_int x)
  let string ?loc ?attrs x = Exp.constant ?loc ?attrs (Const_string (x, None))
end

module PatC = struct
  let int ?loc ?attrs x = Pat.constant ?loc ?attrs (Const_int x)
  let string ?loc ?attrs x = Pat.constant ?loc ?attrs (Const_string (x, None))
end

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

let rec lid_is_simple = function
  | Lident _ -> true
  | Ldot (f, _) -> lid_is_simple f
  | Lapply _ -> false

let rec collection_module_name ?loc = function
  | Lident m -> m
  | Lapply (Ldot (Lident f, "Make"), m) ->
    f ^ "_of_" ^ collection_module_name m
  | _ ->
    raise_errorf ?loc "Unrecognized collection functor application."

let mangle_io_lid ~env ?loc mthd = function
  | Lident _ as lid -> mangle_lid mthd lid
  | Ldot (m, "t") ->
    let cmn = collection_module_name ?loc m in
    if lid_is_simple m then Ldot (Lident cmn, mthd) else
    let definition_path =
      try
        relativize_path env.env_path (Hashtbl.find env.env_defined_modules cmn)
      with Not_found -> [] in
    Ldot (qualify_name definition_path (cmn ^ "_io"), mthd)
  | Ldot (_, _) | Lapply (_, _) ->
    raise_errorf ?loc "Expecting a plain type name or t-component of a module."

let rec manifest_lid = function
  | Lident _ | Ldot _ as lid -> Mod.ident (mknoloc lid)
  | Lapply (f, a) -> Mod.apply (manifest_lid f) (manifest_lid a)

let tag_name_of_constr_name = function
  | "unit"   -> Some ("Tag_unit", 0)
  | "bool"   -> Some ("Tag_bool", 0)
  | "int8"   -> Some ("Tag_byte", 0)
  | "int16"  -> Some ("Tag_i16", 0)
  | "int32"  -> Some ("Tag_i32", 0)
  | "uint64" -> Some ("Tag_u64", 0)
  | "int64"  -> Some ("Tag_i64", 0)
  | "float"  -> Some ("Tag_double", 0)
  | "string" -> Some ("Tag_string", 0)
  | "binary" -> Some ("Tag_binary", 0)
  | "map"    -> Some ("Tag_map", 1)
  | "set"    -> Some ("Tag_set", 1)
  | "list"   -> Some ("Tag_list", 1)
  | "utf8"   -> Some ("Tag_utf8", 0)
  | "utf16"  -> Some ("Tag_utf16", 0)
  | _ -> None

let rec tag_name_of_core_type ~env ptyp =
  let not_inferable () =
    raise_errorf ~loc:ptyp.ptyp_loc "Cannot infer thrift type tag for %s."
                 (Ppx_deriving.string_of_core_type ptyp) in
  match ptyp.ptyp_desc with
  | Ptyp_constr ({txt = Lident name; loc}, ptyps) ->
    if name = "option" then
      match ptyps with
      | [ptyp] -> tag_name_of_core_type ~env ptyp
      | _ -> raise_errorf ~loc:ptyp.ptyp_loc "option takes one parameter."
    else begin
      try tag_name_of_type_decl ~env (Hashtbl.find env.env_type_aliases name)
      with Not_found ->
        match tag_name_of_constr_name name with
        | Some (tag, r) ->
          if r <> List.length ptyps then
            raise_errorf ~loc:ptyp.ptyp_loc "%s expects %d parameters." name r;
          tag
        | None -> not_inferable ()
    end
  | Ptyp_constr ({txt = Ldot (Lapply (fct, mdl), "t")}, ptyps) ->
    begin match fct with
    | Ldot (Lident "Set", "Make") -> "Tag_set"
    | Ldot (Lident "Map", "Make") -> "Tag_map"
    | _ -> not_inferable ()
    end
  | _ -> not_inferable ()

and tag_name_of_type_decl ~env type_decl =
  match type_decl.ptype_kind, type_decl.ptype_manifest with
  | Ptype_abstract, Some ptyp -> tag_name_of_core_type ~env ptyp
  | Ptype_record _, _ -> "Tag_struct"
  | _ -> assert false

let tag_expr_of_core_type ~env ptyp =
  let loc = ptyp.ptyp_loc in
  let tag = tag_name_of_core_type ~env ptyp in
  Exp.construct ~loc (mkloc (Lident tag) loc) None

let tag_pat_of_core_type ~env ptyp =
  let loc = ptyp.ptyp_loc in
  let tag = tag_name_of_core_type ~env ptyp in
  Pat.construct ~loc (mkloc (Lident tag) loc) None

let is_option ptyp =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({txt = Lident "option"}, _) -> true
  | _ -> false

let strip_option ptyp =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({txt = Lident "option"}, [ptyp]) -> ptyp
  | _ -> ptyp
