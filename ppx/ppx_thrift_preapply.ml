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

let rec preapplications_of_lid ~env ~path = function
  | Lapply (Ldot (Lident corig, "Make"), arg) as morig ->
    let name = collection_module_name morig in
    let name_io = name ^ "_io" in
    begin try
      let alt_path = Hashtbl.find env.env_defined_modules name in
      let mimpl_io = qualify_name (relativize_path path alt_path) name_io in
      (Mod.ident (mknoloc mimpl_io), [])
    with Not_found ->
      Hashtbl.add env.env_defined_modules name path;
      let marg_io, stris = preapplications_of_lid ~env ~path arg in
      let funct_io = mknoloc (Lident (corig ^ "_io")) in
      let mapp = manifest_lid morig in
      let mapp_io = Mod.apply (Mod.apply (Mod.ident funct_io) mapp) marg_io in
      let stri_io = Str.module_ (Mb.mk (mknoloc name_io) mapp_io) in
      let mimpl_io = mknoloc (Lident name_io) in
      (Mod.ident mimpl_io, stri_io :: stris)
    end
  | Lident name ->
    let mimpl_io = Mod.ident (mknoloc (Lident (name ^ "_io"))) in
    (mimpl_io, [])
  | _ -> assert false

let rec preapplications_of_core_type ~env ~path ptyp =
  match ptyp.ptyp_desc with
  | Ptyp_constr ({txt = Ldot (m, "t"); loc}, ptyps) ->
    let aux = preapplications_of_core_type ~env ~path in
    let stris1 = List.flatten (List.map aux ptyps) in
    let _, stris2 = preapplications_of_lid ~env ~path m in
    stris1 @ List.rev stris2
  | _ -> []

let preapplications_of_type_decl ~env ~path ptype =
  match ptype.ptype_kind, ptype.ptype_manifest with
  | Ptype_abstract, Some ptyps -> preapplications_of_core_type ~env ~path ptyps
  | Ptype_record plds, _ ->
    let aux pld = preapplications_of_core_type ~env ~path pld.pld_type in
    List.flatten (List.map aux plds)
  | _ -> []
