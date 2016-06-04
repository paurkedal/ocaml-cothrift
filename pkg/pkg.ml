#! /usr/bin/env ocaml

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

#use "topfind"
#require "topkg"

open Topkg

let enable_lwt =
  let doc = "Enable libraries depending on Lwt." in
  Conf.(key "enable-lwt" bool ~absent:true ~doc)

let licenses = List.map Pkg.std_file ["COPYING"; "COPYING.LESSER"]

let opams = [Pkg.opam_file ~lint_deps_excluding:(Some ["lib"]) "opam"]

let () = Pkg.describe ~opams ~licenses "cothrift" @@ fun c ->
  Ok [
    Pkg.mllib "lib/cothrift.mllib";
    Pkg.mllib ~cond:(Conf.value c enable_lwt) "lib/cothrift-lwt.mllib";
    Pkg.mllib ~cond:(Conf.value c enable_lwt) "lib/cothrift-lwt-unix.mllib";
    Pkg.mllib ~api:[] "ppx/ppx_deriving_thrift.mllib";
  ]
