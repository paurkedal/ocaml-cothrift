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

open Thrift

module Make :
  functor (Io : Thrift_sig.Io) ->
  functor (Iprot : Thrift_sig.In_protocol with type 'a io := 'a Io.io) ->
  functor (Oprot : Thrift_sig.Out_protocol with type 'a io := 'a Io.io) ->
sig
  open Io

  module Stuff : sig
    type t = {
      a_bool : bool;
      a_byte : int8;
      an_i16 : int16;
      an_i32 : int32;
      an_i64 : int64 option;
      a_double : float;
      a_string : string;
      an_i16_list : int16 list;
      an_i32_set : Set.Make (Int32).t;
      an_i32_set_set : Set.Make (Set.Make (Int32)).t;
      map1 : bool Map.Make (Set.Make (Int16)).t;
      map2 : Set.Make (Bool).t Map.Make (Set.Make (Int16)).t Map.Make (Int16).t;
    } [@@deriving thrift]
  end

  module Union : sig
    type t =
      | One of int32
      | Many of Set.Make (Int32).t
      [@@deriving thrift]
  end

  module Unix_error : sig
    type t = {errno : int32 [@thrift.id 1];} [@@deriving thrift]
  end

  module Simple_error : sig
    type t = {message : string [@thrift.id 1];} [@@deriving thrift]
  end

  module type Arpeecee = sig
    val f : x: (Stuff.t [@thrift.id 1]) -> unit -> Union.t io
    val sum : elements: (Set.Make (Int32).t [@thrift.id 1]) -> unit ->
          [ `Ok of int32
          | `Unix_error of Unix_error.t [@thrift.id 1]
          | `Simple_error of Simple_error.t [@thrift.id 2]] io
  end [@@deriving thrift]

end
