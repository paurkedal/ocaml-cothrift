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

module Make =
  functor (Io : Thrift_sig.Io) ->
  functor (Iprot : Thrift_sig.In_protocol with type 'a io := 'a Io.io) ->
  functor (Oprot : Thrift_sig.Out_protocol with type 'a io := 'a Io.io) ->
struct
  open Io
  open Iprot
  open Oprot
  include Thrift_ext_protocol.Make (Io) (Iprot) (Oprot)

  module Stuff = struct
    type t = {
      a_bool : bool [@thrift.id 1] [@thrift.default false];
      a_byte : int8 [@thrift.id 2] [@thrift.default -1];
      an_i16 : int16 [@thrift.id 3];
      an_i32 : int32 [@thrift.id 4];
      an_i64 : int64 option [@thrift.id 5];
      a_double : float [@thrift.id 6];
      a_string : string [@thrift.id 7] [@thrift.default ""];
      an_i16_list : int16 list [@thrift.id 8];
    } [@@deriving thrift]
  end
end
