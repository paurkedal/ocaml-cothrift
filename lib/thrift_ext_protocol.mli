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

  val read_list : tag -> (unit -> 'a io) -> unit -> 'a list io
  val write_list : tag -> ('a -> unit io) -> 'a list -> unit io

  module Bool_io :
    Thrift_sig.Value_io with type t = bool and type 'a io := 'a io
  module Int8_io :
    Thrift_sig.Value_io with type t = int8 and type 'a io := 'a io
  module Int16_io :
    Thrift_sig.Value_io with type t = int16 and type 'a io := 'a io
  module Int32_io :
    Thrift_sig.Value_io with type t = int32 and type 'a io := 'a io
  module Int64_io :
    Thrift_sig.Value_io with type t = int64 and type 'a io := 'a io
  module Float_io :
    Thrift_sig.Value_io with type t = float and type 'a io := 'a io

  module Set_io :
    functor (Set : Set.S) ->
    functor (Elt_io : Thrift_sig.Value_io
              with type t = Set.elt and type 'a io := 'a io) ->
    Thrift_sig.Value_io with type t = Set.t and type 'a io := 'a io

  module Map_io :
    functor (Map : Map.S) ->
    functor (Key_io : Thrift_sig.Value_io
              with type t = Map.key and type 'a io := 'a io) ->
  sig
    val read : tag -> (unit -> 'a io) -> unit -> 'a Map.t io
    val write : tag -> ('a -> unit io) -> 'a Map.t -> unit io
  end
end
