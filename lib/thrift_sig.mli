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

module type Io = sig
  type 'a io

  val (>>=) : 'a io -> ('a -> 'b io) -> 'b io
  val (>|=) : 'a io -> ('a -> 'b) -> 'b io
  val return : 'a -> 'a io
  val fail : exn -> 'a io
end

module type In_transport = sig
  type 'a io

  val read_avail : bytes -> int -> int -> int io
  val read_exact : bytes -> int -> int -> unit io
  val read_char : unit -> char io
  val close : unit -> unit io
end

module type Out_transport = sig
  type 'a io

  val write : bytes -> int -> int -> unit io
  val flush : unit -> unit io
  val close : unit -> unit io
end

module type In_protocol = sig
  type 'a io

  val read_message_begin : unit -> (string * message_type * Int32.t) io
  val read_message_end : unit -> unit io
  val read_struct_begin : unit -> string io
  val read_struct_end : unit -> unit io
  val read_field_begin : unit -> (string * tag * int) option io
  val read_field_end : unit -> unit io
  val read_map_begin : unit -> (tag * tag * int) io
  val read_map_end : unit -> unit io
  val read_list_begin : unit -> (tag * int) io
  val read_list_end : unit -> unit io
  val read_set_begin : unit -> (tag * int) io
  val read_set_end : unit -> unit io
  val read_bool : unit -> bool io
  val read_byte : unit -> int io
  val read_i16 : unit -> int io
  val read_i32 : unit -> Int32.t io
  val read_i64 : unit -> Int64.t io
  val read_double : unit -> float io
  val read_string : unit -> string io
  val read_binary : unit -> string io
  val read_binary_bytes : unit -> Bytes.t io

  val close : unit -> unit io
end

module type Out_protocol = sig
  type 'a io

  val write_message_begin : string -> message_type -> Int32.t -> unit io
  val write_message_end : unit -> unit io
  val write_struct_begin : string -> unit io
  val write_struct_end : unit -> unit io
  val write_field_begin : string -> tag -> int -> unit io
  val write_field_end : unit -> unit io
  val write_field_stop : unit -> unit io
  val write_map_begin : tag -> tag -> int -> unit io
  val write_map_end : unit -> unit io
  val write_list_begin : tag -> int -> unit io
  val write_list_end : unit -> unit io
  val write_set_begin : tag -> int -> unit io
  val write_set_end : unit -> unit io
  val write_bool : bool -> unit io
  val write_byte : int -> unit io
  val write_i16 : int -> unit io
  val write_i32 : Int32.t -> unit io
  val write_i64 : Int64.t -> unit io
  val write_double : float -> unit io
  val write_string : string -> unit io
  val write_binary : string -> unit io
  val write_binary_bytes : Bytes.t -> int -> int -> unit io

  val flush : unit -> unit io
  val close : unit -> unit io
end

module type Protocol = sig
  type 'a io

  module In :
    functor (T : In_transport with type 'a io := 'a io) ->
    In_protocol with type 'a io := 'a io
  module Out :
    functor (T : Out_transport with type 'a io := 'a io) ->
    Out_protocol with type 'a io := 'a io
end

module type Processor =
  functor (Io : Io) ->
  functor (Iprot : In_protocol with type 'a io := 'a Io.io) ->
  functor (Oprot : Out_protocol with type 'a io := 'a Io.io) ->
sig
  val run : unit -> 'a Io.io
end
