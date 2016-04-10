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

type byte = int
type i16 = int
type i32 = int32
type i64 = int64
type double = float

type transport_error =
  | Unknown
  | Not_open
  | Already_open
  | Timed_out
  | End_of_file

type protocol_error =
  | Unknown
  | Invalid_data
  | Negative_size
  | Size_limit
  | Bad_version
  | Not_implemented
  | Depth_limit

exception Transport_error of transport_error * string
exception Protocol_error of protocol_error * string

type tag =
  | Tag_stop
  | Tag_void
  | Tag_bool
  | Tag_byte
  | Tag_i16
  | Tag_i32
  | Tag_u64
  | Tag_i64
  | Tag_double
  | Tag_string
  | Tag_struct
  | Tag_map
  | Tag_set
  | Tag_list
  | Tag_utf8
  | Tag_utf16

val char_of_tag : tag -> char
val tag_of_char : char -> tag

type message_type =
  | Call
  | Reply
  | Exception
  | Oneway

val message_type_of_int : int -> message_type
val int_of_message_type : message_type -> int
