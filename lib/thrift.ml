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

type int8 = int
type int16 = int

module Bool = struct type t = bool let compare = compare end
module Int_order = struct type t = int let compare = compare end
module Int8 = Int_order
module Int16 = Int_order
module Float = struct type t = float let compare = compare end

type transport_error =
  | Unknown
  | Not_open
  | Already_open
  | Timed_out
  | End_of_file

exception Transport_error of transport_error * string

type protocol_error =
  | Unknown
  | Invalid_data
  | Negative_size
  | Size_limit
  | Bad_version
  | Not_implemented
  | Depth_limit

exception Protocol_error of protocol_error * string

type message_type =
  | Call
  | Reply
  | Exception
  | Oneway

let message_type_of_int = function
  | 1 -> Call
  | 2 -> Reply
  | 3 -> Exception
  | 4 -> Oneway
  | _ -> failwith "Thrift.message_type_of_int"

let int_of_message_type = function
  | Call -> 1
  | Reply -> 2
  | Exception -> 3
  | Oneway -> 4

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

let char_of_tag = function
  | Tag_stop -> '\000'
  | Tag_void -> '\001'
  | Tag_bool -> '\002'
  | Tag_byte -> '\003'
  | Tag_i16 -> '\006'
  | Tag_i32 -> '\008'
  | Tag_u64 -> '\009'
  | Tag_i64 -> '\010'
  | Tag_double -> '\004'
  | Tag_string -> '\011'
  | Tag_struct -> '\012'
  | Tag_map -> '\013'
  | Tag_set -> '\014'
  | Tag_list -> '\015'
  | Tag_utf8 -> '\016'
  | Tag_utf16 -> '\017'

let tag_of_char = function
  | '\000' -> Tag_stop
  | '\001' -> Tag_void
  | '\002' -> Tag_bool
  | '\003' -> Tag_byte
  | '\006' -> Tag_i16
  | '\008' -> Tag_i32
  | '\009' -> Tag_u64
  | '\010' -> Tag_i64
  | '\004' -> Tag_double
  | '\011' -> Tag_string
  | '\012' -> Tag_struct
  | '\013' -> Tag_map
  | '\014' -> Tag_set
  | '\015' -> Tag_list
  | '\016' -> Tag_utf8
  | '\017' -> Tag_utf16
  | _ -> invalid_arg "Thrift.tag_of_int"
