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

open Printf
open Thrift

let version_mask = 0xffff0000l
let version_1 = 0x80010000l

module Make (Io : Thrift_sig.Io) = struct

  module In (T : Thrift_sig.In_transport with type 'a io := 'a Io.io) = struct
    open Io
    open T

    type 'a io = 'a Io.io

    let protocol_error code msg = fail (Protocol_error (code, msg))
    let protocol_error_f code fmt = kprintf (protocol_error code) fmt

    (* Fixed-Size Buffering *)

    let buf = Bytes.create 8
    let read_buf n = read_exact buf 0 n

    (* Scalar Read *)

    let get_char i = Bytes.get buf i
    let get_byte i = Char.code (Bytes.get buf i)

    let read_bool () = read_char () >|= (<>) '\000'
    let read_byte () = read_char () >|= Char.code
    let read_tag () =
      try read_char () >|= tag_of_char
      with Failure msg -> protocol_error Invalid_data msg

    let read_i16 () = read_buf 2 >|= fun () ->
      get_byte 0 lsl 8 lor
      get_byte 1

    let read_i32 () = read_buf 4 >|= fun () ->
      let (lsl), (lor) = Int32.(shift_left, logor) in
      Int32.of_int (get_byte 0) lsl 24 lor
      Int32.of_int (get_byte 1) lsl 16 lor
      Int32.of_int (get_byte 2) lsl  8 lor
      Int32.of_int (get_byte 3)

    let read_i64 () = read_buf 8 >|= fun () ->
      let (lsl), (lor) = Int64.(shift_left, logor) in
      Int64.of_int (get_byte 0) lsl 56 lor
      Int64.of_int (get_byte 1) lsl 48 lor
      Int64.of_int (get_byte 2) lsl 40 lor
      Int64.of_int (get_byte 3) lsl 32 lor
      Int64.of_int (get_byte 4) lsl 24 lor
      Int64.of_int (get_byte 5) lsl 16 lor
      Int64.of_int (get_byte 6) lsl  8 lor
      Int64.of_int (get_byte 7)

    let read_double () = read_i64 () >|= Int64.float_of_bits

    let read_binary_bytes () =
      read_i32 () >>= fun size ->
      let size = Int32.to_int size in
      let x = Bytes.create size in
      read_exact x 0 size >|= fun () ->
      x

    let read_binary () = read_binary_bytes () >|= Bytes.unsafe_to_string

    let read_string = read_binary

    (* Compound Read *)

    let read_message_begin () =
      try
        read_i32 () >>= fun version_etc ->
        let version = Int32.(logand version_etc version_mask) in
        if version = version_1 then begin
          let msgtype =
            message_type_of_int Int32.(to_int (logand version_etc 0xffl)) in
          read_string () >>= fun name ->
          read_i32 () >|= fun seqid ->
          (name, msgtype, seqid)
        end else if version >= 0l then begin
          let name_size = Int32.to_int version_etc in
          let name = Bytes.create name_size in
          read_exact name 0 name_size >>= fun () ->
          read_byte () >>= fun msgtype ->
          read_i32 () >|= fun seqid ->
          (Bytes.unsafe_to_string name, message_type_of_int msgtype, seqid)
        end else
          protocol_error_f Bad_version "Unsupported version %ld." version
      with Failure msg ->
        protocol_error Invalid_data msg

    let read_message_end = return

    let read_struct_begin () = return ""

    let read_struct_end = return

    let read_field_begin () =
      read_tag () >>= fun field_type ->
      if field_type = Tag_stop then return None else
      read_i16 () >|= fun field_id ->
      Some ("", field_type, field_id)

    let read_field_end = return

    let read_map_begin () =
      read_tag () >>= fun key_type ->
      read_tag () >>= fun value_type ->
      read_i32 () >|= fun size ->
      (key_type, value_type, Int32.to_int size)

    let read_map_end = return

    let read_list_begin () =
      read_tag () >>= fun elem_type ->
      read_i32 () >|= fun size ->
      (elem_type, Int32.to_int size)

    let read_list_end = return

    let read_set_begin = read_list_begin
    let read_set_end = return

    let close = close
  end

  module Out (T : Thrift_sig.Out_transport with type 'a io := 'a Io.io) =
  struct
    open Io
    open T

    type 'a io = 'a Io.io

    (* Fixed-Size Buffering *)

    let buf = Bytes.create 8
    let write_buf n = write buf 0 n

    (* Scalar Write *)

    let set_char i x = Bytes.set buf i x
    let set_byte i x = Bytes.set buf i (Char.chr x)

    let write_byte x =
      set_byte 0 x;
      write_buf 1

    let write_bool x = write_byte (if x then 1 else 0)

    let set_i16 i x =
      set_byte (i + 0) (x asr 8 land 0xff);
      set_byte (i + 1) (x land 0xff)

    let write_i16 x = set_i16 0 x; write_buf 2

    let set_i32 i x =
      let (lsr), (land) = Int32.(shift_right_logical, logand) in
      set_byte (i + 0) (Int32.to_int (x lsr 24));
      set_byte (i + 1) (Int32.to_int (x lsr 16 land 0xffl));
      set_byte (i + 2) (Int32.to_int (x lsr 8 land 0xffl));
      set_byte (i + 3) (Int32.to_int (x land 0xffl))

    let write_i32 x = set_i32 0 x; write_buf 4

    let write_i64 x =
      let (lsr), (land) = Int64.(shift_right_logical, logand) in
      set_byte 0 (Int64.to_int (x lsr 56));
      set_byte 1 (Int64.to_int (x lsr 48 land 0xffL));
      set_byte 2 (Int64.to_int (x lsr 40 land 0xffL));
      set_byte 3 (Int64.to_int (x lsr 32 land 0xffL));
      set_byte 4 (Int64.to_int (x lsr 24 land 0xffL));
      set_byte 5 (Int64.to_int (x lsr 16 land 0xffL));
      set_byte 6 (Int64.to_int (x lsr  8 land 0xffL));
      set_byte 7 (Int64.to_int (x land 0xffL));
      write_buf 8

    let write_double x = write_i64 (Int64.bits_of_float x)

    let write_binary_bytes x i n =
      if n lsr 32 <> 0 then failwith "Thrift_protocol_binary.write_binary";
      write_i32 (Int32.of_int n) >>= fun () ->
      write x i n

    let write_binary x =
      write_binary_bytes (Bytes.unsafe_of_string x) 0 (String.length x)

    let write_string = write_binary

    (* Compound Write *)

    let write_message_begin name msgtype seqid =
      let n = String.length name in
      set_i32 0 Int32.(logor version_1 (of_int (int_of_message_type msgtype)));
      set_i32 4 Int32.(of_int n);
      write_buf 8 >>= fun () ->
      write (Bytes.unsafe_of_string name) 0 n >>= fun () ->
      write_i32 seqid

    let write_message_end = return

    let write_struct_begin _ = return ()

    let write_struct_end = return

    let write_field_begin name field_type field_id =
      set_char 0 (char_of_tag field_type);
      set_i16 1 field_id;
      write_buf 3

    let write_field_end = return

    let write_field_stop () =
      set_char 0 (char_of_tag Tag_stop);
      write_buf 1

    let write_map_begin key_type value_type size =
      set_char 0 (char_of_tag key_type);
      set_char 1 (char_of_tag value_type);
      set_i32 2 (Int32.of_int size);
      write_buf 6

    let write_map_end = return

    let write_list_begin elem_type size =
      set_char 0 (char_of_tag elem_type);
      set_i32 1 (Int32.of_int size);
      write_buf 5

    let write_list_end = return

    let write_set_begin = write_list_begin
    let write_set_end = return

    let flush = flush

    let close = close
  end
end
