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

open Lwt.Infix
open Thrift_lwt
open Thrift_lwt_unix

module List = struct
  include List

  let sample f n =
    let rec aux i acc =
      if i = n then acc else aux (i + 1) (f i :: acc) in
    List.rev (aux 0 [])
end

let random_int32 () =
  let b0 = Random.bits () in
  let b1 = Random.bits () in
  Int32.(logxor (of_int b0) (shift_left (of_int b1) 16))

let random_int64 () =
  let b0 = Random.bits () in
  let b1 = Random.bits () in
  let b2 = Random.bits () in
  let (lxor), (lsl) = Int64.(logxor, shift_left) in
  Int64.(of_int b0 lxor (of_int b1) lsl 30 lxor (of_int b2) lsl 60)

let random_float () =
  ldexp (Random.float 2.0 -. 1.0) (Random.int 20 - 10)

module Make (Protocol_functor : Thrift_sig.Protocol_functor) = struct

  let ic, oc = Lwt_io.pipe ()
  module In_transport = (val Thrift_transport.of_input_channel ic)
  module Out_transport = (val Thrift_transport.of_output_channel oc)
  module In = Protocol_functor.In (In_transport)
  module Out = Protocol_functor.Out (Out_transport)

  let test_scalars () =
    let n = 256 in
    let int8_values = List.sample (fun i -> i) 256 in
    let int16_values = List.sample (fun i -> Random.int 0x10000 - 0x8000) n in
    let int32_values = List.sample (fun i -> random_int32 ()) n in
    let int64_values = List.sample (fun i -> random_int64 ()) n in
    let float_values = List.sample (fun i -> random_float ()) n in
    Lwt.async begin fun () ->
      Out.write_bool true >>
      Out.write_bool false >>
      Lwt_list.iter_s Out.write_int8 int8_values >>
      Lwt_list.iter_s Out.write_int16 int16_values >>
      Lwt_list.iter_s Out.write_int32 int32_values >>
      Lwt_list.iter_s Out.write_int64 int64_values >>
      Lwt_list.iter_s Out.write_float float_values
    end;
    let%lwt btrue = In.read_bool () in
    let%lwt bfalse = In.read_bool () in
    Lwt.return begin
      assert (btrue = true);
      assert (bfalse = false)
    end >>
    Lwt_list.iter_s (fun x -> In.read_int8 () >|= fun x' -> assert (x = x'))
                    int8_values >>
    Lwt_list.iter_s (fun x -> In.read_int16 () >|= fun x' -> assert (x = x'))
                    int16_values >>
    Lwt_list.iter_s (fun x -> In.read_int32 () >|= fun x' -> assert (x = x'))
                    int32_values >>
    Lwt_list.iter_s (fun x -> In.read_int64 () >|= fun x' -> assert (x = x'))
                    int64_values >>
    Lwt_list.iter_s (fun x -> In.read_float () >|= fun x' -> assert (x = x'))
                    float_values

  let test_message message_type_in =
    Lwt.async begin fun () ->
      Out.write_message_begin "fifty-seven" message_type_in 57l >>
      Out.write_message_end () >>
      Out.flush ()
    end;
    let%lwt message_name, message_type, seqid = In.read_message_begin () in
    Lwt.return begin
      assert (message_name = "fifty-seven");
      assert (message_type = message_type_in);
      assert (seqid = 57l)
    end >>
    In.read_message_end ()

  let test_struct () =
    Lwt.async begin fun () ->
      Out.write_struct_begin "some_struct" >>
      Out.write_struct_end ()
    end;
    let%lwt name = In.read_struct_begin () in
    Lwt.return (assert (name = "" || name = "some_struct"))

  let test_field () =
    Lwt.async begin fun () ->
      Out.write_field_begin "test_field" Thrift.Tag_i64 23 >>
      Out.write_field_end () >>
      Out.write_field_stop ()
    end;
    begin match%lwt In.read_field_begin () with
    | None -> assert false
    | Some (name, field_type, field_id) ->
      assert (name = "" || name = "test_field");
      assert (field_type = Thrift.Tag_i64);
      assert (field_id = 23);
      Lwt.return_unit
    end >>
    In.read_field_end () >>
    begin match%lwt In.read_field_begin () with
    | None -> Lwt.return_unit
    | Some _ -> assert false
    end

  let test_map () =
    Lwt.async begin fun () ->
      Out.write_map_begin Thrift.Tag_i16 Thrift.Tag_double 19 >>
      Out.write_map_end ()
    end;
    let%lwt key_type, value_type, count = In.read_map_begin () in
    Lwt.return begin
      assert (key_type = Thrift.Tag_i16);
      assert (value_type = Thrift.Tag_double);
      assert (count = 19)
    end >>
    In.read_map_end ()

  let test_list () =
    Lwt.async begin fun () ->
      Out.write_list_begin Thrift.Tag_i32 19 >>
      Out.write_list_end ()
    end;
    let%lwt elem_type, count = In.read_list_begin () in
    Lwt.return begin
      assert (elem_type = Thrift.Tag_i32);
      assert (count = 19)
    end >>
    In.read_list_end ()

  let test_set () =
    Lwt.async begin fun () ->
      Out.write_set_begin Thrift.Tag_i32 19 >>
      Out.write_set_end ()
    end;
    let%lwt elem_type, count = In.read_set_begin () in
    Lwt.return begin
      assert (elem_type = Thrift.Tag_i32);
      assert (count = 19)
    end >>
    In.read_set_end ()

  let main =
    test_scalars () >>
    test_message Thrift.Call >>
    test_message Thrift.Reply >>
    test_message Thrift.Exception >>
    test_message Thrift.Oneway >>
    test_struct () >>
    test_field () >>
    test_map () >>
    test_list () >>
    test_set ()
end

module Test_binary_protocol = Make (Thrift_protocol_binary)

let run () = Lwt_main.run Test_binary_protocol.main
