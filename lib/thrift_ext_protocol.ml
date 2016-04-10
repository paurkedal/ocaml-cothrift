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

  let read_list tag read_elt () =
    read_list_begin () >>= fun (tag', n) ->
    if tag <> tag' then
      fail (Protocol_error (Invalid_data, "Wrong list element type.")) else
    if n < 0 then
      fail (Protocol_error (Negative_size, "Negative list length.")) else
    let rec read_elts n xs =
      if n = 0 then (read_list_end () >>= fun () -> return (List.rev xs)) else
      read_elt () >>= fun x ->
      read_elts (n - 1) (x :: xs) in
    read_elts n []

  let write_list tag write_elt xs =
    write_list_begin tag (List.length xs) >>= fun () ->
    let rec write_elts = function
      | [] -> write_list_end ()
      | x :: xs -> write_elt x >>= fun () -> write_elts xs in
    write_elts xs

  module Bool_io = struct
    type t = bool
    let tag = Tag_bool
    let read = read_bool
    let write = write_bool
  end
  module Int8_io = struct
    type t = int8
    let tag = Tag_byte
    let read = read_int8
    let write = write_int8
  end
  module Int16_io = struct
    type t = int16
    let tag = Tag_i16
    let read = read_int16
    let write = write_int16
  end
  module Int32_io = struct
    type t = int32
    let tag = Tag_i32
    let read = read_int32
    let write = write_int32
  end
  module Int64_io = struct
    type t = int64
    let tag = Tag_i64
    let read = read_int64
    let write = write_int64
  end
  module Float_io = struct
    type t = float
    let tag = Tag_double
    let read = read_float
    let write = write_float
  end

  module Set_io =
    functor (Set : Set.S) ->
    functor (Elt_io : Thrift_sig.Value_io
              with type t = Set.elt and type 'a io := 'a io) ->
  struct
    type t = Set.t

    let read () =
      read_set_begin () >>= fun (elt_tag, n) ->
      if Elt_io.tag <> elt_tag then
        fail (Protocol_error (Invalid_data, "Wrong set element type.")) else
      if n < 0 then
        fail (Protocol_error (Negative_size, "Negative set size.")) else
      let rec read_elts n xs =
        if n = 0 then (read_set_end () >>= fun () -> return xs) else
        Elt_io.read () >>= fun x ->
        read_elts (n - 1) (Set.add x xs) in
      read_elts n Set.empty

    let write xs =
      Set.fold (fun x acc -> acc >>= fun () -> Elt_io.write x) xs
        (write_set_begin Elt_io.tag (Set.cardinal xs)) >>=
      write_set_end

    let tag = Tag_set
  end

  module Map_io =
    functor (Map : Map.S) ->
    functor (Key_io : Thrift_sig.Value_io
              with type t = Map.key and type 'a io := 'a io) ->
  struct
    let read expected_value_tag read_value () =
      read_map_begin () >>= fun (key_tag, value_tag, n) ->
      if Key_io.tag <> key_tag then
        fail (Protocol_error (Invalid_data, "Wrong type for map keys.")) else
      if expected_value_tag <> value_tag then
        fail (Protocol_error (Invalid_data, "Wrong type for map values.")) else
      if n < 0 then
        fail (Protocol_error (Negative_size, "Negative map size.")) else
      let rec read_bindings n map =
        if n = 0 then (read_map_end () >>= fun () -> return map) else
        Key_io.read () >>= fun k ->
        read_value () >>= fun v ->
        read_bindings (n - 1) (Map.add k v map) in
      read_bindings n Map.empty

    let write value_tag write_value map =
      let write_binding k v acc =
        acc >>= fun () -> Key_io.write k >>= fun () -> write_value v in
      Map.fold write_binding map
        (write_map_begin Key_io.tag value_tag (Map.cardinal map)) >>=
      write_map_end
  end
end
