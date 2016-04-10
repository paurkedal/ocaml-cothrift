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

end
