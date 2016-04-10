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

module Make (Protocol_functor : Thrift_sig.Protocol_functor) = struct

  let ic, oc = Lwt_io.pipe ()

  module In_transport = (val Thrift_transport.of_input_channel ic)
  module Out_transport = (val Thrift_transport.of_output_channel oc)
  module In_protocol = Thrift_protocol_binary.In (In_transport)
  module Out_protocol = Thrift_protocol_binary.Out (Out_transport)
  module Sample = Sample.Make (Thrift_io) (In_protocol) (Out_protocol)

  let run () =
    let stuff_out = Sample.Stuff.{
      a_bool = true;
      a_byte = 2;
      an_i16 = 3;
      an_i32 = 5l;
      an_i64 = Some 7L;
      a_double = 11.0;
      a_string = "thirteen";
      an_i16_list = [17; 19; 21; 23];
    } in
    Lwt.async (fun () -> Sample.Stuff.write stuff_out);
    let%lwt stuff_in = Sample.Stuff.read () in
    let () = assert (stuff_in = stuff_out) in
    In_protocol.close () >> Out_protocol.close ()

end

let test_binary () =
  let module Test_binary = Make (Thrift_protocol_binary) in
  Test_binary.run ()

let run () = Lwt_main.run begin
  test_binary ()
end
