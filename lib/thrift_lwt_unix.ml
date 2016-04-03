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

open Thrift_lwt

module Thrift_server (Processor : Thrift_sig.Processor) = struct

  let process flow ic oc =
    let module In_transport = (val Thrift_transport.of_input_channel ic) in
    let module Out_transport = (val Thrift_transport.of_output_channel oc) in
    let module In_protocol = Thrift_protocol_binary.In (In_transport) in
    let module Out_protocol = Thrift_protocol_binary.Out (Out_transport) in
    let module Processor = Processor (Thrift_io) (In_protocol) (Out_protocol) in
    Processor.run ()

  let serve ?timeout ?stop ~ctx mode =
    Conduit_lwt_unix.serve ?timeout ?stop ~ctx ~mode process

end

let connect ~ctx client =
  let%lwt _, ic, oc = Conduit_lwt_unix.connect ~ctx client in
  let module In_transport = (val Thrift_transport.of_input_channel ic) in
  let module Out_transport = (val Thrift_transport.of_output_channel oc) in
  let module In_protocol = Thrift_protocol_binary.In (In_transport) in
  let module Out_protocol = Thrift_protocol_binary.Out (Out_transport) in
  Lwt.return
    ((module In_protocol : Thrift_sig.In_protocol),
     (module Out_protocol : Thrift_sig.Out_protocol))
