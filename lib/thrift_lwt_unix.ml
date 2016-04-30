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
open Thrift_lwt

module Thrift_transport = struct

  let of_input_channel ?(close = true) ic =
    (module struct
      let read_avail = Lwt_io.read_into ic
      let read_exact = Lwt_io.read_into_exactly ic
      let read_char () = Lwt_io.read_char ic
      let close () = Lwt_io.close ic
    end : Thrift_sig.In_transport)

  let of_output_channel ?(close = true) oc =
    (module struct
      let write = Lwt_io.write_from_exactly oc
      let flush () = Lwt_io.flush oc
      let close () = Lwt_io.close oc
    end : Thrift_sig.Out_transport)

end

module Thrift_server (Processor_functor : Thrift_sig.Processor_functor) = struct

  let process flow ic oc =
    let module In_transport = (val Thrift_transport.of_input_channel ic) in
    let module Out_transport = (val Thrift_transport.of_output_channel oc) in
    let module In_protocol = Thrift_protocol_binary.In (In_transport) in
    let module Out_protocol = Thrift_protocol_binary.Out (Out_transport) in
    let module Processor = Processor_functor (In_protocol) (Out_protocol) in
    let open In_protocol in
    let open Out_protocol in
    let rec loop () =
      let%lwt msg_name, msg_type, seqid = read_message_begin () in
      let%lwt handler =
        try Lwt.return (Hashtbl.find Processor.handlers msg_name)
        with Not_found ->
          let msg = sprintf "Method %s is not implemented." msg_name in
          Lwt.fail (Protocol_error (Not_implemented, msg)) in
      begin match msg_type with
      | Call ->
        begin match handler with
        | Processor.Handler receive ->
          let%lwt is_exn, respond = receive () in
          read_message_end () >>
          write_message_begin msg_name (if is_exn then Exception else Reply)
                              seqid >>
          respond () >>
          write_message_end () >>
          loop ()
        | Processor.Handler_unit (is_oneway, receive) ->
          receive () >>
          read_message_end () >>
          write_message_begin msg_name Reply seqid >>
          write_struct_begin "void" >>
          write_field_stop () >>
          write_struct_end ()
        end
      | Oneway ->
        begin match handler with
        | Processor.Handler _ ->
          let msg = sprintf "Invalid oneway call of %s which returns a value."
                            msg_name in
          Lwt.fail (Protocol_error (Invalid_data, msg))
        | Processor.Handler_unit (is_oneway, receive) ->
          receive () >>
          read_message_end ()
        end
      | Reply ->
        Lwt.fail (Protocol_error (Invalid_data, "Server received reply."))
      | Exception ->
        Lwt.fail (Protocol_error (Invalid_data, "Server received exception."))
      end in
    (* TODO: Handle and log protocol errors here. *)
    loop ()

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
