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

module Thrift_sig = Thrift_lwt_sig

module Thrift_io = struct
  type 'a io = 'a Lwt.t
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
  let return = Lwt.return
  let fail = Lwt.fail
end

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

module Thrift_protocol_binary = Thrift_protocol_binary.Make (Thrift_io)
