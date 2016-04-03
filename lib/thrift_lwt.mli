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

module Thrift_io : Thrift_sig.Io

module Thrift_transport : sig
  val of_input_channel :
    ?close: bool -> Lwt_io.input_channel -> (module Thrift_sig.In_transport)
  val of_output_channel :
    ?close: bool -> Lwt_io.output_channel -> (module Thrift_sig.Out_transport)
end

module Thrift_protocol_binary : Thrift_sig.Protocol_functor
