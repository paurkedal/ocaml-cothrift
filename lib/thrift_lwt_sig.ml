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

type 'a io = 'a Lwt.t

module type Io = Thrift_sig.Io with type 'a io = 'a io
module type In_transport = Thrift_sig.In_transport with type 'a io := 'a io
module type Out_transport = Thrift_sig.Out_transport with type 'a io := 'a io
module type Protocol = Thrift_sig.Protocol with type 'a io := 'a io
module type Processor = Thrift_sig.Processor
