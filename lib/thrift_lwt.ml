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

  type 'a wakener = 'a Lwt.u
  let wait = Lwt.wait
  let wakeup = Lwt.wakeup

  module Condition = struct
    type 'a t = 'a Lwt_condition.t
    let create = Lwt_condition.create
    let wait cond = Lwt_condition.wait cond
    let signal = Lwt_condition.signal
  end
end

module Thrift_protocol_binary = Thrift_protocol_binary.Make (Thrift_io)
