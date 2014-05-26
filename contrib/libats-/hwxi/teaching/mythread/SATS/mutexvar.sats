(***********************************************************************)
(*                                                                     *)
(*                       ATS/contrib/libats-hwxi                       *)
(*                                                                     *)
(***********************************************************************)

(*
** Copyright (C) 2014 Hongwei Xi, ATS Trustful Software, Inc.
**
** Permission is hereby granted, free of charge, to any person obtaining a
** copy of this software and associated documentation files (the "Software"),
** to deal in the Software without restriction, including without limitation
** the rights to use, copy, modify, merge, publish, distribute, sublicense,
** and/or sell copies of the Software, and to permit persons to whom the
** Software is furnished to do so, subject to the following stated conditions:
** 
** The above copyright notice and this permission notice shall be included in
** all copies or substantial portions of the Software.
** 
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
** OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
** FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
** THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
** LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
** FROM OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
** IN THE SOFTWARE.
*)

(* ****** ****** *)
//
// HX-2014-05:
// This is based on mutex
//
(* ****** ****** *)
//
abstype
mutexvar_type (a:vt@ype, int) = ptr
typedef mutexvar (a:vt0p, i) = mutexvar_type (a, i)
//
absvtype mutexvar_ticket_vtype = ptr
vtypedef mutexvar_ticket = mutexvar_ticket_vtype
//
(* ****** ****** *)

fun{a:vt0p}
mutexvar_create_exn ((*void*)): mutexvar (a, 0)

(* ****** ****** *)
//
fun{a:vt0p}
mutexvar_initiate
(
  mutexvar: !mutexvar (a, 0) >> mutexvar (a, 1)
) : mutexvar_ticket
//
(* ****** ****** *)
//
fun{a:vt0p}
mutexvar_waitfor
  (mutexvar: !mutexvar (a, 1) >> mutexvar (a, 0)): (a)
//
(* ****** ****** *)

fun{a:vt0p}
mutexvar_ticket_put (mvt: mutexvar_ticket, x: a): void

(* ****** ****** *)

(* end of [mutexvar.sats] *)