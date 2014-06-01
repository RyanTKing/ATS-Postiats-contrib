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

(*
** HX-2014-05-22: Start it now!
*)

(* ****** ****** *)

abstype
workshop_type(a:vt@ype) = ptr
typedef
workshop(a:vt0p) = workshop_type(a)

(* ****** ****** *)

fun{
a:vt0p
} workshop_create_cap (cap: size_t): workshop(a)

(* ****** ****** *)

fun{}
workshop_get_capacity{a:vt0p}(ws: workshop(a)): size_t

(* ****** ****** *)

fun{
a:vt0p
} workshop_handle_job (ws: workshop(a), x: a): int

(* ****** ****** *)

fun{
a:vt0p
} workshop_insert_job (workshop(a), a): void // block?

(* ****** ****** *)

fun{
a:vt0p
} workshop_takeout_job (ws: workshop(a)): (a) // block?

(* ****** ****** *)

fun{
a:vt0p
} workshop_add_worker (ws: workshop(a)): int(*err*)

(* ****** ****** *)

(* end of [workshop.sats] *)
