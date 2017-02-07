(* ****** ****** *)
//
// lambda8 - A Chip8 emulator written in ATS
// Author: Ryan King (rtking@bu.edu)
//
// key.dats: Handles keyboard input
//
(* ****** ****** *)

#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"

staload UN = "prelude/SATS/unsafe.sats"

staload "./../../../SDL2/SATS/SDL.sats"

#include "./memory.dats"

(* ****** ****** *)

fn quit(): void = (SDL_Quit(); exit 0)

(* ****** ****** *)

fn handle_key(event: SDL_Event): Option(int) =
  let
    val sym = GetKeysym(event)
    val sgn = (if event.type = SDL_KEYUP then 1 else ~1) : int
  in
      case+ 0 of
        | _ when sym = SDLK_1 => Some(0x1 * sgn)
        | _ when sym = SDLK_2 => Some(0x2 * sgn)
        | _ when sym = SDLK_3 => Some(0x3 * sgn)
        | _ when sym = SDLK_4 => Some(0xF * sgn)
        | _ when sym = SDLK_q => Some(0x4 * sgn)
        | _ when sym = SDLK_w => Some(0x5 * sgn)
        | _ when sym = SDLK_e => Some(0x6 * sgn)
        | _ when sym = SDLK_r => Some(0xE * sgn)
        | _ when sym = SDLK_a => Some(0x7 * sgn)
        | _ when sym = SDLK_s => Some(0x8 * sgn)
        | _ when sym = SDLK_d => Some(0x9 * sgn)
        | _ when sym = SDLK_f => Some(0xD * sgn)
        | _ when sym = SDLK_z => Some(0xA * sgn)
        | _ when sym = SDLK_x => Some(0x0 * sgn)
        | _ when sym = SDLK_c => Some(0xB * sgn)
        | _ when sym = SDLK_v => Some(0xC * sgn)
        | _ =>> None()
  end

(* ****** ****** *)

fn check(): Option(int) =
  let
    var event : SDL_Event?
    val res = SDL_PollEvent(event)
    prval () = if res > 0 then opt_unsome(event) else opt_unnone(event)
    val event = $UN.cast{SDL_Event}(event)
    val type = event.type
  in
    case+ 0 of
    | _ when type = SDL_QUIT => (quit(); None())
    | _ when type = SDL_KEYUP || type = SDL_KEYDOWN => handle_key(event)
    | _ =>> None()
  end

 (* End of [keys.dats] *)