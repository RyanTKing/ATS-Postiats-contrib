(* ****** ****** *)
//
// lambda8 - A Chip8 emulator written in ATS
// Author: Ryan King (rtking@bu.edu)
//
// display.dats; Chip8 Display
//
(* ****** ****** *)

#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"

#include "./key.dats"

(* ****** ****** *)

val draw_cnt = ref<int>(0) : ref(int)

(* ****** ****** *)

fn display(window: !SDL_Window_ptr1): void = if !draw_cnt > 0 then () where {
  fun draw_x{c:nat; c <= scr_width}.<scr_width-c>.
  (screen: !SDL_Surface_ptr1, x: int(c), y: intBtw(0,scr_height)): void =
    if x < SCR_WIDTH then () where {
      val () = assertloc(y < SCR_HEIGHT)
      val col = if matrixref_get_at_int(scr, y, SCR_WIDTH, x) = 1 then i2u(0xFFFFFF) else i2u(0x000000)
      var imgrect : SDL_Rect
      val () = imgrect.x := x * SCALE and () = imgrect.y := y * SCALE
      val () = imgrect.w := SCALE and () = imgrect.h := SCALE
      val err = SDL_FillRect(screen, cptr_rvar(imgrect), col)
      val () = draw_x(screen, succ(x), y)
    }

  fun draw_y{r:nat; r <= scr_height}
  (screen: !SDL_Surface_ptr1, y: int(r)): void =
    if y < SCR_HEIGHT then (draw_x(screen, 0, y); draw_y(screen, succ(y)))

  val (fpf | screen) = SDL_GetWindowSurface(window)
  val () = assertloc(ptrcast screen > 0)
  val () = draw_y(screen, 0)
  prval () = fpf(screen)
  val err = SDL_UpdateWindowSurface(window)
  val () = !draw_cnt := 0
}

(* ****** ****** *)

fn init_disp(): SDL_Window_ptr1 = window where {
  val err = SDL_Init(SDL_INIT_EVERYTHING)
  val () = assertloc(err >= 0)
  val W = SCR_WIDTH * SCALE and H = SCR_HEIGHT * SCALE
  val x = SDL_WINDOWPOS_UNDEFINED and y = SDL_WINDOWPOS_UNDEFINED
  val window = SDL_CreateWindow("lambda8", x, y, W, H, SDL_WINDOW_SHOWN)
  val () = if ptrcast window = 0 then $raise DisplayNotFound()
  val () = assertloc(ptrcast(window) > 0)
  val () = display(window)
}

(* End of [display.dats] *)