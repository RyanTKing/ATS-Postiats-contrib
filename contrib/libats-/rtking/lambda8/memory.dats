(* ****** ****** *)
//
// lambda8 - A Chip8 emulator written in ATS
// Author: Ryan King (rtking@bu.edu)
//
// lambda8.sats: Chip8 virtual memory design
//
(* ****** ****** *)

#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"

staload "./SDL_Extra.sats"
staload "./lambda8.sats"

(* ****** ****** *)

macdef i2b(i) = g0int2uint<intknd,uint8knd>(,(i))
macdef b0 = i2b(0)

// 16 CPU Registers
val V = arrayref_make_elt<c8_byte>(i2sz(NUM_REGS), b0): arrayref(c8_byte, num_regs)

// Address Register
val I = ref<c8_addr>(0) : ref(c8_addr)

// Program Counter
val pc = ref<c8_addr>(PC_START) : ref(c8_addr)

// Main memory
val memory = arrayref_make_elt<c8_byte>(
  i2sz(MEM_SIZE),
  b0
) : arrayref(c8_byte, mem_size)

// The Stack
val stack = ref(nil) : ref(list(c8_addr, 0))

// Representation of the Screen
val scr = matrixref_make_elt<int>(
  i2sz(SCR_HEIGHT),
  i2sz(SCR_WIDTH),
  0
) : matrixref(int, scr_height, scr_width)

// Delay  Timer
val delay_timer = ref<int>(0) : ref(int)

// Sound Timer
val sound_timer = ref<int>(0) : ref(int)

// Keys
val keys = arrayref_make_elt<bool>(
  i2sz(NUM_KEYS),
  false
) : arrayref(bool, num_keys)

// Chip8 Font
val chip8_font = (arrszref)$arrpsz{int}(
    0xF0, 0x90, 0x90, 0x90, 0xF0, 0x20, 0x60, 0x20, 0x20, 0x70,
    0xF0, 0x10, 0xF0, 0x80, 0xF0, 0xF0, 0x10, 0xF0, 0x10, 0xF0,
    0x90, 0x90, 0xF0, 0x10, 0x10, 0xF0, 0x80, 0xF0, 0x10, 0xF0,
    0xF0, 0x80, 0xF0, 0x90, 0xF0, 0xF0, 0x10, 0x20, 0x40, 0x40,
    0xF0, 0x90, 0xF0, 0x90, 0xF0, 0xF0, 0x90, 0xF0, 0x10, 0xF0,
    0xF0, 0x90, 0xF0, 0x90, 0x90, 0xE0, 0x90, 0xE0, 0x90, 0xE0,
    0xF0, 0x80, 0x80, 0x80, 0xF0, 0xE0, 0x90, 0x90, 0x90, 0xE0,
    0xF0, 0x80, 0xF0, 0x80, 0xF0, 0xF0, 0x80, 0xF0, 0x80, 0x80)

(* ****** ****** *)

fn{a:t0p} clear_array{n:nat}(arr: arrayref(a, n), init: a, n: int(n)): void =
  loop(0) where {
    fun loop{m:nat; m <= n}.<n-m>.(i: int(m)): void =
      if i < n then (arr[i] := init; loop(i + 1))
  }

(* ****** ****** *)

fn clear_screen(): void = clear_row(0) where {
  fun clear_col{m:nat | m <= scr_width}.<scr_width-m>.
  (r: intBtw(0, scr_height), c: int(m)): void =
      if c < SCR_WIDTH then (
        matrixref_set_at_int(scr, r, SCR_WIDTH, c, 0);
        clear_col(r, succ(c))
      )

  fun clear_row{n:nat; n <= scr_height}.<scr_height-n>.(r: int(n)): void =
    if r < SCR_HEIGHT then (
      clear_col(r, 0);
      clear_row(succ(r))
    )
}

(* ****** ****** *)

fn load_font(): void = aux(0) where {
  fun aux{m:nat; m <= num_chars}.<num_chars-m>.(i: int(m)): void =
    if i < NUM_CHARS then (memory[i] := i2b(chip8_font[i]); aux(succ(i)))
}

(* End of [memory.dats] *)