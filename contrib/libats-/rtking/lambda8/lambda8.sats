(* ****** ****** *)
//
// lambda8 - A Chip8 emulator written in ATS
// Author: Ryan King (rtking@bu.edu)
//
// lambda8.sats: Variable and function implications
//
(* ****** ****** *)

%{#
#include "lambda8.cats"
%}

#define ATS_PACKNAME      "LAMBDA8"
#define ATS_EXTERN_PREFIX "lambda8_"

staload "./../../../SDL2/SATS/SDL.sats"
staload "./SDL_Extra.sats"

(* ****** ****** *)

(* Memory *)
#define MEM_SIZE 0x1000
#define PC_START  0x200

(* Screen *)
#define SCR_WIDTH   64
#define SCR_HEIGHT  32
#define SCALE       10
#define SPR_WIDTH    8

(* Keyboard *)
#define NUM_KEYS    16

#define NUM_CHARS   80

#define NUM_REGS    16

(* A Chip8 word is 2 bytes *)
typedef word = $extype"lambda8_word"

(* ****** ****** *)

(* ****** ****** *)

stadef mem_size = 4096
stadef scr_width = 64
stadef scr_height = 32
stadef num_keys = 16
stadef num_chars = 80
stadef num_regs = 16

typedef c8_addr = [a:int | 0 <= a; a < mem_size] int(a)

(* ****** ****** *)

datatype Opcode =
  | OP0NNN of (c8_addr)
  | OP00E0 of ()
  | OP00EE of ()
  | OP1NNN of (intLt(mem_size))
  | OP2NNN of (intLt(mem_size))
  | OP3XNN of (intLt(num_regs), byte)
  | OP4XNN of (intLt(num_regs), byte)
  | OP5XY0 of (intLt(num_regs), intLt(num_regs))
  | OP6XNN of (intLt(num_regs), byte)
  | OP7XNN of (intLt(num_regs), byte)
  | OP8XY0 of (intLt(num_regs), intLt(num_regs))
  | OP8XY1 of (intLt(num_regs), intLt(num_regs))
  | OP8XY2 of (intLt(num_regs), intLt(num_regs))
  | OP8XY3 of (intLt(num_regs), intLt(num_regs))
  | OP8XY4 of (intLt(num_regs), intLt(num_regs))
  | OP8XY5 of (intLt(num_regs), intLt(num_regs))
  | OP8XY6 of (intLt(num_regs), intLt(num_regs))
  | OP8XY7 of (intLt(num_regs), intLt(num_regs))
  | OP8XYE of (intLt(num_regs), intLt(num_regs))
  | OP9XY0 of (intLt(num_regs), intLt(num_regs))
  | OPANNN of (uint)
  | OPBNNN of (uint)
  | OPCXNN of (uint, byte)
  | OPDXYN of (uint, uint, byte)
  | OPEX9E of (uint)
  | OPEXA1 of (uint)
  | OPFX07 of (uint)
  | OPFX0A of (uint)
  | OPFX15 of (uint)
  | OPFX18 of (uint)
  | OPFX1E of (uint)
  | OPFX29 of (uint)
  | OPFX33 of (uint)
  | OPFX55 of (uint)
  | OPFX65 of (uint)

(* ****** ****** *)

exception DisplayNotFound of ()
exception UnknownOpcode of (word)
exception EmptyStack of (Opcode)
exception GameNotFound of (string)
exception FailedToInitTimer of ()
exception IllegalOperation of ()
exception IllegalMemoryAddress of (word)

(* ****** ****** *)

fun char2byte(char): byte = "mac#%"

symintr c2byte
overload c2byte with char2byte of 0

(* ****** ****** *)

fun to_word(byte, byte): word = "mac#%"
fun word2int(w: word): int = "mac#%"
fun word2uint(w: word): uint = "mac#%"
fun word2byte(w: word): byte = "mac#%"
fun int2word(i: int): word = "mac#%"
fun uint2word(u: uint): word = "mac#%"

(* ****** ****** *)

symintr i2w u2w w2i w2ui w2byte
overload w2i with word2int
overload w2ui with word2uint
overload w2byte with word2byte
overload i2w with int2word
overload u2w with uint2word

(* ****** ****** *)

fun eq_word_int(w: word, i: int): bool = "mac#%"
fun word_land_int(w: word, i: int): word = "mac#%"
fun word_lor_int(w: word, i: int): word = "mac#%"
fun word_lxor_int(w: word, i: int): word = "mac#%"
fun word_rshift(w:word, i: int): word = "mac#%"
fun eq_byte_byte(b1: byte, b2: byte): bool = "mac#%"
fun neq_byte_byte(b1: byte, b2: byte): bool = "mac#%"
fun gt_byte_byte(b1: byte, b2: byte): bool = "mac#%"
fun lt_byte_byte(b1: byte, b2: byte): bool = "mac#%"
fun gte_byte_byte(b1: byte, b2: byte): bool = "mac#%"
fun lte_byte_byte(b1: byte, b2: byte): bool = "mac#%"
fun add_byte_byte(b1: byte, b2: byte): byte = "mac#%"
fun sub_byte_byte(b1: byte, b2: byte): byte = "mac#%"
fun mult_byte_byte(b1: byte, b2: byte): byte = "mac#%"
fun div_byte_byte(b1: byte, b2: byte): byte = "mac#%"
fun mod_byte_byte(b1: byte, b2: byte): byte = "mac#%"
fun lor_byte_byte(b1: byte, b2: byte): byte = "mac#%"
fun land_byte_byte(b1: byte, b2: byte): byte = "mac#%"
fun lxor_byte_byte(b1: byte, b2: byte): byte = "mac#%"
fun byte_rshift(b: byte, n: int): byte = "mac#%"
fun byte_lshift(b: byte, n: int): byte = "mac#%"

(* ****** ****** *)

overload = with eq_word_int
overload land with word_land_int
overload lor with word_lor_int
overload lxor with word_lxor_int
overload lsr with word_rshift
overload = with eq_byte_byte
overload != with neq_byte_byte
overload > with gt_byte_byte
overload < with lt_byte_byte
overload >= with gte_byte_byte
overload <= with lte_byte_byte
overload + with add_byte_byte
overload - with sub_byte_byte
overload * with mult_byte_byte
overload / with div_byte_byte
overload % with mod_byte_byte
overload lor with lor_byte_byte
overload land with land_byte_byte
overload lxor with lxor_byte_byte
overload lsr with byte_rshift
overload lsl with byte_lshift


(* End of [lambda8.stas] *)