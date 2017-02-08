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

(* ****** ****** *)

macdef i2b(i) = g0int2uint<intknd,uint8knd>(,(i))
macdef b2w(i) = g0uint2uint<uint8knd,uint16knd>(,(i))
macdef b2i(i) = g0uint2int<uint8knd,intknd>(,(i))
macdef b2ui(i) = g0uint2uint<uint8knd,uintknd>(,(i))
macdef i2w(i) = g0int2uint<intknd,uint16knd>(,(i))
macdef w2i(i) = g0uint2int<uint16knd,intknd>(,(i))

macdef b0 = i2b(0)
macdef b1 = i2b(1)
macdef w0 = i2w(0)
macdef w1 = i2w(1)
macdef w000F = i2w(0x000F)
macdef w00F0 = i2w(0x00F0)
macdef w0F00 = i2w(0x0F00)
macdef wF000 = i2w(0xF000)
macdef w00FF = i2w(0x00FF)
macdef w0FF0 = i2w(0x0FF0)
macdef wFF00 = i2w(0xFF00)
macdef w0FFF = i2w(0x0FFF)
macdef wFFF0 = i2w(0xFFF0)
macdef wFFFF = i2w(0xFFFF)


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

typedef c8_byte = uint8
typedef c8_word = uint16
typedef c8_addr = [a:int | 0 <= a; a < mem_size] int(a)
typedef c8_reg = [a: int | 0 <= a; a < num_regs] int(a)

// overload land with g0uint_land_uint16
overload lsr with g0uint_lsr_uint16
overload lsl with g0uint_lsl_uint16

(* ****** ****** *)

datatype Opcode =
  | OP0NNN of (c8_addr)
  | OP00E0 of ()
  | OP00EE of ()
  | OP1NNN of (c8_addr)
  | OP2NNN of (c8_addr)
  | OP3XNN of (c8_reg, c8_byte)
  | OP4XNN of (c8_reg, c8_byte)
  | OP5XY0 of (c8_reg, c8_reg)
  | OP6XNN of (c8_reg, c8_byte)
  | OP7XNN of (c8_reg, c8_byte)
  | OP8XY0 of (c8_reg, c8_reg)
  | OP8XY1 of (c8_reg, c8_reg)
  | OP8XY2 of (c8_reg, c8_reg)
  | OP8XY3 of (c8_reg, c8_reg)
  | OP8XY4 of (c8_reg, c8_reg)
  | OP8XY5 of (c8_reg, c8_reg)
  | OP8XY6 of (c8_reg, c8_reg)
  | OP8XY7 of (c8_reg, c8_reg)
  | OP8XYE of (c8_reg, c8_reg)
  | OP9XY0 of (c8_reg, c8_reg)
  | OPANNN of (c8_addr)
  | OPBNNN of (c8_addr)
  | OPCXNN of (c8_reg, uint)
  | OPDXYN of (c8_reg, c8_reg, c8_byte)
  | OPEX9E of (c8_reg)
  | OPEXA1 of (c8_reg)
  | OPFX07 of (c8_reg)
  | OPFX0A of (c8_reg)
  | OPFX15 of (c8_reg)
  | OPFX18 of (c8_reg)
  | OPFX1E of (c8_reg)
  | OPFX29 of (c8_reg)
  | OPFX33 of (c8_reg)
  | OPFX55 of (c8_reg)
  | OPFX65 of (c8_reg)

(* ****** ****** *)

exception DisplayNotFound of ()
exception UnknownOpcode of (c8_word)
exception EmptyStack of (Opcode)
exception GameNotFound of (string)
exception FailedToInitTimer of ()
exception IllegalOperation of ()
exception IllegalRegister of (c8_word)
exception IllegalRegister of (int)
exception IllegalMemoryAddress of (c8_word)

(* ****** ****** *)

fun char2byte(char): byte = "mac#%"

castfn c8_byte_of_char(char):<> c8_byte
castfn c8_addr_of_c8_word(c8_word):<> c8_addr
castfn c8_reg_of_c8_word(c8_word):<> c8_reg
castfn c8_word_of_c8_addr(c8_addr):<> c8_word
castfn c8_addr_of_c8_byte(c8_byte):<> c8_addr
castfn c8_reg_of_int(int):<> c8_reg

symintr c2c8b c8w2c8a c8w2c8r c8a2c8w c8w2c8a i2c8r
overload c2c8b with c8_byte_of_char of 0
overload c8w2c8a with c8_addr_of_c8_word of 0
overload c8w2c8r with c8_reg_of_c8_word of 0
overload i2c8r with c8_reg_of_int of 0
overload c8a2c8w with c8_word_of_c8_addr of 0
overload c8b2c8a with c8_addr_of_c8_byte of 0

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

symintr u2w w2i w2ui w2byte
overload w2i with word2int
overload w2ui with word2uint
overload w2byte with word2byte
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
// overload lsr with word_rshift
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
// overload lor with lor_byte_byte
// overload land with land_byte_byte
// overload lxor with lxor_byte_byte
// overload lsr with byte_rshift
// overload lsl with byte_lshift


(* End of [lambda8.stas] *)