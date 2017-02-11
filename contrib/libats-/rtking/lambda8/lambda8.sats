(* ****** ****** *)
//
// lambda8 - A Chip8 emulator written in ATS
// Author: Ryan King (rtking@bu.edu)
//
// lambda8.sats: Variable and function implications
//
(* ****** ****** *)

#define ATS_PACKNAME      "LAMBDA8"
#define ATS_EXTERN_PREFIX "lambda8_"

staload "./../../../SDL2/SATS/SDL.sats"
staload "./SDL_Extra.sats"

(*- CONSTANT DEFINITIONS -----------------------------------------------------*)

#define NUM_REGS    0xF // Number of registers (0x0 - 0xF)
#define MEM_SIZE 0x1000 // Size of the memory (4096 bytes)
#define NUM_KEYS    0xF // Number of keys (0-0xF)
#define NUM_CHARS  0x50 // Number of chars in the font
#define PC_START  0x200 // First location of the Program Counter
#define SCR_WIDTH    64 // Width of the Screen in pixels
#define SCR_HEIGHT   32 // Height of the Screen in pixels
#define SCALE        10 // Scale of the screen for display
#define SPR_WIDTH     8 // Width of a sprite

(*- STATIC DEFINITIONS -------------------------------------------------------*)

stadef num_regs   = NUM_REGS   // Number of registers
stadef mem_size   = MEM_SIZE   // Size of the memory
stadef num_keys   = NUM_KEYS   // Number of keys
stadef num_chars  = NUM_CHARS  // Number of chars in the font
stadef scr_width  = SCR_WIDTH  // Width of the screen
stadef scr_height = SCR_HEIGHT // Height of the screen

(*- TYPE DEFINITIONS ---------------------------------------------------------*)

typedef c8_byte = uint8               // A single byte (memory and regs)
typedef c8_word = uint16              // A word (opcodes)
typedef c8_addr = intBtw(0, mem_size) // A memory address
typedef c8_reg  = intBtw(0, num_regs) // A register number

(* ****** ****** *)

// The different Chip8 Opcodes
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

(*- TYPE CAST FUNCTIONS ------------------------------------------------------*)

castfn c8_byte_of_char(char):<> c8_byte
castfn c8_addr_of_c8_word(c8_word):<> c8_addr
castfn c8_reg_of_c8_word(c8_word):<> c8_reg
castfn c8_word_of_c8_addr(c8_addr):<> c8_word
castfn c8_addr_of_c8_byte(c8_byte):<> c8_addr
castfn c8_reg_of_int(int):<> c8_reg

(* ****** ****** *)

symintr c2c8b c8w2c8a c8w2c8r c8a2c8w c8w2c8a i2c8r
overload c2c8b with c8_byte_of_char of 0
overload c8w2c8a with c8_addr_of_c8_word of 0
overload c8w2c8r with c8_reg_of_c8_word of 0
overload i2c8r with c8_reg_of_int of 0
overload c8a2c8w with c8_word_of_c8_addr of 0
overload c8b2c8a with c8_addr_of_c8_byte of 0

(* ****** ****** *)

macdef i2b(i) = g0int2uint<intknd,uint8knd>(,(i))
macdef b2w(i) = g0uint2uint<uint8knd,uint16knd>(,(i))
macdef b2i(i) = g0uint2int<uint8knd,intknd>(,(i))
macdef b2ui(i) = g0uint2uint<uint8knd,uintknd>(,(i))
macdef i2w(i) = g0int2uint<intknd,uint16knd>(,(i))
macdef w2i(i) = g0uint2int<uint16knd,intknd>(,(i))

(* ****** ****** *)

overload lsr with g0uint_lsr_uint16
overload lsl with g0uint_lsl_uint16

(*- COMMON VALUE MACROS ------------------------------------------------------*)

macdef b0 = i2b(0)
macdef b1 = i2b(1)

(* ****** ****** *)

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

(*- EXCEPTIONS ---------------------------------------------------------------*)

exception DisplayNotFound of ()
exception UnknownOpcode of (c8_word)
exception EmptyStack of (Opcode)
exception GameNotFound of (string)
exception BadGameFile of (string)
exception FailedToInitTimer of ()
exception IllegalOperation of ()
exception IllegalRegister of (int)
exception IllegalMemoryAddress of (c8_word)
exception IllegalPC of (c8_addr)

(* End of [lambda8.stas] *)