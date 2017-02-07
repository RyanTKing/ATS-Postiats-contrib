(* ****** ****** *)
//
// lambda8 - A Chip8 emulator written in ATS
// Author: Ryan King (rtking@bu.edu)
//
// chip8.dats; Chip8 Display
//
(* ****** ****** *)

#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
#include "share/HATS/atspre_staload_libats_ML.hats"
#include "share/HATS/atslib_staload_libats_libc.hats"

#include "./display.dats"
staload "libats/libc/SATS/stdlib.sats"

(* ****** ****** *)

val key_wait_reset = ref<bool>(false) : ref(bool)

val dec_timers = ref<bool>(false) : ref(bool)

val timer_t = ref<double>(0.) : ref(double)

val timer_late = ref<double>(0.) : ref(double)

val freq = 1. / 60. : double

(* ****** ****** *)

fn cpu_timerinterrupt(interval: uint, ptr): uint = (
  !dec_timers := true;
  interval
)

fn init_cputimer(): void = () where {
  val cpu_timer = SDL_AddTimer(i2u 17, cpu_timerinterrupt, the_null_ptr)
  val () = if t2u(cpu_timer) = 0 then $raise FailedToInitTimer()
  val () = assertloc(t2u(cpu_timer) > 0)
}

(* ****** ****** *)

fn load_game(game_fname: string): void = () where {
  fun load{n:nat}(game_data: list_vt(char, n), i: intLt(n)): void =
    case+ game_data of
      | ~nil_vt() => ()
      | ~cons_vt(b, rest) => (memory[i] := c2byte b; load(rest, succ i))

  val game_opt = fileref_open_opt(game_fname, file_mode_r)
  val () = if option_vt_is_none(game_opt) then $raise GameNotFound(game_fname)
  val () = assertloc(option_vt_is_some(game_opt))
  val ~Some_vt(game) = game_opt
  val game_data = fileref_get_file_charlst(game)
  val () = load(game_data, 0x200)
  val () = fileref_close(game)
}

(* ****** ****** *)

fn fetch{n,m:nat | n < mem_size}(): word = to_word(low, high) where {
  val low = memory[!pc] : byte
  val high = memory[succ(!pc)]
}

(* ****** ****** *)

#define FIRST(w) w2ui((w land 0xF000) lsr 12)
#define LAST(w)  w2ui(w land 0x000F)
#define LAST2(w) w2ui(w land 0x00FF)
#define NNN(w)   w2i(w land 0x0FFF)
#define NN(w)    w2byte(w land 0x00FF)
#define VX(w)    w2i((w land 0x0F00) lsr 8)
#define VY(w)    w2i((w land 0x00F0) lsr 4)
#define N(w)     w2byte(w land 0x000F)

fn decode(opcode: word): Opcode =
  case+ 0 of
    | _ when FIRST(opcode) = 0x0 => (
      case+ 0 of
        | _ when LAST2(opcode) = 0xE0 =>  OP00E0()
        | _ when LAST2(opcode) = 0xEE =>  OP00EE()
        | _                           =>> OP0NNN(NNN(opcode)))
    | _ when FIRST(opcode) = 0x0 =>  OP0NNN(NNN(opcode))
    | _ when FIRST(opcode) = 0x1 =>  OP1NNN(NNN(opcode))
    | _ when FIRST(opcode) = 0x2 =>  OP2NNN(NNN(opcode))
    | _ when FIRST(opcode) = 0x3 =>  OP3XNN(VX(opcode), NN(opcode))
    | _ when FIRST(opcode) = 0x4 =>  OP4XNN(VX(opcode), NN(opcode))
    | _ when FIRST(opcode) = 0x5 =>  OP5XY0(VX(opcode), VY(opcode))
    | _ when FIRST(opcode) = 0x6 =>  OP6XNN(VX(opcode), NN(opcode))
    | _ when FIRST(opcode) = 0x7 =>  OP7XNN(VX(opcode), NN(opcode))
    | _ when FIRST(opcode) = 0x8 => (
      case+ 0 of
        | _ when LAST(opcode) = 0x0 =>  OP8XY0(VX(opcode), VY(opcode))
        | _ when LAST(opcode) = 0x1 =>  OP8XY1(VX(opcode), VY(opcode))
        | _ when LAST(opcode) = 0x2 =>  OP8XY2(VX(opcode), VY(opcode))
        | _ when LAST(opcode) = 0x3 =>  OP8XY3(VX(opcode), VY(opcode))
        | _ when LAST(opcode) = 0x4 =>  OP8XY4(VX(opcode), VY(opcode))
        | _ when LAST(opcode) = 0x5 =>  OP8XY5(VX(opcode), VY(opcode))
        | _ when LAST(opcode) = 0x6 =>  OP8XY6(VX(opcode), VY(opcode))
        | _ when LAST(opcode) = 0x7 =>  OP8XY7(VX(opcode), VY(opcode))
        | _ when LAST(opcode) = 0xE =>  OP8XYE(VX(opcode), VY(opcode))
        | _                         =>> $raise UnknownOpcode(opcode))
    | _ when FIRST(opcode) = 0x9 => (
      case+ 0 of
        | _ when LAST(opcode) = 0x0 =>  OP9XY0(VX(opcode), VY(opcode))
        | _                         =>> $raise UnknownOpcode(opcode))
    | _ when FIRST(opcode) = 0xA    =>  OPANNN(NNN(opcode))
    | _ when FIRST(opcode) = 0xB    =>  OPBNNN(NNN(opcode))
    | _ when FIRST(opcode) = 0xC    =>  OPCXNN(VX(opcode), NN(opcode))
    | _ when FIRST(opcode) = 0xD    =>  OPDXYN(VX(opcode), VY(opcode), N(opcode))
    | _ when FIRST(opcode) = 0XE    => (
      case+ 0 of
        | _ when LAST2(opcode) = 0x9E =>  OPEX9E(VX(opcode))
        | _ when LAST2(opcode) = 0xA1 =>  OPEXA1(VX(opcode))
        | _                           =>> $raise UnknownOpcode(opcode))
    | _ when FIRST(opcode) = 0xF    => (
      case+ 0 of
        | _ when LAST2(opcode) = 0x07 => OPFX07(VX(opcode))
        | _ when LAST2(opcode) = 0x0A => OPFX0A(VX(opcode))
        | _ when LAST2(opcode) = 0x15 => OPFX15(VX(opcode))
        | _ when LAST2(opcode) = 0x18 => OPFX18(VX(opcode))
        | _ when LAST2(opcode) = 0x1E => OPFX1E(VX(opcode))
        | _ when LAST2(opcode) = 0x29 => OPFX29(VX(opcode))
        | _ when LAST2(opcode) = 0x33 => OPFX33(VX(opcode))
        | _ when LAST2(opcode) = 0x55 => OPFX55(VX(opcode))
        | _ when LAST2(opcode) = 0x65 => OPFX65(VX(opcode))
        | _                           =>> $raise UnknownOpcode(opcode))
    | _                             =>> $raise UnknownOpcode(opcode)

(* ****** ****** *)

fn execute(opcode: Opcode): void =
  let
    fn incr_pc(skip: bool): void =
      if skip then !pc := !pc + 4 else !pc := !pc + 2

    fn decr_timer(): void =
      if !dec_timers then begin
        if !sound_timer > 0 then !sound_timer := pred !sound_timer;
        if !delay_timer > 0 then !delay_timer := pred !delay_timer;
        !dec_timers := false
      end
  in
    case+ opcode of
      | OP0NNN(nnn)     => println!("Calls RCA 1802 program at address ", nnn)
      | OP00E0()        => (
          clear_screen();
          !draw_cnt := succ !draw_cnt;
          incr_pc false
        )
      | OP00EE()        => (
          case+ !stack of
            | nil() => $raise EmptyStack(opcode)
            | cons(next_pc, stack_rest) => (
                !pc := next_pc;
                !stack := stack_rest
              ))
      | OP1NNN(nnn)     => !pc := nnn
      | OP2NNN(nnn)     => (!stack := cons(!pc, !stack); !pc := nnn)
      | OP3XNN(x, nn)   => incr_pc(V[x] = nn)
      | OP4XNN(x, nn)   => incr_pc(V[x] != nn)
      | OP5XY0(x, y)    => incr_pc(V[x] = V[y])
      | OP6XNN(x, nn)   => (V[x] := nn; incr_pc false)
      | OP7XNN(x, nn)   => (V[x] := V[x] + nn; incr_pc false)
      | OP8XY0(x, y)    => (V[x] := V[y]; incr_pc false)
      | OP8XY1(x, y)    => (V[x] := V[x] lor V[y]; incr_pc false)
      | OP8XY2(x, y)    => (V[x] := V[x] land V[y]; incr_pc false)
      | OP8XY3(x, y)    => (V[x] := V[x] lxor V[y]; incr_pc false)
      | OP8XY4(x, y)    => (
          V[0xF] := (
            if V[x] > (i2byte 0xFF - V[y]) then i2byte 1 else i2byte 0
          );
          V[x] := V[x] + V[y];
          incr_pc false
        )
      | OP8XY5(x, y)    => (
          V[0xF] := (if V[x] > V[y] then i2byte 1 else i2byte 0);
          V[x] := V[x] - V[y];
          incr_pc false
        )
      | OP8XY6(x, y)    => (
          V[0xF] := V[x] land i2byte 0x1;
          V[x] := V[x] lsr 1;
          incr_pc false
        )
      | OP8XY7(x, y)    => (
          V[0xF] := (if V[y] > V[x] then i2byte 1 else i2byte 0);
          V[x] := V[y] - V[x];
          incr_pc false
        )
      | OP8XYE(x, y)    => (
        V[0xF] := (V[x] lsr 7) land i2byte 0x1;
        V[x] := V[x] lsl 1;
        incr_pc false
      )
      | OP9XY0(x, y)    => incr_pc(V[x] != V[y])
      | OPANNN(nnn)     => (!I := nnn; incr_pc false)
      | OPBNNN(nnn)     => (!pc := byte2uint0 V[0] + nnn)
      | OPCXNN(x, nn)   => (
          V[x] := (i2byte(rand() % 256) land nn);
          incr_pc false
        )
      | OPDXYN(x, y, n) => println!("Draws a 8xn sprite at (V[", x, "], V[", y, "])")
      | OPEX9E(x)       => println!("Skips next instruction if key stored at V[", x, "] is pressed")
      | OPEXA1(x)       => println!("Skips next instruction if key stored at V[", x, "] isn't pressed")
      | OPFX07(x)       => (V[x] := i2byte !delay_timer; incr_pc false)
      | OPFX0A(x)       => println!("A key press is awaited then stored in V[", x, "]")
      | OPFX15(x)       => (!delay_timer := byte2int0 V[x]; incr_pc false)
      | OPFX18(x)       => (!sound_timer := byte2int0 V[x]; incr_pc false)
      | OPFX1E(x)       => (
          V[0xF] := (
            if (!I + byte2uint0 V[x]) > 0xFFF then i2byte 1 else i2byte 0
          );
          !I := (!I + byte2uint0 V[x]) land i2u 0xFFF;
          incr_pc false
        )
      | OPFX29(x)       => (!I := byte2uint0 V[x] * i2u 5; incr_pc false)
      | OPFX33(x)       => (
          memory[!I] := V[x] / i2byte 100;
          memory[!I] := (V[x] % i2byte 100) / i2byte 10;
          memory[!I] := V[x] % i2byte 10
        )
      | OPFX55(x)       =>
        let
          fun store(i: int, n: uint):<cloref1> void =
            if i2byte n <= V[x] then
              (memory[i] := V[n]; store(i + 1, n + i2u 1))
          val () = store(w2i(!I), i2u 0)
        in
          incr_pc false
        end

      | OPFX65(x)       =>
        let
          fun fill(I: word, n: uint):<cloref1> void =
            if i2byte n <= V[x] then
              (V[n] := memory[w2i I]; fill(i2w(w2i(I) + 1), n + i2u(1)))
          val () = fill(!I, i2u 0)
        in
          incr_pc false
        end
  end

(* ****** ****** *)

fn draw_flag(): bool =
  if !draw_cnt >= 1 then (!draw_cnt := 0; true) else false

(* ****** ****** *)

fun main_loop(window: !SDL_Window_ptr1): void =
  let
    val opcode = fetch()
    val opcode = decode(opcode)
    val () = execute(opcode)
    val () = display(window)
    val key = check()
    val () = if option_is_some(key) then println!("KEY! ", option_unsome(key))
  in
    main_loop(window)
  end

(* ****** ****** *)

implement main0(argc, argv) = () where {
  val () = if argc != 2 then (
      if argc = 1 then fprintln!(stderr_ref, "lambda8: No game specified")
      else fprintln!(stderr_ref, "lambda8: Unknown arguments");
      exit(1)
    )
  val () = assertloc(argc = 2)
  val () = init_mem()
  val cpu = load_game(argv[1])
  val window = init_disp()
  val () = init_cputimer()
  val () = main_loop(window)
  val () = SDL_DestroyWindow window
}

(* End of [lambda8.dats] *)