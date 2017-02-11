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
  fun load{n:nat}(game_data: list_vt(char, n), i: c8_addr): void =
    case+ game_data of
      | ~nil_vt() => ()
      | ~cons_vt(c, rest) => (
          memory[i] := c2c8b(c);
          if i < MEM_SIZE - 1 then load(rest, succ(i)) else free(rest)
        )

  val game_opt = fileref_open_opt(game_fname, file_mode_r)
  val () = if option_vt_is_none(game_opt) then $raise GameNotFound(game_fname)
  val () = assertloc(option_vt_is_some(game_opt))
  val ~Some_vt(game) = game_opt
  val game_data = fileref_get_file_charlst(game)
  val () = load(game_data, 0x200)
  val () = fileref_close(game)
}

(* ****** ****** *)

fn fetch(): c8_word = (high lsl 16) lor low where {
  val high_addr = !pc
  val low_addr = succ(!pc)
  val high = b2w(memory[!pc]) : c8_word
  val low = if low_addr >= MEM_SIZE then $raise IllegalPC(!pc) else b2w(memory[low_addr]) : c8_word
}

(* ****** ****** *)

fn decode(opcode: c8_word): Opcode =
  let
    fn word_to_addr(w: c8_word): c8_addr =
      if w >= MEM_SIZE then $raise IllegalMemoryAddress(w)
      else c8w2c8a(w)

    fn word_to_reg(w: c8_word): c8_reg =
      if w >= NUM_REGS then $raise IllegalRegister(w2i(w))
      else c8w2c8r(w)

    macdef FIRST(w) = ((,(w) land w000F) lsr 12)
    macdef LAST(w) = (,(w) land w000F)
    macdef LAST2(w) = (,(w) land w00FF)
    macdef NNN(w) = word_to_addr(,(w) land w0FFF)
    macdef NN(w) = (,(w) land w00FF)
    macdef VX(w) = word_to_reg((,(w) land w0F00) lsr 8)
    macdef VY(w) = word_to_reg((,(w) land w00F0) lsr 4)
    macdef N(w) = (,(w) land w000F)
  in
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
  end

(* ****** ****** *)

fn add_c8_byte_c8_addr(b: c8_byte, a: c8_addr): c8_addr =
  let
    val w1 = b2w(b)
    val w2 = c8a2c8w(a)
    val sum = w1 + w2
    val () = if sum >= MEM_SIZE then $raise IllegalMemoryAddress(sum)
   in
    c8w2c8a(sum)
   end

overload + with add_c8_byte_c8_addr

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

    macdef VF_toggle(b) = if (,(b)) then V[0xF] := b1 else V[0xF] := b0

    fn get_reg(i: int): c8_reg =
      if i >= NUM_REGS then $raise IllegalRegister(i)
      else i2c8r(i)
  in
    case+ opcode of
      | OP0NNN(nnn)     => println!("Calls RCA 1802 program at address ", nnn)
      | OP00E0()        => (
          clear_screen();
          !draw_cnt := succ !draw_cnt;
          incr_pc(false)
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
      | OP6XNN(x, nn)   => (V[x] := nn; incr_pc(false))
      | OP7XNN(x, nn)   => (V[x] := V[x] + nn; incr_pc(false))
      | OP8XY0(x, y)    => (V[x] := V[y]; incr_pc(false))
      | OP8XY1(x, y)    => (V[x] := V[x] lor V[y]; incr_pc(false))
      | OP8XY2(x, y)    => (V[x] := V[x] land V[y]; incr_pc(false))
      | OP8XY3(x, y)    => (V[x] := V[x] lxor V[y]; incr_pc(false))
      | OP8XY4(x, y)    => (
          VF_toggle(V[x] > (w00FF - V[y]));
          V[x] := V[x] + V[y];
          incr_pc(false)
        )
      | OP8XY5(x, y)    => (
          VF_toggle(V[x] > V[y]);
          V[x] := V[x] - V[y];
          incr_pc(false)
        )
      | OP8XY6(x, y)    => (
          V[0xF] := V[x] land b1;
          V[x] := V[x] lsr 1;
          incr_pc(false)
        )
      | OP8XY7(x, y)    => (
          VF_toggle(V[y] > V[x]);
          V[x] := V[y] - V[x];
          incr_pc(false)
        )
      | OP8XYE(x, y)    => (
        V[0xF] := (V[x] lsr 7) land b1;
        V[x] := V[x] lsl 1;
        incr_pc(false)
      )
      | OP9XY0(x, y)    => incr_pc(V[x] != V[y])
      | OPANNN(nnn)     => (!I := nnn; incr_pc(false))
      | OPBNNN(nnn)     => (!pc := V[0] + nnn)
      | OPCXNN(x, nn)   => (
          V[x] := (i2b(rand() % 256) land nn);
          incr_pc(false)
        )
      | OPDXYN(x, y, n) => println!("Draws a 8xn sprite at (V[", x, "], V[", y, "])")
      | OPEX9E(x)       => println!("Skips next instruction if key stored at V[", x, "] is pressed")
      | OPEXA1(x)       => println!("Skips next instruction if key stored at V[", x, "] isn't pressed")
      | OPFX07(x)       => (V[x] := !delay_timer; incr_pc(false))
      | OPFX0A(x)       => println!("A key press is awaited then stored in V[", x, "]")
      | OPFX15(x)       => (!delay_timer := V[x]; incr_pc(false))
      | OPFX18(x)       => (!sound_timer := V[x]; incr_pc(false))
      | OPFX1E(x)       => (
          !I := V[x] + !I;
          incr_pc(false)
        )
      | OPFX29(x)       => (!I := c8b2c8a(V[x] * i2b(5)); incr_pc(false))
      | OPFX33(x)       => (
          memory[!I] := V[x] / i2b(100);
          memory[!I] := (V[x] % i2b(100)) / i2b(10);
          memory[!I] := V[x] % i2b(10)
        )
      | OPFX55(x)       => (incr_pc(false); store(!I, 0)) where {
          fun store(a: c8_addr, n: int):<cloref1> void =
            if i2b(n) <= V[x] then (
              memory[a] := V[get_reg(n)];
              store(succ(a), n + 1)
            )
        }

      | OPFX65(x)       => (incr_pc(false); fill(!I, 0)) where {
          fun fill(a: c8_addr, n: int):<cloref1> void =
            if i2b(n) <= V[x] then (
              V[get_reg(n)] := memory[a];
              fill(succ(a), n + 1)
            )
        }
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
  val () = load_font()
  val cpu = load_game(argv[1])
  val window = init_disp()
  val () = init_cputimer()
  val () = main_loop(window)
  val () = SDL_DestroyWindow window
}

(* End of [lambda8.dats] *)