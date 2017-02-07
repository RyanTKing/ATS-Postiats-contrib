/* ****** ****** */
//
// lambda8 - A Chip8 emulator written in ATS
// Author: Ryan King (rtking@bu.edu)
//
// lambda8.cats: C portions, mainly for interfacing with SDL
//
/* ****** ****** */

#ifndef LAMBDA8_CATS
#define LAMBDA8_CATS

typedef short unsigned int lambda8_word;

ATSinline()
atstype_byte lambda8_char2byte(atstype_char c) {
  return (atstype_byte) c;
}

ATSinline()
atstype_int lambda8_word2int(lambda8_word w) {
  return (atstype_int) w;
}

ATSinline()
atstype_uint lambda8_word2uint(lambda8_word w) {
  return (atstype_uint) w;
}

ATSinline()
atstype_byte lambda8_word2byte(lambda8_word w) {
  return (atstype_byte) w;
}

ATSinline()
lambda8_word lambda8_int2word(atstype_int i) {
  return (lambda8_word) i;
}

ATSinline()
lambda8_word lambda8_uint2word(atstype_int ui) {
  return (lambda8_word) ui;
}

ATSinline()
lambda8_word lambda8_to_word(atstype_byte low, atstype_byte high) {
  return (lambda8_word) ((uint) (low << 8) ^ (uint) high);
}

ATSinline()
atstype_bool lambda8_eq_word_int(lambda8_word w, atstype_int i) {
  return (atstype_int) w == i;
}

ATSinline()
lambda8_word lambda8_word_land_int(lambda8_word w, atstype_int i) {
  return (lambda8_word) (w & i);
}

ATSinline()
lambda8_word lambda8_word_lor_int(lambda8_word w, atstype_int i) {
  return (lambda8_word) (w | i);
}

ATSinline()
lambda8_word lambda8_word_lxor_int(lambda8_word w, atstype_int i) {
  return (lambda8_word) (w ^ i);
}

ATSinline()
lambda8_word lambda8_word_rshift(lambda8_word w, atstype_int i) {
  return (lambda8_word) (w >> i);
}

ATSinline()
atstype_bool lambda8_eq_byte_byte(atstype_byte b1, atstype_byte b2) {
  return (int) b1 == (int) b2;
}

ATSinline()
atstype_bool lambda8_neq_byte_byte(atstype_byte b1, atstype_byte b2) {
  return (int) b1 != (int) b2;
}

ATSinline()
atstype_byte lambda8_add_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 + b2;
}

ATSinline()
atstype_byte lambda8_sub_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 - b2;
}

ATSinline()
atstype_byte lambda8_mult_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 * b2;
}

ATSinline()
atstype_byte lambda8_div_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 / b2;
}

ATSinline()
atstype_byte lambda8_mod_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 % b2;
}

ATSinline()
atstype_byte lambda8_lor_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 | b2;
}

ATSinline()
atstype_byte lambda8_land_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 & b2;
}

ATSinline()
atstype_byte lambda8_lxor_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 ^ b2;
}

ATSinline()
atstype_byte lambda8_byte_rshift(atstype_byte b, atstype_int n) {
  return b >> n;
}

ATSinline()
atstype_byte lambda8_byte_lshift(atstype_byte b, atstype_int n) {
  return b << n;
}

ATSinline()
atstype_int lambda8_byte_to_int(atstype_byte b) {
  return (atstype_int) b;
}

ATSinline()
atstype_bool lambda8_gt_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 > b2;
}

ATSinline()
atstype_bool lambda8_lt_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 < b2;
}

ATSinline()
atstype_bool lambda8_gte_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 >= b2;
}

ATSinline()
atstype_bool lambda8_lte_byte_byte(atstype_byte b1, atstype_byte b2) {
  return b1 <= b2;
}

#endif

/* End of [lambda8.cats] */
