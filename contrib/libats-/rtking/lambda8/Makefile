############################################
#
# lambda8 - a Chip8 emulator written in ATS
# Author: Ryan King (rtking@bu.edu)
#

# ATS System Locations

PATSHOMEQ="$(HOME)/Projects/ATS-Postiats"
PATSHOMERELOCQ="$(HOME)/Projects/ATS-Postiats-contrib"

# Compilers

CC=gcc
PATSCCOMP = $(CC) -std=c99 -D_XOPEN_SOURCE

PATSCC=$(PATSHOMEQ)/bin/patscc
PATSOPT=$(PATSHOMEQ)/bin/patsopt
PATSLIB=$(PATSHOMEQ)/ccomp/atslib/lib

# Includes

INCLUDE=-I$(PATSHOMEQ) -I$(PATSHOMEQ)/ccomp/runtime

# C Flags

CFLAGS += -D_GNU_SOURCE

# Linker Flags

LDFLAGS += -L$(PATSLIB)
LDFLAGS += -latslib

# Extra flags

EXTRAFLAGS=`sdl2-config --cflags --libs`

# Malloc Flag

MALLOCFLAG := -DATS_MEMALLOC_LIBC

INCLUDE += -I$(PATSHOMERELOC)/contrib

all :: \
lambda8
lambda8 :: \
lambda8.dats; \
$(PATSCC) -o $@ $< $(CFLAGS) $(LDFLAGS) $(MALLOCFLAG) $(EXTRAFLAGS) $(INCLUDE)

clean:: ; rm -f *~
clean:: ; rm -f *_?ats.c

cleanall:: clean
cleanall:: ; rm lambda8
