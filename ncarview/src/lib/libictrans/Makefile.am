# This is an automake file for NCL.
# Ed Hartnett 9/6/18

# Find c.h.
AM_CPPFLAGS = -I$(top_srcdir)/common/src/libncarg_c

# Find cgm_tools.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarview/src/lib/libcgm

# Find ctrans.h.
AM_CPPFLAGS += -I$(top_srcdir)/ncarview/src/lib/libctrans

OBJS = commands.o cmd_tab.o get_cmd.o glob.o spooler.o ictrans.o	\
lex_stuff.o input.o lex.yy_linux.o ictrans_mem.o ictrans_wks.o		\
fort_ictrans.o

libictrans.o: $(OBJS)
	ld -r -o $@ $^

commands.o: commands.c
	$(CC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${CFLAGS} -o $@ $^
cmd_tab.o: cmd_tab.c
	$(CC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${CFLAGS} -o $@ $^
get_cmd.o: get_cmd.c
	$(CC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${CFLAGS} -o $@ $^
glob.o: glob.c
	$(CC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${CFLAGS} -o $@ $^
spooler.o: spooler.c
	$(CC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${CFLAGS} -o $@ $^
ictrans.o: ictrans.c
	$(CC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${CFLAGS} -o $@ $^
lex_stuff.o: lex_stuff.c
	$(CC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${CFLAGS} -o $@ $^
input.o: input.c
	$(CC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${CFLAGS} -o $@ $^
lex.yy_linux.o: lex.yy_linux.c
	$(CC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${CFLAGS} -o $@ $^
ictrans_mem.o: ictrans_mem.c
	$(CC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${CFLAGS} -o $@ $^
ictrans_wks.o: ictrans_wks.c
	$(CC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${CFLAGS} -o $@ $^
fort_ictrans.o: fort_ictrans.f
	$(FC) -c ${AM_CPPFLAGS} ${CPPFLAGS} ${DEFS} ${FFLAGS} -o $@ $^

# Install this "lib" file.
robjdir = $(prefix)/lib/ncarg/robj
robj_HEADERS = libictrans.o

EXTRA_DIST = commands.c cmd_tab.c get_cmd.c glob.c spooler.c	\
ictrans.c lex_stuff.c input.c lex.yy_linux.c ictrans_mem.c	\
ictrans_wks.c fort_ictrans.f

CLEANFILES = *.o
