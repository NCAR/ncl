# This is an automake file for NCL.
# Ed Hartnett 2/21/2019

#MAN1	= fontc graphc ncargpath
#MAN5	= fontcap graphcap

man1_MANS = fontc.p graphc.p ncargpath.p
man5_MANS = fontcap.p graphcap.p

# Include the source for the man pages.
EXTRA_DIST = fontc.m graphc.m ncargpath.m fontcap.m graphcap.m

NROFF = nroff -man

%.p: %.m
	tbl $< | $(NROFF) > col > $@

CLEANFILES = *.p
