C
C $Id: arblda.f,v 1.12 1995-04-28 19:40:55 kennison Exp $
C
      BLOCK DATA ARBLDA
C
C Declare the AREAS common block.
C
C
C ARCOMN contains variables which are used by all the AREAS routines.
C
      COMMON /ARCOMN/ IAD,IAU,ILC,RLC,ILM,RLM,ILP,RLP,IBS,RBS,DBS,IDB,
     +                IDC,IDI,IRC(16),RLA,RWA,RDI,RSI
      SAVE   /ARCOMN/
C
C Below are descriptions of all the common variables and default values
C for those which require defaults.
C
C IAD is the type of arithmetic desired by the user, as follows:
C
C IAD=0 allows AREAS to decide what type of arithmetic to use.
C IAD=1 forces the use of real arithmetic.
C IAD=2 forces the use of double-precision arithmetic.
C IAD=3 forces the use of multiple-precision arithmetic.
C
      DATA IAD / 0 /
C
C IAU is the type of arithmetic actually chosen for use, either by the
C user or by AREAS itself, as follows:
C
C IAU=0 says that no choice has been made yet.
C IAU=1 specifies the use of real arithmetic.
C IAU=2 specifies the use of double-precision arithmetic.
C IAU=3 specifies the use of multiple-precision arithmetic.
C
      DATA IAU / 0 /
C
C ILC is the largest coordinate value to be used.  ARINIT sets RLC
C equal to REAL(ILC).
C
      DATA ILC / 1000000 /
C
C ILM is equal to ILC-1, RLM is equal to ILM, ILP is equal to ILC+1,
C and RLP is equal to ILP.  All of these values are set by ARINIT.
C
C IBS is the base for the multiple-precision arithmetic, when that type
C of arithmetic is selected.  Its value is set by ARINIT.  RBS is made
C equal to REAL(IBS) and DBS is made equal to DBLE(IBS).
C
      DATA IBS / 0 /
C
C IDB is the internal parameter 'DB', which may be set non-zero by a
C user program to turn on the production of debug plots.
C
      DATA IDB / 0 /
C
C IDC is the internal parameter 'DC', which may be set by a user
C program to change the range of color indices used by ARDBPA.
C
      DATA IDC / 100 /
C
C IDI is the internal parameter 'DI', which may be retrieved in a user
C version of the routine "APR".  Its value will be 1 if the polygon to
C be processed is traced counter-clockwise (interior to the left), or a
C 2 if the polygon is traced clockwise (interior to the right).
C
      DATA IDI / 0 /
C
C IRC is the internal parameter 'RC'.  For IGI = 1 to 16, IRC(IGI) says
C how to reconcile contradictory area-identifier information for group
C IGI.  (Groups with group identifiers greater than 16 are affected by
C IRC(16).)  The default value of IRC(IGI) is zero, which says to do it
C the original way, using the most recently-provided information for a
C given area; the value 1 says to do it a new way, using that area
C identifier seen most frequently for the area, but to ignore zeroes;
C the value 2 says to do it the new way, but not to ignore zeroes.
C
      DATA IRC / 16*0 /
C
C RLA is the internal parameter 'AL', which specifies the length of the
C arrowheads to be used on debug plots, stated as a fraction of the
C distance across the plotter frame.
C
      DATA RLA / .008 /
C
C RWA is the internal parameter 'AW', which specifies the width of the
C arrowheads to be used on debug plots, stated as a fraction of the
C distance across the plotter frame.  RWA is actually half the width
C of an arrowhead.
C
      DATA RWA / .002 /
C
C RDI is the internal parameter 'ID', which specifies the distance from
C an edge to an identifier on a debug plot, stated as a fraction of the
C distance across the plotter frame.
C
      DATA RDI / .004 /
C
C RSI is the internal parameter 'IS', which specifies the size (width)
C of characters used to write identifiers on a debug plot, stated as a
C fraction of the distance across the plotter frame.
C
      DATA RSI / .001 /
C
      END
