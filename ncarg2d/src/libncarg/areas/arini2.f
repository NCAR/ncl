C
C $Id: arini2.f,v 1.7 1994-03-16 23:11:27 kennison Exp $
C
      SUBROUTINE ARINI2 (ILC,RS1,RS2,RS3,DS1,DS2,DS3,RS4,RS5,RS6)
C
      DOUBLE PRECISION DS1,DS2,DS3
C
C This code had to be moved here from the routine ARINIT in order to
C force compilers (on the Mac and possibly elsewhere) to set up code
C that not only computes these quantities to the stated precision,
C but actually uses the values computed and stored, instead of using
C values from extended-precision arithmetic registers.
C
      RS1=REAL(ILC)*REAL(ILC)
      RS2=RS1+.25E0
      RS3=RS2+.25E0
      DS1=DBLE(ILC)*DBLE(ILC)
      DS2=DS1+.25D0
      DS3=DS2+.25D0
      RS4=REAL(ILC)
      RS5=RS4+.25E0
      RS6=RS5+.25E0
C
C Done.
C
      RETURN
C
      END
