C
C $Id: cpinrk.f,v 1.2 1994-03-17 01:51:16 kennison Exp $
C
      SUBROUTINE CPINRK (NSDR,TMP1,TMP2,TMP3)
C
C This routine computes some quantities needed by CPINRC; the code is
C here so as to ensure that, on machines on which arithmetic is done
C in double-precision registers, these quantities will be truncated to
C real precision before being used in tests.
C
      TMP1=10.**(-NSDR)
      TMP2=  1.+TMP1
      TMP3=TMP2+TMP1
C
C Done.
C
      RETURN
C
      END
