C
C $Id: agfpbn.f,v 1.2 1996-04-18 17:46:07 kennison Exp $
C
      INTEGER FUNCTION AGFPBN (FPDP)
C
C The value of AGFPBN(FPDP) is a binary dash pattern, obtained from the
C floating-point dash pattern FPDP.  On machines having a word length
C greater than 16 bits, AGFPBN(FPDP) = IFIX(FPDP).  On machines having
C a word length of 16 bits, this is not true.  For example, when FPDP =
C 65535. (2 to the 16th minus 1), the equivalent binary dash pattern
C does not have the value 65535, but the value -1 (assuming integers
C are represented in a ones' complement format).  So, the functions
C ISHIFT and IOR must be used to generate the dash pattern.
C
      TEMP=FPDP
      AGFPBN=0
C
      DO 101 I=1,16
        IF (AMOD(TEMP,2.).GE.1.) AGFPBN=IOR(AGFPBN,ISHIFT(1,I-1))
        TEMP=TEMP/2.
  101 CONTINUE
C
      RETURN
C
      END
