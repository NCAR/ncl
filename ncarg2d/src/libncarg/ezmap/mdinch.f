C
C $Id: mdinch.f,v 1.8 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDINCH (INUM,CHRS,NCHR)
C
C This routine, when given an integer INUM (which should be in the
C range from 0 to 999, inclusive) and a character buffer CHRS already
C containing NCHS characters, adds the character representation of
C the integer to the buffer.
C
        INTEGER       INUM
        CHARACTER*(*) CHRS
        INTEGER       NCHR
C
C Declare local variables.
C
        INTEGER       IDIG,ITMP
C
C Don't trust the user ... :-)
C
        ITMP=MAX(0,MIN(999,ABS(INUM)))
C
C Decide whether 3, 2, or 1 characters will be generated and increment
C NCHR appropriately.
C
        IF (ITMP.GE.100) THEN
          NCHR=NCHR+3
        ELSE IF (ITMP.GE.10) THEN
          NCHR=NCHR+2
        ELSE
          NCHR=NCHR+1
        END IF
C
C Generate the characters (in reverse order) and store them.
C
        IDIG=0
C
  101   CHRS(NCHR-IDIG:NCHR-IDIG)=CHAR(ICHAR('0')+MOD(ITMP,10))
        ITMP=ITMP/10
        IDIG=IDIG+1
        IF (ITMP.NE.0) GO TO 101
C
C Done.
C
        RETURN
C
      END
