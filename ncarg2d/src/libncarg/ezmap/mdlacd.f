C
C $Id: mdlacd.f,v 1.6 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDLACD (RLAT,CHRS,NCHR,NOFD)
C
C This routine, given a latitude RLAT and a character buffer CHRS,
C returns CHRS and NCHR such that CHRS(1:NCHR), when written by PLCHHQ,
C will yield a representation of RLAT in degrees and possibly fractions
C of a degree north or south of the equator.
C
        DOUBLE PRECISION RLAT
        CHARACTER*(*)    CHRS
        INTEGER          NCHR,NOFD
C
C Declare local variables.
C
        INTEGER          I,ILAT
C
C Compute the absolute value of the latitude in units of 10**(-5)
C degrees, limiting it to the range [0,9000000].
C
        ILAT=MIN(9000000,INT(100000.D0*ABS(RLAT)+.5D0))
C
C If the value is zero, return a string that will make PLCHHQ write a
C 0 followed by a degree sign.
C
        IF (ILAT.EQ.0) THEN
          NCHR=7
          CHRS='0:F34:0'
          RETURN
        END IF
C
C Otherwise, put blanks in the buffer and zero the current character
C count.
C
        CHRS=' '
        NCHR=0
C
C Put the number of degrees in the buffer.
C
        CALL MDINCH (ILAT/100000,CHRS,NCHR)
C
C If there is to be a fractional part, put it in the buffer.
C
        IF (NOFD.NE.0) THEN
          NCHR=NCHR+1
          CHRS(NCHR:NCHR)='.'
          DO 101 I=1,NOFD
            CALL MDINCH (MOD(ILAT/10**(5-I),10),CHRS,NCHR)
  101     CONTINUE
        END IF
C
C Add the code for a degree sign to the buffer.
C
        NCHR=NCHR+9
        CHRS(NCHR-8:NCHR)=':F34:0:F:'
C
C Finally, put either an "S" or an "N" in the buffer.
C
        NCHR=NCHR+1
C
        IF (RLAT.LT.0.) THEN
          CHRS(NCHR:NCHR)='S'
        ELSE
          CHRS(NCHR:NCHR)='N'
        END IF
C
C Done.
C
        RETURN
C
      END
