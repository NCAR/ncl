C
C $Id: mdlocd.f,v 1.6 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDLOCD (RLON,CHRS,NCHR,NOFD)
C
C This routine, given a longitude RLON and a character buffer CHRS,
C returns CHRS and NCHR such that CHRS(1:NCHR), when written by PLCHHQ,
C will yield a representation of RLON in degrees and possibly fractions
C of a degree east or west of the prime meridian at Greenwich.
C
        DOUBLE PRECISION RLON
        CHARACTER*(*)    CHRS
        INTEGER          NCHR,NOFD
C
C Declare local variables.
C
        DOUBLE PRECISION ALON
        INTEGER          I,ILON
C
C Compute the equivalent value of the longitude in the range -180 to
C +180 degrees.
C
        ALON=MOD(MOD(RLON,360.D0)+360.D0,360.D0)
        IF (ALON.GE.180.D0) ALON=ALON-360.D0
C
C Compute the absolute value of the longitude in units of 10**(-5)
C degrees, limiting it to the range [0,18000000].
C
        ILON=MIN(18000000,INT(100000.D0*ABS(ALON)+.5D0))
C
C If the value is zero, return a string that will make PLCHHQ write a
C 0 followed by a degree sign.
C
        IF (ILON.EQ.0) THEN
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
        CALL MDINCH (ILON/100000,CHRS,NCHR)
C
C If there is to be a fractional part, put it in the buffer.
C
        IF (NOFD.NE.0) THEN
          NCHR=NCHR+1
          CHRS(NCHR:NCHR)='.'
          DO 101 I=1,NOFD
            CALL MDINCH (MOD(ILON/10**(5-I),10),CHRS,NCHR)
  101     CONTINUE
        END IF
C
C Add the code for a degree sign to the buffer.
C
        NCHR=NCHR+9
        CHRS(NCHR-8:NCHR)=':F34:0:F:'
C
C Finally, put either a "W" or an "E" in the buffer.
C
        NCHR=NCHR+1
C
        IF (ALON.LT.0.) THEN
          CHRS(NCHR:NCHR)='W'
        ELSE
          CHRS(NCHR:NCHR)='E'
        END IF
C
C Done.
C
        RETURN
C
      END
