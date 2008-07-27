C
C	$Id: gflcnv.f,v 1.6 2008-07-27 00:21:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GFLCNV (FNUM,INUM)
C
C     This subroutine converts the floating-point number in
C     FNUM into two 16-bit integers in the low order bits of
C     INUM.  These two 16-bit quantities are suitable for
C     insertion into the CGM.  The first 16-bit integer is
C     given by the low-order 15 bits resulting from INUM(1)=FUNM.
C     This number is then expressed in 2's complement format.  The
C     second 16-bit integer is the decimal part of FNUM
C     multiplied by 2**16 (65536.)
C
      REAL     FNUM
      INTEGER  INUM(2)
C
      INTEGER  JNUM,JABS
C
C  Find the smallest integer less than or equal to FNUM and store
C  it in JNUM.
C
      JNUM = INT(FNUM)
      IF (FNUM.LT.0. .AND. REAL(JNUM).NE.FNUM) JNUM = JNUM-1
C
C  Find the fractional part.
C
      INUM(2) = 65536.*(FNUM-REAL(JNUM))
C
C  Define 2's complement of whole part.
C
      JABS = ABS(JNUM)
      INUM(1) = IAND(32767,JABS)
      IF (JNUM .LT. 0) THEN
        INUM(1) = (32767-INUM(1))+1
        INUM(1) = IOR(INUM(1),ISHIFT(1,15))
      ENDIF
C
      RETURN
      END
