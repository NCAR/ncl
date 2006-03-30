C
C	$Id: gflcnv.f,v 1.5 2006-03-30 00:45:03 fred Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
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
