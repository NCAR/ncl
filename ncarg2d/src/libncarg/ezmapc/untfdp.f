C
C $Id: untfdp.f,v 1.2 2000-07-12 16:24:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE UNTFDP (INUNIT,IOUNIT,FACTOR,IFLG)
C
C SUBROUTINE TO DETERMINE CONVERGENCE FACTOR BETWEEN TWO LINEAL UNITS
C
C * INPUT ........
C * INUNIT * UNIT CODE OF SOURCE.
C * IOUNIT * UNIT CODE OF TARGET.
C
C * OUTPUT .......
C * FACTOR * CONVERGENCE FACTOR FROM SOURCE TO TARGET.
C * IFLG   * RETURN FLAG .EQ. 0 , NORMAL RETURN.
C            RETURN FLAG .NE. 0 , ABNORMAL RETURN.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      INTEGER INUNIT,IOUNIT,IFLG,MAXUNT
      DIMENSION FACTRS(6,6)
      COMMON /PRINZ0/ IPEMSG,IPELUN,IPPARM,IPPLUN
        INTEGER IPEMSG,IPELUN,IPPARM,IPPLUN
      SAVE   /PRINZ0/
      PARAMETER (ZERO = 0.0D0, MAXUNT = 6)
      DATA FACTRS /0.1000000000000000D01 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.2062648062470963D06 ,
     .             0.5729577951308231D02 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.1000000000000000D01 ,
     .             0.3048006096012192D00 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.1000002000004000D01 ,
     .             0.0000000000000000D00 , 0.3280833333333333D01 ,
     .             0.1000000000000000D01 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.3280839895013124D01 ,
     .             0.4848136811095360D-5 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.1000000000000000D01 ,
     .             0.2777777777777778D-3 , 0.0000000000000000D00 ,
     .             0.1745329251994330D-1 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.3600000000000000D04 ,
     .             0.1000000000000000D01 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.9999980000000000D00 ,
     .             0.3048000000000000D00 , 0.0000000000000000D00 ,
     .             0.0000000000000000D00 , 0.1000000000000000D01 /
C
      IF (INUNIT .GE. 0 .AND. INUNIT .LT. MAXUNT .AND.
     .    IOUNIT .GE. 0 .AND. IOUNIT .LT. MAXUNT) THEN
         FACTOR = FACTRS(IOUNIT+1 , INUNIT+1)
         IF (FACTOR .NE. ZERO) THEN
            IFLG = 0
            RETURN
         ELSE
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,2000) INUNIT,IOUNIT
 2000       FORMAT (' INCONSISTENT UNIT CODES = ',I6,' / ',I6)
            IFLG = 12
            RETURN
         END IF
      ELSE
         IF (INUNIT.LT.0 .OR. INUNIT.GE.MAXUNT) THEN
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010) INUNIT
 2010       FORMAT (' ILLEGAL SOURCE OR TARGET UNIT CODE = ',I6)
         ELSE
            IF (IPEMSG .EQ. 0) WRITE (IPELUN,2010) IOUNIT
         END IF
         IFLG = 11
         RETURN
      END IF
C
      END
