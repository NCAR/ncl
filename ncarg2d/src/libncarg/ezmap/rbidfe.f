C
C $Id: rbidfe.f,v 1.3 2000-08-22 15:03:41 haley Exp $
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
      FUNCTION RBIDFE (QDFE)
C
C The value of RBIDFE(QDFE) is the latitude for which the distance of
C the parallel of the Robinson projection from the equator is QDFE.
C
        DIMENSION PDFE(19)
C
        DATA PDFE( 1) / 0.0000 /  !   0N
        DATA PDFE( 2) / 0.0620 /  !   5N
        DATA PDFE( 3) / 0.1240 /  !  10N
        DATA PDFE( 4) / 0.1860 /  !  15N
        DATA PDFE( 5) / 0.2480 /  !  20N
        DATA PDFE( 6) / 0.3100 /  !  25N
        DATA PDFE( 7) / 0.3720 /  !  30N
        DATA PDFE( 8) / 0.4340 /  !  35N
        DATA PDFE( 9) / 0.4958 /  !  40N
        DATA PDFE(10) / 0.5571 /  !  45N
        DATA PDFE(11) / 0.6176 /  !  50N
        DATA PDFE(12) / 0.6769 /  !  55N
        DATA PDFE(13) / 0.7346 /  !  60N
        DATA PDFE(14) / 0.7903 /  !  65N
        DATA PDFE(15) / 0.8435 /  !  70N
        DATA PDFE(16) / 0.8936 /  !  75N
        DATA PDFE(17) / 0.9394 /  !  80N
        DATA PDFE(18) / 0.9761 /  !  85N
        DATA PDFE(19) / 1.0000 /  !  90N
C
C XDFE is the magnitude of QDFE, limited to the range of values in the
C table.
C
        XDFE=MAX(0.,MIN(1.,ABS(QDFE)/.5072))
C
C Find the indices of the values in the table between which XDFE lies.
C
        IBEG=1
        IEND=19
C
  101   ITST=(IBEG+IEND)/2
C
        IF (PDFE(ITST).LE.XDFE) THEN
          IBEG=ITST
        ELSE
          IEND=ITST
        END IF
C
        IF (IEND-IBEG.GT.1) GO TO 101
C
C Now, just interpolate to find the desired latitude.
C
        RBIDFE=SIGN(5.*(REAL(IBEG-1)+(      XDFE-PDFE(IBEG))/
     +                               (PDFE(IEND)-PDFE(IBEG))),QDFE)
C
C Done.
C
        RETURN
C
      END
