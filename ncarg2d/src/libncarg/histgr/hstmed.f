C
C $Id: hstmed.f,v 1.4 2000-07-12 16:24:23 haley Exp $
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
C *****************************************************
C
      SUBROUTINE HSTMED(DATARR,NPTS2,WRK2,MED)
C
C *****************************************************
C
C  FINDS MEDIAN OF ALL DATA POINTS USING SHELL SORT
C
      REAL DATARR(NPTS2), WRK2(NPTS2), MED, TEMP
      INTEGER FLAG, D, HALFN
C
C  COPY DATA ARRAY TO WORK ARRAY
C
        DO 790 I = 1,NPTS2
  790     WRK2(I) = DATARR(I)
C
C  DO SHELL SORT
C
        HALFN = NPTS2/2
        D = NPTS2
  810   D = (D+1)/2
        FLAG = 0
        DO 830 I=1, NPTS2-D
          IF(WRK2(I) .GT. WRK2(I+D)) THEN
            TEMP = WRK2(I)
            WRK2(I) = WRK2(I+D)
            WRK2(I+D) = TEMP
            FLAG = 1
          ENDIF
  830   CONTINUE
        IF (FLAG .EQ. 1 .OR. D .GT. 1)GOTO 810
C
  850 IF (FLOAT(NPTS2)/2. .GT. FLOAT(HALFN)) THEN
        MED = WRK2(MIN0(HALFN + 1,NPTS2))
      ELSE
        MED = (WRK2(MAX0(1,HALFN)) + WRK2(MIN0(HALFN + 1,NPTS2)))/2.
      ENDIF
      RETURN
      END
