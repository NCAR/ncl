C
C $Id: pcffhl.f,v 1.4 2000-07-12 16:24:56 haley Exp $
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
      SUBROUTINE PCFFHL (NCI,JNDX,XORIG,YORIG,NCO,JNDXO,RDGU)       
C
      include 'pcffdx.h'
      include 'pcffsp.h'
C
C Process holes.
C
      DIMENSION RDGU(*)
C
      JNDXO = JNDX
      NDX = NCI
C
   10 CONTINUE
      NDX = NDX+1
C
      IF (SFLGS(NDX) .EQ. 3) THEN
C
C  End hole, connect the last point back to the initial point if not
C  an outline font.
C
        IF (OUTLIN .EQ. 0) THEN
          JNDXO = JNDXO+1 
          RDGU(JNDXO) = XORIG
          JNDXO = JNDXO+1 
          RDGU(JNDXO) = YORIG
        ENDIF
C
C  Terminate the line if an outline font has been requested and
C  set the next point in the new line back to the original
C  point upon entry to this subroutine.
C
        IF (OUTLIN .EQ. 1) THEN
          JNDXO = JNDXO+1
          RDGU(JNDXO) = -2048.
          JNDXO = JNDXO+1 
          RDGU(JNDXO) = 0.
          JNDXO = JNDXO+1
          RDGU(JNDXO) = XORIG
          JNDXO = JNDXO+1
          RDGU(JNDXO) = YORIG
        ENDIF
        NCO = NDX
        RETURN
      ELSE IF (SFLGS(NDX) .EQ. 0) THEN
C
C  Add coordinate to the current path.
C
        JNDXO = JNDXO+1
        RDGU(JNDXO) = XC(NDX)
        JNDXO = JNDXO+1
        RDGU(JNDXO) = YC(NDX)
        GO TO 10
      ELSE IF (SFLGS(NDX) .EQ. 4) THEN
C
C  Process the Bezier curve.
C
          BCNTLX(1) = RDGU(JNDXO-1)
          BCNTLY(1) = RDGU(JNDXO)
          DO 30 IB=2,4
            BCNTLX(IB) = XC(NDX+IB-2)
            BCNTLY(IB) = YC(NDX+IB-2)
   30     CONTINUE
C
          CALL BCCURV(BCNTLX,BCNTLY,IBZL,BZXC,BZYC,NBP)
          DO 60 K=2,NBP
            JNDXO = JNDXO+1
            RDGU(JNDXO) = BZXC(K)
            JNDXO = JNDXO+1
            RDGU(JNDXO) = BZYC(K)
   60     CONTINUE
          NDX = NDX+2
          GO TO 10
      ELSE
          PRINT * , 'PCFFHL -- Invalid fontcap encoding encountered.'
          STOP
      ENDIF
C
      END
