C
C $Id: pcffhl.f,v 1.6 2008-07-27 00:17:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
