C
C       $Id: vvudmv.f,v 1.10 2008-07-27 00:17:35 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE VVUDMV(XCS,YCS,NCS,IAI,IAG,NAI)
C
      DIMENSION XCS(NCS),YCS(NCS),IAI(NAI),IAG(NAI)
C
C
C Example version of user masked drawing program
C
C XCS,YCS    - Arrays of X,Y coordinates
C NCS        - Number of points
C IAI        - Area identifier array
C IAG        - Area group array
C NAI        - number of area/group identifiers
C
C --------------------------------------------------------------------
C
      PARAMETER (IPMXCT=64)
      REAL XC(IPMXCT),YC(IPMXCT)
      INTEGER ICNT
      DATA ICNT /0/
      SAVE XC,YC,ICNT

      DO 101 I=1,NAI
         IF (IAI(I).LT.0) RETURN
 101  CONTINUE
C
      CALL VVGETI('AST',IAS)
C
C Depending on the arrow style, draw a polyline or a filled polygon
C
      IF (IAS.EQ.0) THEN
         CALL GPL(NCS,XCS,YCS)
         RETURN
      END IF
C
C If the 'filled' arrows are hollow, just draw the line as above
C
      CALL VVGETI('ACM',ICM)
      IF (ICM.EQ.-1) THEN
         CALL GPL(NCS,XCS,YCS)
         RETURN
      END IF
C
C This routine to draw filled arrows is rather cumbersome and
C completely eliminates arrows that partially intrude into
C a masked area
C
      IF (ICNT.NE.0) THEN
         IF (XCS(1).NE.XC(ICNT) .OR. YCS(1).NE.YC(ICNT)) THEN
            ICNT = 0
         ELSE IF (ICNT + NCS - 1 .GT. IPMXCT) THEN
C
C If the buffer size is exceeded throw away the points involved
C Uncomment the following to print out an error message in this case
C
C     WRITE(*,*) "ERROR IN VVUDMV: LOCAL BUFFER OVERFLOW"
C
            ICNT = 0
         ELSE
            DO 10 I = 2,NCS
               ICNT = ICNT+1
               XC(ICNT) = XCS(I)
               YC(ICNT) = YCS(I)
 10         CONTINUE
C
C If the points form a closed polygon draw them and empty the buffer
C
            IF (XC(1).EQ.XC(ICNT) .AND. YC(1).EQ.YC(ICNT)) THEN
C
               CALL VVGETI('AFO',IFO)
               IF (IFO.GT.0 .AND. ICM.GT.-2) THEN
                  CALL GPL(ICNT,XC,YC)
               END IF
               CALL GFA(ICNT,XC,YC)
               IF (IFO.LE.0 .AND. ICM.GT.-2) THEN
                  CALL GPL(ICNT,XC,YC)
               END IF
C
               ICNT = 0
            END IF
            RETURN
         END IF
      END IF
C
C Draw the polygon if the points close; otherwise buffer the points.
C
      IF (XCS(1).EQ.XCS(NCS).AND.YCS(1).EQ.YCS(NCS)) THEN
C
         CALL VVGETI('ACM',ICM)
         CALL VVGETI('AFO',IFO)
         IF (IFO.GT.0 .AND. ICM.GT.-2) THEN
            CALL GPL(NCS,XCS,YCS)
         END IF
         CALL GFA(NCS,XCS,YCS)
         IF (IFO.LE.0 .AND. ICM.GT.-2) THEN
            CALL GPL(NCS,XCS,YCS)
         END IF
C     
      ELSE
         DO 20 I = 1,NCS
            ICNT = ICNT+1
            XC(ICNT) = XCS(I)
            YC(ICNT) = YCS(I)
 20      CONTINUE
      ENDIF
C
C Done.
C
      RETURN
C
      END
