C
C       $Id: vvudmv.f,v 1.5 1996-01-19 17:21:52 dbrown Exp $
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
      PARAMETER (PMXCNT=64)
      REAL XC(PMXCNT),YC(PMXCNT)
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
         CALL CURVE(XCS,YCS,NCS)
      ELSE
         IF (ICNT.NE.0) THEN
            IF (XCS(1).NE.XC(ICNT) .OR. YCS(1).NE.YC(ICNT)) THEN
               ICNT = 0
            ELSE IF (ICNT + NCS - 1 .GT. PMXCNT) THEN
C
C If the buffer size is exceeded throw away the points involved
C
C              WRITE(*,*) "ERROR IN VVUDMV: LOCAL BUFFER OVERFLOW"
C
               ICNT = 0
            ELSE
               DO 10 I = 2,NCS
                  ICNT = ICNT+1
                  XC(ICNT) = XCS(I)
                  YC(ICNT) = YCS(I)
 10            CONTINUE
C
C If the points form a closed polygon draw them and empty the buffer
C
               IF (XC(1).EQ.XC(ICNT) .AND. YC(1).EQ.YC(ICNT)) THEN
                  CALL GSPLCI(0)
                  CALL CURVE(XC,YC,ICNT)
                  CALL GFA(ICNT,XC,YC)
                  write(*,*) icnt
                  ICNT = 0
               END IF
               RETURN
            END IF
         END IF
C
C Draw the polygon if the points close; otherwise buffer the points.
C
         IF (XCS(1).EQ.XCS(NCS).AND.YCS(1).EQ.YCS(NCS)) THEN
            CALL GSPLCI(0)
            CALL CURVE(XCS,YCS,NCS)
            CALL GFA(NCS,XCS,YCS)
         ELSE
            DO 20 I = 1,NCS
               ICNT = ICNT+1
               XC(ICNT) = XCS(I)
               YC(ICNT) = YCS(I)
 20         CONTINUE
         ENDIF
      END IF
C
C Done.
C
      RETURN
C
      END
