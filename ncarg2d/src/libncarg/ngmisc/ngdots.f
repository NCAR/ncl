C
C $Id: ngdots.f,v 1.14 2008-07-27 00:17:17 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE NGDOTS(X,Y,NUM,SIZE,ICOLOR)
C
C  Draws filled dots at positions (X(I),Y(I),I=1,NUM) at size 
C  SIZE with color given by the color index ICOLOR.  Size is 
C  the diameter of the dot in world coordinates along the vertical
C  axis.
C  
C  The dots are scaled appropriately so that they will be circular
C  when the normalization transformation does not preserve aspect
C  ratio.
C
C  The algorithm is to construct a single circle and then translate
C  it to the various coordinate positions.  The original circle is
C  computed by using trig functions to get points in the first
C  octant, and then using symmetries to get the rest of the points
C  on the circle.
C
C  The number of points used for the circle is adjusted depending
C  on the relative size of the circle.  The maximum number of points
C  is 512 and the minimum is 8.
C
      SAVE
C
      include 'ngcomn.h'
C
      DIMENSION X(NUM),Y(NUM),TWIN(4),TVPT(4)
      DIMENSION CIRCX(513),CIRCY(513),CIRCXX(513),CIRCYY(513)
      INTEGER   OLDASF(13),NEWASF(13)
C
C  PI and number of points per unit of arc length.
C
      DATA PI,NPU/3.1415927,1200/
C
C  Size below which markers are drawn in addition to the filled dots
C  to insure that the dots do not disappear by falling between pixels.
C
      DATA DM/.05/
C
C  Size cutoff for increasing the number of points at smaller sizes
C  so that the dots look round.
C
      DATA PC/.03/
C
C  Minimum dot size.  Below this tolerance, only dot markers are drawn.
C
      DATA SM/.001/
C
      DATA IFIRST/0/
C
C  If log scaling or mirror imaging has been requested, issue a
C  warning that only GKS world coordinates are accepted.
C
      CALL GETUSV('LS',LSV)
      CALL GETUSV('MI',MIV)
      IF ((LSV.NE.1 .OR. MIV.NE.1) .AND. IFIRST.EQ.0) THEN
        PRINT *, 'NGDOTS -- **Warning**'
        PRINT *, '          Log scaling or mirror-imaging have been requ
     +ested via a SET call;'
        PRINT *, '          NGDOTS accepts only GKS world coordinates.'
        IFIRST = 1
      ENDIF
C
C  Calculate the number of points to use in calculating the points
C  in the first octant of the circle.
C
      CALL GQCNTN(IER,NTNR)
      CALL GQNT(NTNR,IER,TWIN,TVPT)
C
C  Normalize the size.
C
      SD = SIZE*(TVPT(4)-TVPT(3))/(TWIN(4)-TWIN(3))
C
C  Set a minimum dot size.
C
      IF (SD .LT. SM) THEN
        SD = SM
        TSIZE = SD*(TWIN(4)-TWIN(3))/(TVPT(4)-TVPT(3))
      ELSE
        TSIZE = SIZE
      ENDIF
C
C  Calculate the number of points on the curve.
C
      NP = INT(NPU*PI*SD)
C
C  Increase the number of points for smaller circles.
C
      IF (SD .LE. PC) NP = NP*2
      NP = NP/8
      NP = NP+1
      NP = MAX(NP,2)
      NP = MIN(NP,64)    
C
C  Center the dot at (0.,0.) and translate it to the (X,Y) 
C  coordinates later.
C
C  Calculate the points in the first octant.
C
      AINC = .25*PI/REAL(NP)
      R = .5*TSIZE
      DO 10 I=1,NP
        T = REAL(I-1)*AINC
        CIRCX(I) = R*COS(T)
        CIRCY(I) = R*SIN(T)
   10 CONTINUE
C 
C  Use symmetries to get the other points on the circle.
C
      NP2 = 2*NP
      DO 30 I=NP+1,NP2-1
        CIRCX(I) = CIRCY(NP2-I)
        CIRCY(I) = CIRCX(NP2-I)
   30 CONTINUE
      NP = NP2-1
      NP2 = 2*NP
      DO 40 I=NP+1,NP2-1
        CIRCX(I) = -CIRCX(NP2-I)
        CIRCY(I) =  CIRCY(NP2-I)
   40 CONTINUE
      NP = NP2-1
      NP2 = 2*NP
      DO 50 I=NP+1,NP2-1
        CIRCX(I) =  CIRCX(NP2-I)
        CIRCY(I) = -CIRCY(NP2-I)
   50 CONTINUE
      NP = NP2-1
C
C  Scale factor for non-square aspect ratios.
C
      WSCALE = (TWIN(2)-TWIN(1))/(TWIN(4)-TWIN(3))
      VSCALE = (TVPT(4)-TVPT(3))/(TVPT(2)-TVPT(1))
      DO 20 I=1,NP
        CIRCX(I) = WSCALE*VSCALE*CIRCX(I)
   20 CONTINUE
C
C  Draw filled dots if the size is large enough.
C
      IF (SD .GE. SM) THEN
C
C  Save the fill color index and interior style.
C
        CALL GQFACI(IER,ICOLRO)
        CALL GQFAIS(IER,ISTYLO)
C
C  Set the fill style to solid and the fill collor to ICOLOR.
C
        CALL GSFAIS(1)
        CALL GSFACI(ICOLOR)
C
C  Draw the dots or circles, depending on ICDFLG.
C
        DO 60 I=1,NUM
          DO 70 J=1,NP
            CIRCXX(J) = X(I) + CIRCX(J)
            CIRCYY(J) = Y(I) + CIRCY(J)
   70     CONTINUE
          IF (ICDFLG .EQ. 0) THEN
            CALL GFA(NP,CIRCXX,CIRCYY)
          ELSE
            CALL GQPLCI(IER,IPLCO)
            CALL GSPLCI(ICOLOR)
            CALL GPL(NP,CIRCXX,CIRCYY)
            CALL GSPLCI(IPLCO)
          ENDIF
   60   CONTINUE
C
C  Restore the fill color and style.
C
        CALL GSFACI(ICOLRO)
        CALL GSFAIS(ISTYLO)
      ENDIF
C
C  Draw polymarkers if the size is small enough (note that there is
C  an overlap where both dots and markers are drawn).  The polymarkers
C  are drawn only when filled dots are requested and not when circles
C  are requested.
C
      IF (SD.LT.DM .AND. ICDFLG.EQ.0) THEN
C
C  Save current GKS attributes.
C
        CALL GQASF(IER,OLDASF)
        CALL GQMK (IER,MTYPEO)
        CALL GQMKSC(IER,SZSFO)
        CALL GQPMCI(IER,IMCOLO)
C
C  Set attributes for polymarkers and draw them.
C
        DO 80 I=1,13
          NEWASF(I) = OLDASF(I)
   80   CONTINUE
        NEWASF(4) = 1
        NEWASF(5) = 1
        NEWASF(6) = 1
        CALL GSASF(NEWASF)
        CALL GSMK(1)
        CALL GSMKSC(1.0)
        CALL GSPMCI(ICOLOR)
        CALL GPM(NUM,X,Y)
C
C  Restore original attributes.
C
        CALL GSASF(OLDASF)
        CALL GSMK(MTYPEO)
        CALL GSMKSC(SZSFO)
        CALL GSPMCI(IMCOLO)
      ENDIF
C
      RETURN
      END
