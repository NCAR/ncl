      SUBROUTINE NGDOTS(X,Y,NUM,SIZE,ICOLOR)
C
C  Draws filled dots at positions (X(I),Y(I),I=1,NUM) at size 
C  SIZE with color given by the color index ICOLOR.  Size is 
C  the diameter of the dot in user Y coordinates (world coordinates).
C  
C  The dots are scaled appropriately so that they will be circular
C  when the normalization transformation is anisotropic.
C
C  The algorithm is to construct a single circle and then translate
C  it to the various coordinate positions.  The original circle is
C  computed by using trig functions to get points in the first
C  octant, and then use symmetries to get the rest of the points
C  on the circle.
C
C  The number of points used for the circle is adjusted depending
C  on the relative size of the circle.  The maximum number of points
C  is 512 and the minimum is 16.
C
      DIMENSION X(NUM),Y(NUM),TWIN(4),TVPT(4)
      DIMENSION CIRCX(513),CIRCY(513),CIRCXX(513),CIRCYY(513)
C
C  PI and number of points per unit of arc length.
C
      DATA PI,NPU/3.1415927,1200/
C
C  Size cutoff for increasing the number of points and minimum dot size.
C
      DATA PC,SM/.03,.0015/
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
      BSCALE = (TWIN(2)-TWIN(1))/(TWIN(4)-TWIN(3))
      DO 20 I=1,NP
        CIRCX(I) = BSCALE*CIRCX(I)
   20 CONTINUE
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
C  Draw the dots.
C
      DO 60 I=1,NUM
        DO 70 J=1,NP
          CIRCXX(J) = X(I) + CIRCX(J)
          CIRCYY(J) = Y(I) + CIRCY(J)
   70   CONTINUE
        CALL GFA(NP,CIRCXX,CIRCYY)
   60 CONTINUE
C
C  Restore the fill color and style.
C
      CALL GSFACI(ICOLRO)
      CALL GSFAIS(ISTYLO)
C
      RETURN
      END
