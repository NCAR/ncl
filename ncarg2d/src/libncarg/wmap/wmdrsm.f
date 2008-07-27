C
C	$Id: wmdrsm.f,v 1.8 2008-07-27 00:17:36 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE WMDRSM(NUMSYM,DSTBTW,IPTS,XIN,YIN,ARCLEN,XT,YT,IPOSIT)       
C
C  Draw weather symbols along the curve  ((XIN,YIN),I=1,IPTS).
C  ARCLEN contains the cumulative polygonal arclengths of the curve.
C  XT and YT are arrays to be used for plotting the symbols.
C
      include 'wmcomn.h'
C
      PARAMETER (EPS=0.0001)
      DIMENSION XIN(IPTS),YIN(IPTS),ARCLEN(IPTS),XT(IPTS),YT(IPTS)
      DIMENSION IPOSIT(NUMSYM)
C
C  Arithmetic statement function for computing square of distance.
C
      DSQ(A,B) = A**2 + B**2
C
C  Check error status.
C
      IF (ICFELL('WMDRSM - Uncleared prior error',1) .NE. 0) RETURN
C
C  Save fill attribures and set to current values.
C
C
C  Save the current fill attributes and reset.
C
      CALL GQFACI(IER,ICOLD)
      CALL GQPLCI(IER,IPOLD)
      CALL GQFAIS(IER,IFOLD)
      CALL GSFACI(ICOLOR)
      CALL GSFAIS(1)
C
C  Handle the case of drawing a single symbol.
C
      SYMWDH = 0.5*SYMWID
      WDTHSQ = SYMWID*SYMWID 
      IF (NUMSYM.EQ.1 .AND. IFRONT.NE.7 .AND.
     +             ARCLEN(IPTS).LT.BEGDST+SYMWID+ENDDST) THEN
C
C  Draw a single symbol in the center of the curve in the direction
C  determined by the slope of the endpoints of the line.  Extend
C  the line by the amounts in BEGDST and ENDDST.
C
C  Find the midpoint of the input curve and the angle created by the
C  endpoints.
C
        SYMPOS = 0.5*ARCLEN(IPTS)
        CALL WMINTV(SYMPOS,ARCLEN,IPTS,NDX)
        IPOSIT(1) = NDX
        XPM = XIN(NDX)
        YPM = YIN(NDX)
        TDST = SQRT(DSQ(YIN(IPTS)-YIN(1),XIN(IPTS)-XIN(1)))
        SINANG = (YIN(IPTS)-YIN(1))/TDST
        COSANG = (XIN(IPTS)-XIN(1))/TDST
C
C  Calculate the end points for positioning the symbol.
C  
        XPR = XPM+SYMWDH*COSANG
        YPR = YPM+SYMWDH*SINANG
        XPL = XPM-SYMWDH*COSANG
        YPL = YPM-SYMWDH*SINANG
C
C  Get the points making up the symbol.
C
        IF (ABS(ISTYPE(1)) .EQ. 1) THEN
          NOUT = 3
        ELSE
          NOUT = NPTSBZ
        ENDIF
        CALL WMGTSO(XPL,YPL,XPR,YPR,NOUT,XT,YT,ISTYPE(1))
C
C  Close the curve and draw it.
C
        XT(NOUT+1) = XT(1)
        YT(NOUT+1) = YT(1)
        NF = NOUT+1
        IF (IALOFT .NE. 0) THEN
          CALL WMLGPL(NF,XT,YT)
        ELSE
          CALL GFA(NF,XT,YT)
        ENDIF
C
C  Draw end lines.
C
        XT(1) = XPR+ENDDST*COSANG
        YT(1) = YPR+ENDDST*SINANG
        XT(2) = XPL-ENDDST*COSANG
        YT(2) = YPL-ENDDST*SINANG
        CALL WMLGPL(2,XT,YT)
        GO TO 120
      ENDIF
C
      DO 60 I=1,NUMSYM
C
C  Calculate the distance along the curve where the symbol should
C  be placed and find the point on the curve where this is.
C
        IF (NUMSYM .EQ. 1) THEN
          SYMPOS = 0.5*ARCLEN(IPTS)
        ELSE
          SYMPOS = BEGDST+SYMWDH+REAL(I-1)*(DSTBTW+SYMWID)
        ENDIF
        CALL WMINTV(SYMPOS,ARCLEN,IPTS,NDX)
        IPOSIT(I) = NDX
C
C  Back up along the curve half a symbol width.
C
        DO 20 J=1,IPTS
          IF (NDX-J .LT. 1) THEN
            NDXL = 1
            XPL = XIN(NDXL)
            YPL = YIN(NDXL)
            GO TO 90
          ENDIF
          IF (ARCLEN(NDX)-ARCLEN(NDX-J) .LT. SYMWDH) GO TO 20
          NDXL = NDX-J
          GO TO 70
   20   CONTINUE
   70   CONTINUE
C
C  We know that the desired coordinate lies between 
C  (XIN(NDXL),YIN(NDXL)) and (XIN(NDXL+1),YIN(NDXL+1)) -- interpolate 
C  to get the coordinate more accurately.
C
        VNTRVL = ARCLEN(NDXL+1)-ARCLEN(NDXL)
        IF (VNTRVL .LT. EPS) THEN
C
C  Spacing is small enough that interpolation will not matter.
C
          XPL = XIN(NDXL)
          YPL = YIN(NDXL)
        ELSE
          RATIO = (ARCLEN(NDX)-ARCLEN(NDXL)-SYMWDH)/VNTRVL
          XPL = XIN(NDXL)+RATIO*(XIN(NDXL+1)-XIN(NDXL))
          YPL = YIN(NDXL)+RATIO*(YIN(NDXL+1)-YIN(NDXL))
        ENDIF
   90   CONTINUE
C
C  Now, move forward along the curve so that the straight line distance
C  from (XPL,YPL) is a full symbol width.
C
        DO 30 J=1,IPTS
          IF (NDX+J .GT. IPTS) THEN
            NDXR = IPTS
            XPR = XIN(NDXR)
            YPR = YIN(NDXR)
            GO TO 100
          ENDIF
          IF (DSQ(XIN(NDX+J)-XPL,YIN(NDX+J)-YPL) .LT. WDTHSQ) 
     +        GO TO 30
          NDXR = NDX+J
          GO TO 80
   30   CONTINUE
   80   CONTINUE
C
C  We know that the desired coordinate lies between 
C  (XIN(NDXR),YIN(NDXR)) and (XIN(NDXR-1),YIN(NDXR-1)) -- 
C  interpolate to get the coordinate more accurately.
C
        DSTM = SQRT(DSQ(XIN(NDXR-1)-XPL,YIN(NDXR-1)-YPL))
        DSTN = SQRT(DSQ(XIN(NDXR)-XPL,YIN(NDXR)-YPL))
        VNTRVL = ABS(DSTN-DSTM)
        IF (VNTRVL .LT. EPS) THEN
C
C  Spacing is small enough that interpolation will not matter.
C
          XPR = XIN(NDXR)
          YPR = YIN(NDXR)
        ELSE
          RATIO = (SYMWID-DSTM)/VNTRVL
          XPR = XIN(NDXR-1)+RATIO*(XIN(NDXR)-XIN(NDXR-1))
          YPR = YIN(NDXR-1)+RATIO*(YIN(NDXR)-YIN(NDXR-1))
        ENDIF
  100   CONTINUE
C
C  Get the points making up the symbol.
C
        IF (ABS(ISTYPE(I)).EQ.1 .OR. ABS(ISTYPE(I)).EQ.3) THEN
          NOUT = 3
        ELSE
          NOUT = NPTSBZ
        ENDIF
        CALL WMGTSO(XPL,YPL,XPR,YPR,NOUT,XT,YT,ISTYPE(I))
C
C  Add points along the curve and draw.
C
        IF (NDXR-NDXL .GE. 2) THEN
          DO 40 J=NDXL+1,NDXR-1
            XT(NOUT+J-NDXL) = XIN(J)
            YT(NOUT+J-NDXL) = YIN(J)
   40     CONTINUE
          NF = NOUT+NDXR-NDXL-1
        ELSE
          NF = NOUT
        ENDIF
        IF (IALOFT .NE. 0) THEN
          ICOLO = ICOLOR
          IF (ABS(ISTYPE(I)) .EQ. 2) THEN
            ICOLOR = IWARMC
          ELSE IF (ABS(ISTYPE(I)) .EQ. 1) THEN
            ICOLOR = ICOLDC
          ENDIF
          CALL WMLGPL(NF,XT,YT)
          ICOLOR = ICOLO
        ELSE IF(IFRONT.EQ.7) THEN
          CALL GQLWSC(IER,OSCL)
          CALL GSLWSC(DLINWD)
          CALL GPL(2,XT,YT)
          CALL GSLWSC(OSCL)
        ELSE
          IF (ABS(ISTYPE(I)) .EQ. 2) THEN
            CALL GSFACI(IWARMC)
          ELSE IF (ABS(ISTYPE(I)) .EQ. 1) THEN
            CALL GSFACI(ICOLDC)
          ENDIF
          CALL GFA(NF,XT,YT)
        ENDIF
C          
   60 CONTINUE
  120 CONTINUE
C
C  Restore fill attributes.
C
      CALL GSFACI(ICOLD)
      CALL GSPLCI(IPOLD)
      CALL GSFAIS(IFOLD)
C
      RETURN
      END
