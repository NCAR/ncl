      PROGRAM NORMAL
C
C  Illustrate normalization transformations.
C
      PARAMETER (ILD=121)
      DIMENSION PLX(ILD),PLY(ILD)
      DATA  RADC/.0174532/
C
C  Open GKS, open and activate the metafile workstation.
C
      CALL GOPKS (6,IDUM)
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1)
C
C  Define necessary color indices, color index 0 defines the
C  background color.
C
      CALL GSCR(1,0,.0,.0,.6)
      CALL GSCR(1,1,1.,1.,1.)
      CALL GSCR(1,2,1.,1.,0.)
      CALL GSCR(1,3,1.,1.,0.)
      CALL GSCR(1,4,0.,1.,0.)
      CALL GSCR(1,5,0.,1.,1.)
C
C  Create the data for a spiral in the world coordinate square
C  bounded by (-10.,-10.) and (10.,10.) .
C
      J = 0
      DO 10 I=0,720,6
        SCALE = REAL(I)/75.
        J = J+1
        PLX(J) = SCALE*COS(REAL(I-1)*RADC)
        PLY(J) = SCALE*SIN(REAL(I-1)*RADC)
   10 CONTINUE
C
C  Define a normalization transformation that does not preserve
C  aspect ratio.  Draw the transformed spiral with a box bounding
C  the viewport.
C
      XL = -10.
      XR =  10.
      YB = -10.
      YT =  10.
      CALL GSVP(1,.55,.95,.4,.65)
      CALL GSWN(1,XL,XR,YB,YT)
      CALL GSELNT(1)
      CALL GSPLCI(1)
      CALL GPL(121,PLX,PLY)
      CALL BOX(XL,XR,YB,YT)
C
C  Draw an image representing the window to the left of the viewport.
C
      CALL GSVP(1,.05,.45,.3,.70)
      CALL GSWN(1,XL,XR,YB,YT)
      CALL GSELNT(1)
      CALL GSPLCI(1)
      CALL GPL(121,PLX,PLY)
      CALL BOX(XL,XR,YB,YT)
C
C  Draw dashed lines between the outlines.
C
      CALL DLINE(.05,.30,.55,.40)
      CALL DLINE(.45,.30,.95,.40)
      CALL DLINE(.05,.70,.55,.65)
      CALL DLINE(.45,.70,.95,.65)
C
C  Label the plot using Plotchar.
C
      CALL PCSETI('FN',26)
      CALL PCSETI('CC',3)
      CALL PLCHHQ(.5,.83,'Normalization transformation',.035,0.,0.)
C
      CALL PCSETI('FN',21)
      CALL PCSETI('CC',5)
      CALL PLCHHQ(.07,.64,'Window',.022,0.,-1.)
      CALL PLCHHQ(.57,.62,'Viewport',.022,0.,-1.)
C
      CALL GSTXFP(-9,2)
      CALL GSCHH (.035)
      CALL GSTXCI(2)
      CALL GSTXAL(2,3)
      CALL GTX(.5,.22,'Normalization transformation defined by')
C
      CALL PCSETI('FN',29)
      CALL PLCHHQ(.25,.15,'CALL GSWN(1,-10., 10.,-10., 10.)',
     -            .015,0.,-1.)
      CALL PLCHHQ(.25,.10,'CALL GSVP(1, .55, .95, .40, .65)',
     -            .015,0.,-1.)
C
      CALL FRAME
C
C  Deactivate and close the workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
C
      STOP
      END
      SUBROUTINE BOX(XL,XR,YB,YT)
C
C  Draw a box with corner points (XL,YB) and (XR,YT).
C
      DIMENSION XA(5),YA(5)
C
      XA(1) = XL
      YA(1) = YB
      XA(2) = XR
      YA(2) = YB
      XA(3) = XR
      YA(3) = YT
      XA(4) = XL
      YA(4) = YT
      XA(5) = XA(1)
      YA(5) = YA(1)
C
      CALL GSPLCI(1)
      CALL GPL(5,XA,YA)
C
      RETURN
      END
      SUBROUTINE DLINE (X1,Y1,X2,Y2)
      DIMENSION XA(2),YA(2)
C
C  Draw a dashed line with color index 2 between the coordinates
C  (X1,Y1) and (X2,Y2) .
C
      CALL GSELNT(0)
      CALL GSPLCI(2)
      CALL GSLN(2)
      XA(1) = X1
      YA(1) = Y1
      XA(2) = X2
      YA(2) = Y2
      CALL GPL(2,XA,YA)
      CALL GSLN(1)
C
      RETURN
      END
