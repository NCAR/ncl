
      PROGRAM CSEX01
C
C  This example illustrates the effects of using differing numbers
C  of knots in calls to CSA1S with the same input data.
C
C
C  The dimensionality of the problem.
C
      PARAMETER (NDIM=1)
C
C  The number of input data points.
C
      PARAMETER (NDATA=10) 
C
C  The number of output data points.
C
      PARAMETER (NPTS=101) 
C  
C  The maximum number of knots used in any call.
C
      PARAMETER (NCF=9)
C
C  The size of the workspace.
C
      PARAMETER (NWRK=NCF*(NCF+3))
C
C  Dimension arrays.
C
      DIMENSION XDATA(NDIM,NDATA),YDATA(NDATA),XDATAT(NDATA)
      DIMENSION WORK(NWRK)
      DIMENSION XO(NPTS),YO4(NPTS),YO7(NPTS),YO9(NPTS)
C
C  Define the original data.
C
      DATA XDATA(NDIM,1),XDATA(NDIM,2),XDATA(NDIM,3)/ 0.00, 0.1, 0.2 /       
      DATA XDATA(NDIM,4),XDATA(NDIM,5),XDATA(NDIM,6)/ 0.30, 0.5, 0.6 /
      DATA XDATA(NDIM,7),XDATA(NDIM,8),XDATA(NDIM,9)/ 0.65, 0.8, 0.9 /
      DATA XDATA(NDIM,10)                           / 1.00           /
      DATA YDATA/0.0, 0.8, -0.9, -0.9, 0.9, 1.0, 0.90, -0.8, -0.8, 0./
C
C  Create the output X coordinate array.
C 
      XINC = 1./(NPTS-1)
      DO 10 I=1,NPTS
        XO(I) = (I-1)*XINC
   10 CONTINUE
C
C  Calculate the approximated function values using differing 
C  number of knots.
C
      KNOTS = 4
      CALL CSA1S (NDATA,XDATA,YDATA,KNOTS,NPTS,XO,YO4,NWRK,WORK,IER)       
      IF (IER .NE. 0) THEN
        WRITE(6,520) IER
  520   FORMAT(' Error ',I3,' returned from CSA1S')
        STOP
      ENDIF
C
      KNOTS = 7
      CALL CSA1S (NDATA,XDATA,YDATA,KNOTS,NPTS,XO,YO7,NWRK,WORK,IER)       
      IF (IER .NE. 0) THEN
        WRITE(6,520) IER
        STOP
      ENDIF
C
      KNOTS = 9
      CALL CSA1S (NDATA,XDATA,YDATA,KNOTS,NPTS,XO,YO9,NWRK,WORK,IER)       
      IF (IER .NE. 0) THEN
        WRITE(6,520) IER
        STOP
      ENDIF
C
C  Draw a plot of the approximation functions and mark the original points.
C
      DO 34 I=1,NDATA
        XDATAT(I) = XDATA(1,I)
   34 CONTINUE
C
      CALL DRWFT1(NDATA,XDATAT,YDATA,NPTS,XO,YO4,YO7,YO9)
C
      STOP
      END
      SUBROUTINE DRWFT1(NUMO,X,Y,IO,XO,CURVE1,CURVE2,CURVE3)
C
C  This subroutine uses NCAR Graphics to plot three curves on
C  the same picture showing the results from calling CSA1X with
C  differing number of knots.  The values for the curves are
C  contained in arrays CURVE1, CURVE2, and CURVE3.
C
      DIMENSION XO(IO),CURVE1(IO),CURVE2(IO),CURVE3(IO)
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C     PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C  Vertical position for initial curve.
C
      DATA YPOS_TOP/0.88/
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define a color table.
C
      CALL GSCR(IWKID, 0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID, 1, 0.0, 0.0, 0.0)
      CALL GSCR(IWKID, 2, 1.0, 0.0, 0.0)
      CALL GSCR(IWKID, 3, 0.0, 0.0, 1.0)
      CALL GSCLIP(0)
C
C  Plot the main title.
C
      CALL PLCHHQ(0.50,0.95,':F21:Demo for csa1s',0.035,0.,0.)
C
C  Draw a background grid for the first curve.
C
      YB = -1.2
      YT =  1.2
      CALL BKGFT1(YPOS_TOP,'knots = 4',YB,YT)
      CALL GRIDAL(5,5,4,1,1,1,10,0.0,YB)
C
C  Mark the original data points.
C
      CALL GSMKSC(2.2)
      CALL GSPMCI(3)
      CALL GSLWSC(1.)
      CALL GPM(NUMO,X,Y)
C
C  Graph the approximated function values for KNOTS=4.
C
      CALL GSPLCI(1)
      CALL GPL(IO,XO,CURVE1)
C
C  Graph the approximated function values for KNOTS=7.
C
      CALL BKGFT1(YPOS_TOP-0.3,'knots = 7',YB,YT)
      CALL GRIDAL(5,5,4,1,1,1,10,0.0,YB)
      CALL GPM(NUMO,X,Y)
      CALL GPL(IO,XO,CURVE2)
      CALL GSPLCI(1)
C
C  Graph the approximated function values for KNOTS=9.
C
      CALL BKGFT1(YPOS_TOP-0.6,'knots = 9',YB,YT)
      CALL GRIDAL(5,5,4,1,1,1,10,0.0,YB)
      CALL GPM(NUMO,X,Y)
      CALL GPL(IO,XO,CURVE3)
      CALL GSPLCI(1)
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
C
      RETURN
      END
      SUBROUTINE BKGFT1(YPOS,LABEL,YB,YT)
C
C  This subroutine draws a background grid.
C
      DIMENSION XX(2),YY(2)
      CHARACTER*(*) LABEL
C
      CALL SET(0.,1.,0.,1.,0.,1.,0.,1.,1)
C
C  Plot the curve label using font 21 (Helvetica).
C
      CALL PCSETI('FN',21)
      CALL PLCHHQ(0.25,YPOS - 0.03,LABEL,0.025,0.,-1.)
      CALL SET(0.13,0.93,YPOS-0.2,YPOS,0.0,1., YB, YT, 1)
C
C  Draw a horizontal line at Y=0. using color index 2.
C
      XX(1) = 0.
      XX(2) = 1.
      YY(1) = 0.
      YY(2) = 0.
      CALL GSPLCI(2)
      CALL GPL(2,XX,YY)
      CALL GSPLCI(1)
C
C  Set Gridal parameters. 
C
C
C   Set LTY to indicate that the Plotchar routine PLCHHQ should be used.
C
      CALL GASETI('LTY',1)
C
C   Size and format for X axis labels.
C
      CALL GASETR('XLS',0.02)
      CALL GASETC('XLF','(F3.1)')
C
C   Size and format for Y axis labels.
C
      CALL GASETR('YLS',0.02)
      CALL GASETC('YLF','(F5.1)')
C
C   Length of major tick marks for the X and Y axes.
C
      CALL GASETR('XMJ',0.02)
      CALL GASETR('YMJ',0.02)
C
      RETURN
      END
