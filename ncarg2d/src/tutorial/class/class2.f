
      PROGRAM CLASS2
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

      PARAMETER (NPTS=200)
      PARAMETER (NCURVE=4)
      REAL YDRA(NPTS,NCURVE),XDRA(NPTS)
      CHARACTER*27 STRING
C
C Generate some data
C
      DO 15 I=1,NPTS
         XDRA(I )=I*0.1
         DO 10 J=1,NCURVE
            YDRA(I,J)=SIN(XDRA(I)+0.2*J)*EXP(-0.01*XDRA(I)*J**2)
 10      CONTINUE
 15   CONTINUE
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Set up a color table
C
      CALL DEFCLR(IWKID)
C
C Label the axes
C
      CALL PLCHHQ(.5,.05,'Time (seconds)',.012,0.,0.)
      CALL PLCHHQ(.025,.5,'Position (meters)',.012,90.,0.)
C
C Set the window for the range of the data and set the viewport to
C protect the axis labels
C
      CALL SET(.1,.9,.1,.9,0.0,20.0,-1.4,1.4,1)
C
C Set up tick mark labels
C
      CALL LABMOD('(I2)','(F4.1)',0,0,8,8,0,0,0)
C
C Draw axes and their labels
C
      CALL GRIDAL(10,2,15,2,1,1,5,0.0,0.0)
C
C Draw each curve with a different label
C
      DO 20, I=1,NCURVE
         WRITE (STRING,30) I
         CALL DASHDC(STRING,1,1)
         CALL GSPLCI(I+1)
         CALL GSTXCI(I+1)
         CALL CURVED(XDRA,YDRA(1,I),NPTS)
 20   CONTINUE
C
C Close the frame
C
      CALL FRAME
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS

 30   FORMAT('''$$$$$$$$$$$$$$$$''''Curve''''',I1)
      STOP
      END
C
      SUBROUTINE DEFCLR (IWKID)
C
C Define a color table
C
C Background color is black
C
      CALL GSCR(IWKID, 0, 0.0, 0.0, 0.0)
C
C Default foreground color is white
C
      CALL GSCR(IWKID, 1, 1.0, 1.0, 1.0)
C
C Red
C
      CALL GSCR(IWKID, 2, 1.0, 0.0, 0.0)
C
C Green
C
      CALL GSCR(IWKID, 3, 0.0, 1.0, 0.0)
C
C Blue 
C
      CALL GSCR(IWKID, 4, 0.4, 0.7, 0.9)
C
C Magenta
C
      CALL GSCR(IWKID, 5, 0.7, 0.4, 0.7)
C
C Orange
C
      CALL GSCR(IWKID, 6, 0.9, 0.7, 0.4)
C
C Teal
C
      CALL GSCR(IWKID, 7, 0.4, 0.9, 0.7)
      RETURN
      END
