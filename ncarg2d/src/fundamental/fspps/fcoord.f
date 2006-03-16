
      PROGRAM FCOORD
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
C Define arrays to hold data defining a spiral in the user coordinate
C system.
C
      DIMENSION X(476),Y(476)
C
C Define arrays to hold the numbers defining the viewport and window,
C as retrieved from GKS.
C
      DIMENSION VIEW(4),WIND(4)
C
C Define a character variable in which to construct labels.
C
      CHARACTER*26 CHRS
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Turn off clipping at the edges of the viewport (which GKS does by
C default).
C
      CALL GSCLIP (0)
C
C Define the X and Y coordinates of a spiral in the user coordinate
C system.  It lies in a rectangular region bounded by the lines
C "X=100", "X=1000", "Y=100", and "Y=1000".
C
      DO 101 I=1,476
         THETA=.031415926535898*REAL(I-1)
         X(I)=500.+.9*REAL(I-1)*COS(THETA)
         Y(I)=500.+.9*REAL(I-1)*SIN(THETA)
 101  CONTINUE
C
C Loop through the possible values of 'LS'.
C
      DO 103 ILS=1,4
C
C Define the fractional coordinates of the left and right edges of the
C viewport.
C
         VPL=REAL(ILS-1)/4.+.020
         VPR=REAL(ILS  )/4.-.020
C
C For each of the possible values of 'LS', loop through the possible
C values of 'MI'.
C
         DO 102 IMI=1,4
C
C Define the fractional coordinates of the bottom and top edges of the
C viewport.
C
            VPB=REAL(4-IMI)/4.+.059
            VPT=REAL(5-IMI)/4.-.001
C
C Outline the viewport.  PLOTIF expects fractional coordinates.
C
            CALL PLOTIF (VPL,VPB,0)
            CALL PLOTIF (VPR,VPB,1)
            CALL PLOTIF (VPR,VPT,1)
            CALL PLOTIF (VPL,VPT,1)
            CALL PLOTIF (VPL,VPB,1)
C
C Call SET to define the mapping from the user system to the plotter
C frame.  The SET call specifies 'MI' = 1 (since the value of argument
C 5 is less than that of argument 6 and the value of argument 7 is less
C that of argument 8).  The SETUSV call overrides this to obtain the
C desired value.
C
            CALL SET    (VPL,VPR,VPB,VPT,100.,1000.,100.,1000.,ILS)
            CALL SETUSV ('MI (MIRROR IMAGING FLAG)',IMI)
C
C Call the routine CURVE to draw the spiral.
C
            CALL CURVE  (X,Y,476)
C
C Label the curve.  First, write the values of 'MI' and 'LS'.  Note
C the use of CFUX and CFUY to map meaningful fractional coordinates
C to the user coordinates required by PLCHMQ.
C
            WRITE (CHRS,'(''MI='',I1,'' LS='',I1)') IMI,ILS
            CALL PLCHMQ (CFUX(.5*(VPL+VPR)),CFUY(VPB-.0120),
     +           CHRS(1:9),.012,0.,0.)
C
C Retrieve the values defining the window and viewport, using GKS
C calls.
C
            CALL GQNT (1,IERR,WIND,VIEW)
C
C Write them out, too.
C
            WRITE (CHRS,'(''VP='',F5.3,3('','',F5.3))') (VIEW(I),I=1,4)
            CHRS( 4: 4)=' '
            CHRS(10:10)=' '
            CHRS(16:16)=' '
            CHRS(22:22)=' '
            CALL PLCHMQ (CFUX(.5*(VPL+VPR)),CFUY(VPB-.0320),
     +           CHRS(1:26),.008,0.,0.)
            WRITE (CHRS,'(''WD='',F5.0,3('','',F5.0))') (WIND(I),I=1,4)
            CALL PLCHMQ (CFUX(.5*(VPL+VPR)),CFUY(VPB-.0480),
     +           CHRS(1:26),.008,0.,0.)
C
C End of loop through the values of 'MI'.
C
 102     CONTINUE
C
C End of loop through the values of 'LS'.
C
 103  CONTINUE
C
C Advance the frame.
C
      CALL FRAME
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
C Done.
C
      STOP
C
      END
