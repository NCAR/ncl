
      PROGRAM FTHEX01
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
C Declare an array in which to put an eye position for THREED.
C
      DIMENSION PEYE(3)
C
C Define a character variable in which to form numeric labels.
C
      CHARACTER*8 CHRS
C
C Declare a function W(U,V) to be used in the example.
C
      WFUN(U,V)=.5+.25*SIN(5.*U)+.25*COS(5.*V)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Make the tick marks drawn by PERIM3 different from the default.
C
      CALL TICK43 (12,8,24,16,48,32)
C
C Define the boundaries of the box to be projected from 3-space to
C 2-space.
C
      UMIN=0.
      UMAX=1.
      VMIN=0.
      VMAX=1.
      WMIN=0.
      WMAX=1.
C
C Define the position of the eye.
C
      PEYE(1)=6.
      PEYE(2)=4.
      PEYE(3)=5.
C
C Initialize THREED.
C
      CALL SET3 (.1,.9,.1,.9,UMIN,UMAX,VMIN,VMAX,WMIN,WMAX,PEYE)
C
C Draw perimeters in each of the three coordinate planes.
C
      CALL PERIM3 (10,2,10,2,1,0.)
      CALL PERIM3 (10,2,10,2,2,0.)
      CALL PERIM3 (10,4,10,4,3,0.)
C
C Put some labels on the plot.  First, the U axis.
C
      CALL PWRZT (.5,0.,1.1,'U',1,3,-1,+3,0)
C
      DO 102 ILAB=1,10
         UPOS=REAL(ILAB)/10.
         WRITE (CHRS,'(F8.1)') UPOS
         IBEG=0
         DO 101 ICHR=1,8
            IF (CHRS(ICHR:ICHR).NE.' ') THEN
               IF (IBEG.EQ.0) THEN
                  IBEG=ICHR
               END IF
               IEND=ICHR
            END IF
 101     CONTINUE
         IF (CHRS(IBEG:IBEG).EQ.'0') IBEG=MIN(IBEG+1,IEND)
         CALL PWRZT (UPOS,0.,1.05,CHRS(IBEG:IEND),IEND-IBEG+1,
     +        3,-1,+3,0)
 102  CONTINUE
C
C Next, the V axis.
C
      CALL PWRZT (0.,.5,1.1,'V',1,3,+2,+3,0)
C
      DO 104 ILAB=1,10
         VPOS=REAL(ILAB)/10.
         WRITE (CHRS,'(F8.1)') VPOS
         IBEG=0
         DO 103 ICHR=1,8
            IF (CHRS(ICHR:ICHR).NE.' ') THEN
               IF (IBEG.EQ.0) THEN
                  IBEG=ICHR
               END IF
               IEND=ICHR
            END IF
 103     CONTINUE
         IF (CHRS(IBEG:IBEG).EQ.'0') IBEG=MIN(IBEG+1,IEND)
         CALL PWRZT (0.,VPOS,1.05,CHRS(IBEG:IEND),IEND-IBEG+1,
     +        3,+2,+3,0)
 104  CONTINUE
C
C Finally, the W axis.
C
      CALL PWRZT (1.2,0.,.5,'W',1,3,-1,+3,1)
C
      DO 106 ILAB=0,10
         WPOS=REAL(ILAB)/10.
         WRITE (CHRS,'(F8.1)') WPOS
         IBEG=0
         DO 105 ICHR=1,8
            IF (CHRS(ICHR:ICHR).NE.' ') THEN
               IF (IBEG.EQ.0) THEN
                  IBEG=ICHR
               END IF
               IEND=ICHR
            END IF
 105     CONTINUE
         IF (CHRS(IBEG:IBEG).EQ.'0') IBEG=MIN(IBEG+1,IEND)
         CALL PWRZT (1.05,0.,WPOS,CHRS(IBEG:IEND),IEND-IBEG+1,
     +        3,-1,+3,1)
 106  CONTINUE
C
C Using POINT3, draw grids inside the perimeters drawn by PERIM3.
C
      DO 108 I=1,11
         PTMP=REAL(I-1)/10.
         DO 107 J=1,101
            QTMP=REAL(J-1)/100.
            CALL POINT3 (PTMP,QTMP,0.)
            CALL POINT3 (QTMP,PTMP,0.)
            CALL POINT3 (PTMP,0.,QTMP)
            CALL POINT3 (QTMP,0.,PTMP)
            CALL POINT3 (0.,PTMP,QTMP)
            CALL POINT3 (0.,QTMP,PTMP)
 107     CONTINUE
 108  CONTINUE
C
C Double the line width and draw a wire-frame representation of the
C surface defined by the function WFUN, using the routines FRST3 and
C VECT3.
C
      CALL PLOTIF (0.,0.,2)
      CALL GSLWSC (2.)
C
      DO 110 I=1,11
         UTMP=REAL(I-1)/10.
         CALL FRST3 (UTMP,0.,WFUN(UTMP,0.))
         DO 109 J=2,11
            VTMP=REAL(J-1)/10.
            CALL VECT3 (UTMP,VTMP,WFUN(UTMP,VTMP))
 109     CONTINUE
 110  CONTINUE
C
      DO 112 J=1,11
         VTMP=REAL(J-1)/10.
         CALL FRST3 (0.,VTMP,WFUN(0.,VTMP))
         DO 111 I=2,11
            UTMP=REAL(I-1)/10.
            CALL VECT3 (UTMP,VTMP,WFUN(UTMP,VTMP))
 111     CONTINUE
 112  CONTINUE
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
