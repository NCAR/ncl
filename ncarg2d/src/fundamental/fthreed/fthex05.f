
      PROGRAM FTHEX05
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
C Declare the common block in which the viewing distance will be
C communicated to THREED.
C
      COMMON /TEMPRT/ RZRO
C
C Declare the common block in which the internal parameters of
C SRFACE are stored.
C
      COMMON /SRFIP1/ IFR,ISTP,IROTS,IDRX,IDRY,IDRZ,IUPPER,ISKIRT,
     +     NCLA,THETA,HSKIRT,CHI,CLO,CINC,ISPVAL
C
C Declare an array in which to put an eye position for THREED and
C both the eye position and the position of the point looked at
C for SRFACE.
C
      DIMENSION PEYE(6)
C
C Declare arrays in which to define the surface to be drawn by
C SRFACE and to provide the workspace it needs.
C
      DIMENSION U(21),V(21),W(21,21),RWRK(882)
C
C Define a character variable in which to form numeric labels.
C
      CHARACTER*8 CHRS
C
C Declare a function W(U,V) that defines the surface to be drawn.
C
      WFUN(UU,VV)=.5+.25*SIN(5.*UU)+.25*COS(5.*VV)
C
C Turn off frame advances by SRFACE.
C
      IFR=0
C
C Turn on the drawing of skirts by SRFACE and set the base value
C for the skirts.
C
      ISKIRT=1
      HSKIRT=0.
C
C Define the surface to be drawn.
C
      DO 101 I=1,21
         U(I)=REAL(I-1)/20.
 101  CONTINUE
C
      DO 102 J=1,21
         V(J)=REAL(J-1)/20.
 102  CONTINUE
C
      DO 104 I=1,21
         UTMP=REAL(I-1)/20.
         DO 103 J=1,21
            VTMP=REAL(J-1)/20.
            W(I,J)=WFUN(UTMP,VTMP)
 103     CONTINUE
 104  CONTINUE
C
C Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Make the tick marks drawn by PERIM3 twice as long as the default.
C
      CALL TICK43 (24,16,24,16,24,16)
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
C Define the distance from which the box, when viewed from the direction
C that makes it biggest, should just fill the screen.
C
      RZRO=8.
C
C Define the position of the eye.
C
      PEYE(1)=7.
      PEYE(2)=5.
      PEYE(3)=3.
C
C Define the position of the point looked at to be the point at the
C center of the box.  These three values in the array are seen only
C by SRFACE; THREED automatically assumes that you're looking at the
C center of the box.
C
      PEYE(4)=.5*(UMIN+UMAX)
      PEYE(5)=.5*(VMIN+VMAX)
      PEYE(6)=.5*(WMIN+WMAX)
C
C Communicate to SRFACE the dimensions of the box and the distance from
C which it is to be viewed.
C
      CALL SETR   (UMIN,UMAX,VMIN,VMAX,WMIN,WMAX,RZRO)
C
C Initialize THREED, using arguments that will make it use exactly the
C same projection as SRFACE.
C
      CALL SET3   (.0087976,.9902248,.0087976,.9902248,
     +     UMIN,UMAX,VMIN,VMAX,WMIN,WMAX,PEYE)
C
C Draw perimeters in each of the three coordinate planes.
C
      CALL PERIM3 (10,2,10,2,1,0.)
      CALL PERIM3 (10,2,10,2,2,0.)
      CALL PERIM3 (10,2,10,2,3,0.)
C
C Put some labels on the plot.  First, the U axis.
C
      CALL PWRZT (.5,0.,1.1,'U',1,3,-1,+3,0)
C 
      DO 106 ILAB=0,10
         UPOS=REAL(ILAB)/10.
         WRITE (CHRS,'(F8.1)') UPOS
         IBEG=0
         DO 105 ICHR=1,8
            IF (CHRS(ICHR:ICHR).NE.' ') THEN
               IF (IBEG.EQ.0) THEN
                  IBEG=ICHR
               END IF
               IEND=ICHR
            END IF
 105     CONTINUE
         CALL PWRZT (UPOS,0.,1.05,CHRS(IBEG:IEND),IEND-IBEG+1,
     +        3,-1,+3,0)
 106  CONTINUE
C
C Next, the V axis.
C  
      CALL PWRZT (0.,.5,1.1,'V',1,3,+2,+3,0)
C  
      DO 108 ILAB=1,10
         VPOS=REAL(ILAB)/10.
         WRITE (CHRS,'(F8.1)') VPOS
         IBEG=0
         DO 107 ICHR=1,8
            IF (CHRS(ICHR:ICHR).NE.' ') THEN
               IF (IBEG.EQ.0) THEN
                  IBEG=ICHR
               END IF
               IEND=ICHR
            END IF
 107     CONTINUE
         CALL PWRZT (0.,VPOS,1.05,CHRS(IBEG:IEND),IEND-IBEG+1,
     +        3,+2,+3,0)
 108  CONTINUE
C 
C Finally, the W axis.
C 
      CALL PWRZT (1.2,0.,.5,'W',1,3,-1,+3,1)
C 
      DO 110 ILAB=0,10
         WPOS=REAL(ILAB)/10.
         WRITE (CHRS,'(F8.1)') WPOS
         IBEG=0
         DO 109 ICHR=1,8
            IF (CHRS(ICHR:ICHR).NE.' ') THEN
               IF (IBEG.EQ.0) THEN
                  IBEG=ICHR
               END IF
               IEND=ICHR
            END IF
 109     CONTINUE
         CALL PWRZT (1.05,0.,WPOS,CHRS(IBEG:IEND),IEND-IBEG+1,
     +        3,-1,+3,1)
 110  CONTINUE
C 
C Using POINT3, draw grids inside the perimeters drawn by PERIM3.
C 
      DO 112 I=1,11
         PTMP=REAL(I-1)/10.
         DO 111 J=1,101
            QTMP=REAL(J-1)/100.
            CALL POINT3 (PTMP,QTMP,0.)
            CALL POINT3 (QTMP,PTMP,0.)
            CALL POINT3 (PTMP,0.,QTMP)
            CALL POINT3 (QTMP,0.,PTMP)
            CALL POINT3 (0.,PTMP,QTMP)
            CALL POINT3 (0.,QTMP,PTMP)
 111     CONTINUE
 112  CONTINUE
C 
C  Double the line width and draw the surface.
C 
      CALL PLOTIF (0.,0.,2)
      CALL GSLWSC (2.)
C 
      CALL SRFACE (U,V,W,RWRK,21,21,21,PEYE,0.)
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
