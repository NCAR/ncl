
      PROGRAM CARFILL
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

      PARAMETER (LMAP=5000, NMAP=43, NPTS=50, NCNTR=5, NGRPS=2)
      PARAMETER (NWRK=1000)
      
      INTEGER MAP(LMAP), IAREA(NGRPS), IGRP(NGRPS)
      REAL XGEO(NMAP), YGEO(NMAP), X(12), Y(12)
      REAL XCNTR(NPTS), YCNTR1(NPTS), YCNTR2(NPTS), YCNTR3(NPTS)
      REAL YCNTR4(NPTS), YCNTR5(NPTS), XPERIM(5), YPERIM(5)
      REAL XWRK(NWRK), YWRK(NWRK)
      CHARACTER*14 STRING
      
      DATA XGEO / .63, .12, .05, .07, .10, .04, .19, .31, .31, .41,
     +     .39, .47, .64, .63, .70, .66, .67, .69, .76, .92,
     2     .95, .69, .64, .53, .53, .60, .63, .63, .72, .74,
     3     .79, .75, .75, .80, .75, .70, .68, .64, .63, .55,
     4     .55, .63, .63/
      DATA YGEO / .94, .95, .92, .85, .83, .78, .84, .75, .69, .58,
     +     .64, .55, .47, .37, .30, .05, .03, .05, .13, .26,
     2     .38, .52, .50, .57, .63, .63, .59, .64, .72, .71,
     3     .75, .75, .77, .78, .85, .83, .86, .86, .77, .80,
     4     .86, .90, .94/
      DATA XPERIM /0.0, 1.0, 1.0, 0.0, 0.0/
      DATA YPERIM /0.0, 0.0, 1.0, 1.0, 0.0/
      DATA X /.10, .22, .25, .25, .25, .50,
     +     .30, .47, .50, .77, .75, .68/
      DATA Y /.98, .70, .55, .38, .18, .18,
     +     .90, .85, .70, .35, .18, .05/
      
      EXTERNAL FILL
      
      XCNTR(1) = 0.0
      YCNTR1(1) = 0.25
      YCNTR2(1) = 0.40
      YCNTR3(1) = 0.60
      YCNTR4(1) = 0.80
      YCNTR5(1) = 0.95
      DO 10 J=2,NPTS
         DIST = REAL(J)/REAL(NPTS)
         XCNTR(J) = DIST
         YCNTR1(J) = .1*COS(REAL(4*3.14*DIST))+.15
         YCNTR2(J) = .1*COS(REAL(4*3.14*DIST))+.30
         YCNTR3(J) = .1*COS(REAL(4*3.14*DIST))+.50
         YCNTR4(J) = .1*COS(REAL(4*3.14*DIST))+.70
         YCNTR5(J) = .1*COS(REAL(4*3.14*DIST))+.85
 10   CONTINUE

C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)

      CALL COLOR(IWKID)
C
C Set the fill area option to solid
C
      CALL GSFAIS(1)
C
C Initialize Areas
C
      CALL ARINAM (MAP,LMAP)
C
C Add continents to area map in group 1.
C
      CALL AREDAM (MAP, XGEO, YGEO, NMAP, 1, 2, 1)
C
C Add contours and perimeter to area map in group 3.
C
      CALL AREDAM (MAP, XCNTR, YCNTR1, NPTS, 3, 2, 1)
      CALL AREDAM (MAP, XCNTR, YCNTR2, NPTS, 3, 3, 2)
      CALL AREDAM (MAP, XCNTR, YCNTR3, NPTS, 3, 4, 3)
      CALL AREDAM (MAP, XCNTR, YCNTR4, NPTS, 3, 5, 4)
      CALL AREDAM (MAP, XCNTR, YCNTR5, NPTS, 3, 6, 5)
C
C Fill water blue and contour lines according to area id.
C
      CALL ARSCAM(MAP,XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS,FILL)
C
C Write out area and group identifiers for each area, using red for
C geographic identifiers, and green for contour identifiers.
C
      CALL SFLUSH
      CALL GSPLCI(0)
      DO 20 I=1,12
         CALL ARGTAI(MAP, X(I), Y(I), IAREA, IGRP, NGRPS, NAI, 0)
         DO 30 J=1,2
            IF ((I.LE.6).OR.(I.EQ.12)) CALL GSPLCI(3)
            IF (I.EQ.7) CALL GSPLCI(0)
            IF (I.EQ.8) CALL GSPLCI(3)
            IF (I.EQ.9) CALL GSPLCI(6)
            IF (I.EQ.10) CALL GSPLCI(0)
            IF (I.EQ.11) CALL GSPLCI(3)
            WRITE (STRING,11) J,IAREA(J),J,IGRP(J)
            IF (IGRP(J).EQ.1) THEN
               CALL PLCHHQ (X(I), Y(I), STRING, .01, 0., 0.)
            ENDIF
            IF (IGRP(J).EQ.3) THEN
               CALL PLCHHQ (X(I), Y(I)-.02, STRING, .01, 0., 0.)
            ENDIF
 30      CONTINUE
 20   CONTINUE
C
C Draw contours and perimeter in white
C
      CALL GSPLCI (6)
      CALL CURVE (XCNTR,YCNTR1,NPTS)
      CALL CURVE (XCNTR,YCNTR2,NPTS)
      CALL CURVE (XCNTR,YCNTR3,NPTS)
      CALL CURVE (XCNTR,YCNTR4,NPTS)
      CALL CURVE (XCNTR,YCNTR5,NPTS)
      CALL CURVE (XPERIM,YPERIM,5)
      
      CALL FRAME
C
C Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
C Write out amount of space needed for the area map
C
      ISPACE = MAP(1) - MAP(6) + MAP(5)
      WRITE (6,*) 'Area Map Space Used: ',ISPACE
      
 11   FORMAT ('A(',I1,')=',I1,' G(',I1,')=',I1)
      STOP
      END
      
      SUBROUTINE COLOR(IWKID)
C
C Define color table
C
C Background is black
C
      CALL GSCR(IWKID,0,0.,0.,0.)
C
C Red
C
      CALL GSCR(IWKID,1,1.,0.,0.)
C
C Green
C
      CALL GSCR(IWKID,2,0.,1.,0.)
C
C Yellow
C
      CALL GSCR(IWKID,3,1.,1.,0.)
C
C Blue
C
      CALL GSCR(IWKID,4,0.,0.,1.)
C
C Magenta
C
      CALL GSCR(IWKID,5,1.,0.,1.)
C
C White
C
      CALL GSCR(IWKID,6,1.,1.,1.)
C
C Aqua
C
      CALL GSCR(IWKID,7,0.,1.,1.)

      RETURN
      END

      SUBROUTINE FILL(XWRK,YWRK,NWRK,IAREA,IGRP,NGRPS)

      INTEGER IAREA(NGRPS),IGRP(NGRPS),IDGEO,IDCNTR
      REAL XWRK(NWRK),YWRK(NWRK)

      IDGEO=-1
      IDCNTR=-1
C
C Retrieve geographic and contour area identifier info
C
      DO 10, I=1,NGRPS
         IF (IGRP(I).EQ.1) IDGEO=IAREA(I)
         IF (IGRP(I).EQ.3) IDCNTR=IAREA(I)
 10   CONTINUE
C
C If the contour segment is over water, then fill it in aquamarine
C
      IF (IDGEO .EQ. 1) THEN
         CALL GSFACI(7)
         CALL GFA(NWRK,XWRK,YWRK)
C
C If the contour band is over land, then fill it according to it's 
C area identifier info.
C
      ELSE IF ((IDGEO .EQ. 2) .AND. (IDCNTR .GE. 1)) THEN
         CALL GSFACI(IDCNTR)
         CALL GFA(NWRK,XWRK,YWRK)
      ENDIF

      RETURN
      END

