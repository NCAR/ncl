
      PROGRAM MPEXFI
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
C Define a data array.
C
      DIMENSION XYCD(224)
C
C Define the centers and the expansion/shrinkage factors for
C various copies of the curve to be drawn.
C
      DIMENSION FLAC(4),FLOC(4),FMUL(4)
C
      DATA FLOC(1),FLAC(1),FMUL(1) / -38.1,32.0,1.7 /
      DATA FLOC(2),FLAC(2),FMUL(2) / -37.9,32.0,1.7 /
      DATA FLOC(3),FLAC(3),FMUL(3) / -38.0,31.9,1.7 /
      DATA FLOC(4),FLAC(4),FMUL(4) / -38.0,32.1,1.7 /
C
C Open GKS.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Fill the data array.
C
      READ 1001 , XYCD
C
C Define the altitude of the satellite.
C
      CALL MAPSTR ('SA',2.)
C
C Draw a map of the North Atlantic, as seen by a satellite.
C
      CALL SUPMAP (7,32.,-38.,20.,0.,0.,0.,0.,1,-1000,5,0,IERR)
C
C Force MAPIT to draw dotted lines.
C
      CALL MAPSTI ('DL',1)
      CALL MAPSTI ('DD',24)
C
C Draw some curves.
C
      DO 102 I=1,4
C
        IFST=0
C
        DO 101 J=1,112
          IF (XYCD(2*J-1).EQ.0.) THEN
            IFST=0
          ELSE
            FLON=FLOC(I)+FMUL(I)*(XYCD(2*J-1)-15.)
            FLAT=FLAC(I)+FMUL(I)*(XYCD(2*J  )-15.)
            CALL MAPIT (FLAT,FLON,IFST)
            IFST=1
          END IF
  101   CONTINUE
C
  102 CONTINUE
C
C Dump MAPIT's buffers.
C
      CALL MAPIQ
C
C Draw a boundary around the edge of the plotter frame.
C
      CALL BNDARY
C
C Advance the frame.
C
      CALL FRAME
C
C Close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
C Done.
C
      STOP
C
C Format.
C
 1001 FORMAT (14E5.0)
C
      END
