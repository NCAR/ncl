
      PROGRAM TSTITL
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
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C INVOKE DEMO DRIVER
C
      CALL STITL(IERR)
C
C DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
      SUBROUTINE STITL (IERROR)
C
C PURPOSE                To provide a simple demonstration of the
C                        routine STITLE.
C
C USAGE                  CALL STITL (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                            = 0  If there is a normal exit from STITLE
C                            = 1  Otherwise
C
C I/O                    If there is a normal exit from STITLE,
C                        the message
C
C                          STITLE TEST SUCCESSFUL . . . SEE PLOTS TO
C                          VERIFY PERFORMANCE
C
C                        is written on unit 6
C
C PRECISION              SINGLE
C
C REQUIRED LIBRARY       STITLE
C FILES
C
C LANGUAGE               FORTRAN
C
C HISTORY                Written  by members of the
C                        Scientific Computing Division of NCAR,
C                        Boulder Colorado
C
C PORTABILITY            FORTRAN 77
C
C
      CHARACTER*80    CARDS(6)
C
C Initialize the error parameter.
C
      IERROR = 1
C
C Store character strings in array CARDS.  These strings contain text,
C plus information regarding character size and location of the text
C on the scroll.
C
      NCARDS = 4
      CARDS(1) = '  512  760    1  1.5Demonstration'
      CARDS(2) = '  512  600    1  1.5Plot'
      CARDS(3) = '  512  440    1  1.0for'
      CARDS(4) = '  512  280    1  1.5STITLE'
C
C Define the remaining inputs to routine STITLE.  Note that the
C output produced (a single frame with no scrolling to appear for
C 6.0 seconds) could equally well have been produced by FTITLE.
C We call STITLE in this demo to avoid reading the input lines.
C
      NYST  = 512
      NYFIN = 512
      TST   = 0.0
      TMV   = 0.0
      TFIN  = 6.0
      MOVIE =   1
C
C Call STITLE.
C
      CALL STITLE (CARDS,NCARDS,NYST,NYFIN,TST,TMV,TFIN,MOVIE)
      IERROR = 0
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT ('     STITLE TEST SUCCESSFUL',24X,
     1        'SEE PLOTS TO VERIFY PERFORMANCE')
C
      END
