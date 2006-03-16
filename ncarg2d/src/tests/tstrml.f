
      PROGRAM TSTRML
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
      CALL STRML(IERR)
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
      SUBROUTINE STRML (IERROR)
C
C PURPOSE                To provide a simple demonstration of STRMLN.
C
C USAGE                  CALL STRML (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, the test was not successful.
C
C I/O                    If the test is successful, the message
C
C               STRMLN TEST EXECUTED--SEE PLOT TO CERTIFY
C
C                        is printed on unit 6.  In addition, 1
C                        frame is produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plot.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C REQUIRED ROUTINES      STRMLN
C
C REQUIRED GKS LEVEL     0A
C
C ALGORITHM              Routine TSTRML calls routine STRMLN to
C                        produce a plot which depicts the flow and
C                        magnitude of a vector field.
C
      REAL            U(21,25)   ,V(21,25)   ,WRK(1050)
C
C Specify coordinates for plot titles.  The values TX and TY
C define the center of the title string in a 0. to 1. range.
C
      DATA TX/.5/,TY/.9765/
C
C Set the grid dimensions.
C
      DATA NH,NV/21,25/
C
C Initialize the error parameter.
C
      IERROR = 1
C
C Specify horizontal and vertical vector components U and V on
C the rectangular grid.
C
      TPIMX = 2.*3.14/REAL(NH)
      TPJMX = 2.*3.14/REAL(NV)
      DO  20 J=1,NV
         DO  10 I=1,NH
         U(I,J) = SIN(TPIMX*(REAL(I)-1.))
         V(I,J) = SIN(TPJMX*(REAL(J)-1.))
   10    CONTINUE
   20 CONTINUE
C
C Select normalization transformation 0.
C
      CALL GSELNT (0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ (TX,TY,'DEMONSTRATION PLOT FOR ROUTINE STRMLN',16.,
     1            0.,0.)
C
C Define normalization transformation 1, and set up log scaling.
C
      CALL SET(0.1, 0.9, 0.1, 0.9,1.0, 21., 1.0, 25.,1)
C
C Draw the plot perimeter.
C
      CALL PERIM(1,0,1,0)
C
C Call STRMLN for vector field streamlines plot.
C
      CALL STRMLN (U,V,WRK,NH,NH,NV,1,IER)
C
      CALL FRAME
C
      IERROR = 0
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT (' STRMLN TEST EXECUTED--SEE PLOT TO CERTIFY')
C
      END
