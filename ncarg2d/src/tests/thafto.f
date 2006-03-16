
      PROGRAM THAFTO
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
      CALL HAFTO(IERR)
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
      SUBROUTINE HAFTO (IERROR)
C
C PURPOSE                Provides a simple demonstration of HAFTON
C
C USAGE                  CALL HAFTO (IERROR)
C
C ARGUMENTS
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, the test was not successful.
C
C I/O                    If the test is successful, the message
C               HAFTON TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 2 half-tone
C                        frames are produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plots.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C REQUIRED ROUTINES      HAFTON
C
C REQUIRED GKS LEVEL     0A
C
C ALGORITHM              The function
C                          Z(X,Y) = X + Y + 1./((X-.1)**2+Y**2+.09)
C                                   -1./((X+.1)**2+Y**2+.09)
C                        for X = -1. TO +1. in increments of .1, and
C                            Y = -1.2 TO +1.2 in increments of .1,
C                        is computed.  Then, entries EZHFTN and HAFTON
C                        are called to generate 2 half-tone plots of Z.
C
C Z contains the values to be plotted.
C
      REAL            Z(21,25)
      SAVE
C
C Specify coordinates for plot titles.  The values TX and TY
C define the center of the title string in a 0. to 1. range.
C
      DATA TX/0.0762/, TY/0.9769/
C
C Specify low (FLO) and high (FHI) contour values, and NLEV
C unique contour levels.  NOPT determines how Z maps onto the
C intensities, and the directness of the mapping.
C
      DATA FLO/-4.0/, FHI/4.0/, NLEV/8/, NOPT/-3/
C
C Initialize the error indicator
C
      IERROR = 0
C
C Fill the 2 dimensional array to be plotted
C
      DO  20 I=1,21
         X = .1*REAL(I-11)
         DO  10 J=1,25
            Y = .1*REAL(J-13)
            Z(I,J) = X+Y+1./((X-.10)**2+Y**2+.09)-
     1               1./((X+.10)**2+Y**2+.09)
   10    CONTINUE
   20 CONTINUE
C
C Select normalization trans 0 for plotting title.
C
      CALL GSELNT (0)
C
C
C     Frame 1 -- The EZHFTN entry with default parameters.
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ (TX,TY,
     1      'DEMONSTRATION PLOT FOR ENTRY EZHFTN OF HAFTON',16.,0.,-1.)
C
C Entry EZHFTN requires only the array and its dimensions.
C
      CALL EZHFTN (Z,21,25)
C
C
C     Frame 2 -- The HAFTON entry with user selectable parameters.
C
C   Add a plot title.
C
      CALL PLCHLQ (TX,TY,
     1      'DEMONSTRATION PLOT FOR ENTRY HAFTON OF HAFTON',16.,0.,-1.)
C
C Entry HAFTON allows user specification of plot parameters.
C
      CALL HAFTON (Z,21,21,25,FLO,FHI,NLEV,NOPT,0,0,0.)
      CALL FRAME
C
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT (' HAFTON TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END
