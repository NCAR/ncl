
      PROGRAM TPWRZI
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
      CALL TPWRZI1(IERR)
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
      SUBROUTINE TPWRZI1 (IERROR)
C
C PURPOSE                To provide a simple demonstration of
C                        entry PWRZI with the ISOSRF utility.
C
C USAGE                  CALL TPWRZI1 (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, otherwise
C
C I/O                    If the test is successful, the message
C
C               PWRZI TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 1
C                        frame is produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plot.
C
C PRECISION              Single
C
C REQUIRED ROUTINES      PWRZI, ISOSRF
C
C REQUIRED GKS LEVEL     0A
C
C LANGUAGE               FORTRAN 77
C
C ALGORITHM              A function of 3 variables is defined and the
C                        values of the function on a 3-D rectangular
C                        grid are stored in an array.  This test routine
C                        then calls ISOSRF to draw an iso-valued surface
C                        plot of the function.  PWRZI is then called 3
C                        times to label the front, side, and back of
C                        the picture.
C
      DIMENSION       T(21,31,19),SLAB(33,33),EYE(3)
C
C Define the center of a plot title string on a square grid of size
C 0. to 1.
C
      DATA TX/0.4375/, TY/0.9667/
C
      DATA NU,NV,NW/21,31,19/
      DATA RBIG1,RBIG2,RSML1,RSML2/6.,6.,2.,2./
      DATA TISO/0./
      DATA MUVWP2/33/
      DATA IFLAG/-7/
C
C Initialize the error parameter.
C
      IERROR = 1
C
C Fill the 3-D array to be plotted.
C
      JCENT1 = REAL(NV)*.5-RBIG1*.5
      JCENT2 = REAL(NV)*.5+RBIG2*.5
      DO  30 I=1,NU
         FIMID = I-NU/2
         DO  20 J=1,NV
            FJMID1 = J-JCENT1
            FJMID2 = J-JCENT2
            DO  10 K=1,NW
               FKMID = K-NW/2
               F1 = SQRT(RBIG1*RBIG1/(FJMID1*FJMID1+FKMID*FKMID+.1))
               F2 = SQRT(RBIG2*RBIG2/(FIMID*FIMID+FJMID2*FJMID2+.1))
               FIP1 = (1.-F1)*FIMID
               FIP2 = (1.-F2)*FIMID
               FJP1 = (1.-F1)*FJMID1
               FJP2 = (1.-F2)*FJMID2
               FKP1 = (1.-F1)*FKMID
               FKP2 = (1.-F2)*FKMID
               T(I,J,K) = MIN(FIMID*FIMID+FJP1*FJP1+FKP1*FKP1-
     1                    RSML1*RSML1,
     2                      FKMID*FKMID+FIP2*FIP2+FJP2*FJP2-RSML2*RSML2)
   10       CONTINUE
   20    CONTINUE
   30 CONTINUE
C
C Define the eye position.
C
      EYE(1) = 100.
      EYE(2) = 150.
      EYE(3) = 125.
C
C Select normalization transformation number 0.
C
      CALL GSELNT (0)
C
C Label the plot.
C
      CALL PLCHLQ (TX,TY,'DEMONSTRATION PLOT FOR PWRZI',16.,0.,0.)
C
C Test ISOSRF with subarray T.
C
      MU = NU/2
      MV = NV/2
      MW = NW/2
      MUVWP2 = MAX(MU,MV,MW)+2
      CALL ISOSRF (T(MU,MV,MW),NU,MU,NV,MV,MW,EYE,MUVWP2,SLAB,TISO,
     1             IFLAG)
      ISIZE = 35
      CALL PWRZI (5.,16.,.5,'FRONT',5,ISIZE,-1,3,0)
      CALL PWRZI (11.,7.5,.5,'SIDE',4,ISIZE,2,-1,0)
      CALL PWRZI (5.,1.,5.,' BACK BACK BACK BACK BACK',25,ISIZE,-1,3,0)
      CALL FRAME
      IERROR = 0
C
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT (' PWRZI TEST EXECUTED--SEE PLOT TO CERTIFY')
C
      END
