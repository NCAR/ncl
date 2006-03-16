
      PROGRAM TISOHR
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
      CALL ISOSCR()
      CALL ISOHR(IERR)
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
      SUBROUTINE ISOHR (IERROR)
C
C PURPOSE                To provide a simple demonstration of ISOSRFHR.
C
C USAGE                  CALL ISOHR (IERROR)
C
C ARGUMENTS
C
C ON OUTPUT              IERROR
C                          An integer variable
C                          = 0, if the test was successful,
C                          = 1, the test was not successful.
C
C I/O                    A scratch file must be assigned to unit IUNIT.
C                        Common block UNITS should be inserted in the
C                        calling program.  Then, set IUNIT in the
C                        calling program.
C
C                        If the test is successful, the message
C
C               ISOSRFHR TEST EXECUTED--SEE PLOT TO CERTIFY
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
C REQUIRED ROUTINES      ISOSRFHR
C
C REQUIRED GKS LEVEL     0A
C
C ALGORITHM              This test program draws a perspective view
C                        of 2 interlocking doughnuts.
C
      DIMENSION       EYE(3)     ,S(4)       ,IS2(4,200) ,
     1                ST1(81,51,2)           ,IOBJS(81,51)
      COMMON /UNITS/  IUNIT
      SAVE
C
C Specify coordinates for plot titles on a square grid of integer
C coordinates that range from 1 to 1024.  IX and IY define the center
C of the title string.
C
      DATA IX/448/, IY/990/
C
C
C Define the eye position.
C
      DATA            EYE(1), EYE(2), EYE(3) / 200., 250., 250. /
C
C Define the overall dimensions of the box containing the objects.
C
      DATA            NU, NV, NW /  51, 81, 51 /
C
C Specify the dimensions of the model of the image plane.
C
      DATA            LX, NX, NY / 4, 180, 180 /
C
C Specify the user viewport in a 1 to 1024 range.
C
      DATA            S(1),S(2),S(3),S(4)/ 10.,1010.,10.,1010./
      DATA            MV / 81 /
C
C Specify the large and small radii for the individual doughnuts.
C
      DATA            RBIG1,RBIG2,RSML1,RSML2/ 20., 20., 6., 6. /
C
C
C Call the initialization routine.
C
      CALL INIT3D (EYE,NU,NV,NW,ST1,LX,NY,IS2,IUNIT,S)
C
C Initialize the error indicator
C
      IERROR = 1
C
C Create and plot the interlocking doughnuts.
C
      JCENT1 = REAL(NV)*.5-RBIG1*.5
      JCENT2 = REAL(NV)*.5+RBIG2*.5
      DO  70 IBKWDS=1,NU
         I = NU+1-IBKWDS
C
C Create the i-th cross section in the U direction of the 3-D array
C and store in IOBJS as zeros and ones.
C
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
               TEMP = MIN(FIMID**2+FJP1**2+FKP1**2-RSML1**2,
     1                                FKMID**2+FIP2**2+FJP2**2-RSML2**2)
               IF (TEMP .LE. 0.) IOBJS(J,K) = 1
               IF (TEMP .GT. 0.) IOBJS(J,K) = 0
   10       CONTINUE
   20    CONTINUE
C
C Set proper words to 1 for drawing axes.
C
         IF (I .NE. 1) GO TO  50
         DO  30 K=1,NW
            IOBJS(1,K) = 1
   30    CONTINUE
         DO  40 J=1,NV
            IOBJS(J,1) = 1
   40    CONTINUE
         GO TO  60
   50    CONTINUE
         IOBJS(1,1) = 1
   60    CONTINUE
C
C Call the draw and remember routine for this slab.
C
      CALL DANDR (NV,NW,ST1,LX,NX,NY,IS2,IUNIT,S,IOBJS,MV)
   70 CONTINUE
C
      CALL GQCNTN(IER,ICN)
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      XC = CPUX(IX)
      YC = CPUY(IY)
      CALL PLCHLQ(XC,YC,'DEMONSTRATION PLOT FOR ISOSRFHR',16.,0.,0.)
      CALL GSELNT(ICN)
C
C Advance the frame.
C
      CALL FRAME
C
      IERROR = 0
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT (' ISOSRFHR TEST EXECUTED--SEE PLOT TO CERTIFY')
C
      END
