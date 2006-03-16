
      PROGRAM TISOSR
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
      CALL ISOSR(IERR)
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
      SUBROUTINE ISOSR (IERROR)
C
C PURPOSE                To provide a simple demonstration of ISOSRF.
C
C USAGE                  CALL ISOSR (IERROR)
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
C               ISOSRF TEST EXECUTED--SEE PLOTS TO CERTIFY
C
C                        is printed on unit 6.  In addition, 2
C                        frames are produced on the machine graphics
C                        device.  In order to determine if the test
C                        was successful, it is necessary to examine
C                        the plots.
C
C PRECISION              Single
C
C LANGUAGE               FORTRAN 77
C
C REQUIRED ROUTINES      ISOSRF
C
C REQUIRED GKS LEVEL     0A
C
C ALGORITHM              Values of a function on a 3-D rectangular grid
C                        are stored in array T.  Entries EZISOS and
C                        ISOSRF are called to draw iso-valued surface
C                        plots of the function.
C
      SAVE
      DIMENSION       T(21,31,19),SLAB(33,33),EYE(3)
C
C Specify coordinates for plot titles.
C
      REAL IX,IY
      DATA IX/.44/, IY/.95/
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
C
C     Frame 1 -- The EZISOS entry.
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(IX,IY,'DEMONSTRATION PLOT FOR ENTRY EZISOS OF ISOSRF',
     1 16.,0.,0.)
      CALL EZISOS (T,NU,NV,NW,EYE,SLAB,TISO)
C
C
C     Frame 2 -- The ISOSRF entry.
C
C Select normalization transformation 0.
C
      CALL GSELNT(0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ(IX,IY,'DEMONSTRATION PLOT FOR ENTRY ISOSRF OF ISOSRF',
     1 16.,0.,0.)
C
C Test ISOSRF with a subarray of T.
C
      MU=NU/2
      MV=NV/2
      MW=NW/2
      MUVWP2=MAX(MU,MV,MW)+2
      CALL ISOSRF(T(MU,MV,MW),NU,MU,NV,MV,MW,EYE,MUVWP2,SLAB,TISO,IFLAG)
      CALL FRAME
C
      IERROR = 0
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT (' ISOSRF TEST EXECUTED--SEE PLOTS TO CERTIFY')
C
      END
