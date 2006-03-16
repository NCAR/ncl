
      PROGRAM CCPHLT
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

      PARAMETER (M=40,N=40,LRWK=3500,LIWK=4000)
      REAL Z(M,N), RWRK(LRWK)
      INTEGER IWRK(LIWK)
      
      CALL GENDAT(Z,M,M,N,1,25,1.03,24.88)
C 
C Open GKS, open and activate a workstation.
C 
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      CALL GSCLIP (0)
C
C Set up label options
C
      CALL CPSETC('HLT - HIGH/LOW TEXT',
     1     'Local High ($ZDV$)''Local Low ($ZDV$)')
      CALL CPSETI('HLX - HIGH/LOW SEARCH RADIUS IN X',3)
      CALL CPSETI('HLY - HIGH/LOW SEARCH RADIUS IN Y',2)
      CALL CPSETR('HLL - HIGH/LOW LINE WIDTH',3.0)
C
C Initialize Conpack
C
      CALL CPRECT(Z, M, M, N, RWRK, LRWK, IWRK, LIWK)
C
C Draw Perimeter
C
      CALL CPBACK(Z, RWRK, IWRK)
C
C Draw Contours
C
      CALL CPCLDR(Z,RWRK,IWRK)
      CALL CPLBDR(Z,RWRK,IWRK)
C     
C Close frame and close GKS
C
      CALL FRAME
C 
C Deactivate and close workstation, close GKS.
C 
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      
      STOP
      END
      
      SUBROUTINE GENDAT (DATA,IDIM,M,N,MLOW,MHGH,DLOW,DHGH)
C 
C This is a routine to generate test data for two-dimensional graphics
C routines.  Given an array "DATA", dimensioned "IDIM x 1", it fills
C the sub-array ((DATA(I,J),I=1,M),J=1,N) with a two-dimensional field
C of data having approximately "MLOW" lows and "MHGH" highs, a minimum
C value of exactly "DLOW" and a maximum value of exactly "DHGH".
C 
C "MLOW" and "MHGH" are each forced to be greater than or equal to 1
C and less than or equal to 25.
C 
C The function used is a sum of exponentials.
C 
      DIMENSION DATA(IDIM,1),CCNT(3,50)
C 
      FOVM=9./REAL(M)
      FOVN=9./REAL(N)
C 
      NLOW=MAX(1,MIN(25,MLOW))
      NHGH=MAX(1,MIN(25,MHGH))
      NCNT=NLOW+NHGH
C 
      DO 101 K=1,NCNT
         CCNT(1,K)=1.+(REAL(M)-1.)*FRAN()
         CCNT(2,K)=1.+(REAL(N)-1.)*FRAN()
         IF (K.LE.NLOW) THEN
            CCNT(3,K)=-1.
         ELSE
            CCNT(3,K)=+1.
         END IF
 101  CONTINUE
C 
      DMIN=+1.E36
      DMAX=-1.E36
      DO 104 J=1,N
         DO 103 I=1,M
            DATA(I,J)=.5*(DLOW+DHGH)
            DO 102 K=1,NCNT
               TEMP=-((FOVM*(REAL(I)-CCNT(1,K)))**2+
     1               (FOVN*(REAL(J)-CCNT(2,K)))**2)
               IF (TEMP.GE.-20.) DATA(I,J)=DATA(I,J)+
     1               .5*(DHGH-DLOW)*CCNT(3,K)*EXP(TEMP)
 102        CONTINUE
            DMIN=MIN(DMIN,DATA(I,J))
            DMAX=MAX(DMAX,DATA(I,J))
 103     CONTINUE
 104  CONTINUE
C 
      DO 106 J=1,N
         DO 105 I=1,M
            DATA(I,J)=(DATA(I,J)-DMIN)/(DMAX-DMIN)*(DHGH-DLOW)+DLOW
 105     CONTINUE
 106  CONTINUE
C 
      RETURN
C 
      END
      FUNCTION FRAN ()
      DIMENSION RSEQ (100)
      SAVE ISEQ
      DATA RSEQ / .749,.973,.666,.804,.081,.483,.919,.903,.951,.960 ,
     1      .039,.269,.270,.756,.222,.478,.621,.063,.550,.798 ,
     2      .027,.569,.149,.697,.451,.738,.508,.041,.266,.249 ,
     3      .019,.191,.266,.625,.492,.940,.508,.406,.972,.311 ,
     4      .757,.378,.299,.536,.619,.844,.342,.295,.447,.499 ,
     5      .688,.193,.225,.520,.954,.749,.997,.693,.217,.273 ,
     6      .961,.948,.902,.104,.495,.257,.524,.100,.492,.347 ,
     7      .981,.019,.225,.806,.678,.710,.235,.600,.994,.758 ,
     8      .682,.373,.009,.469,.203,.730,.588,.603,.213,.495 ,
     9      .884,.032,.185,.127,.010,.180,.689,.354,.372,.429 /
      DATA ISEQ / 0 /
      ISEQ=MOD(ISEQ,100)+1
      FRAN=RSEQ(ISEQ)
      RETURN
      END
