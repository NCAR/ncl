
      PROGRAM CCPEZCT
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
      
      PARAMETER (M=50, N=50)
      REAL ZREG(M,N)
C      
C Get some data
C
      CALL MKDAT (3,ZREG, M, N)
C 
C Open GKS, open and activate a workstation.
C 
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C 
C Call CPEZCT
C 
      CALL CPEZCT (ZREG, M, N)
C 
C Deactivate and close workstation, close GKS.
C 
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C      
C Quit
C
      STOP
      END
      
      SUBROUTINE MKDAT(NCTFR,ZDAT,M,N)
C 
C Create some data to contour
C 
      REAL ZDAT(M,N)
C 
C NCTFR is used to generate a random number, ZDAT is the data array
C 
      CALL GGDINI (0.,1.,NCTFR,.9)
      ZMIN= 1.E36
      ZMAX=-1.E36
      DO 10 I=1,M
         RLON=.017453292519943*(-180.+360.*REAL(I-1)/REAL(M-1))
         DO 20 J=1,N
            RLAT=.017453292519943*(-90.+180.*REAL(J-1)/REAL(N-1))
            ZDAT(I,J)=GGDPNT(RLAT,RLON)+.5*COS(4.*RLAT)
            ZMIN=MIN(ZMIN,ZDAT(I,J))
            ZMAX=MAX(ZMAX,ZDAT(I,J))
 20      CONTINUE
 10   CONTINUE
      DO 30 I=1,M
         DO 40 J=1,N
            ZDAT(I,J)=((ZDAT(I,J)-ZMIN)/(ZMAX-ZMIN))*130.-50.
 40      CONTINUE
 30   CONTINUE
      
      RETURN
      END
