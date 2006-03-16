
      PROGRAM FFEX01
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
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Invoke demo driver
C
      CALL FEX01(IERR)
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
C
      SUBROUTINE FEX01 (IERROR)
C
      PARAMETER (PI=3.14159) 
      PARAMETER (MSIZE=21, NSIZE=25, IWSIZE = 2*MSIZE*NSIZE)
C
      REAL U(MSIZE,NSIZE), V(MSIZE,NSIZE), WRK(IWSIZE)
C
C Set the grid dimensions.
C
      DATA M, N /MSIZE, NSIZE/
C
C Specify horizontal and vertical vector components U and V on
C the rectangular grid.
C
      GISIZE = 2.0*PI/REAL(M)
      GJSIZE = 2.0*PI/REAL(N)
      DO  20 J = 1,N
         DO  10 I = 1,M
            U(I,J) = COS(GISIZE*(REAL(I)-1.0))
            V(I,J) = COS(GJSIZE*(REAL(J)-1.0))
   10    CONTINUE
   20 CONTINUE
C
C Draw the field with streamlines overlaid on vectors
C
      IDM = 0
      RDM = 0.0
      CALL VVINIT(U,M,V,M,RDM,IDM,M,N,RDM,IDM)
      CALL VVECTR(U,V,RDM,IDM,IDM,RDM)
      CALL STINIT(U,M,V,M,RDM,IDM,M,N,WRK,IWSIZE)
      CALL STREAM(U,V,RDM,IDM,IDM,WRK)
      CALL PERIM(1,0,1,0)
      CALL FRAME
C
C Draw just the vectors
C 
C
      IDM = 0
      RDM = 0.0
      CALL VVINIT(U,M,V,M,RDM,IDM,M,N,RDM,IDM)
      CALL VVECTR(U,V,RDM,IDM,IDM,RDM)
      CALL PERIM(1,0,1,0)
      CALL FRAME
C
C Draw just the streamlines
C
      IDM = 0
      RDM = 0.0
      CALL STINIT(U,M,V,M,RDM,IDM,M,N,WRK,IWSIZE)
      CALL STREAM(U,V,RDM,IDM,IDM,WRK)
      CALL PERIM(1,0,1,0)
      CALL FRAME
C
      END
