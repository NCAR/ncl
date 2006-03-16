
      PROGRAM FSTREAM
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

      PARAMETER (M=21,N=25)
      REAL U(M,N), V(M,N), WRK(2*M*N)
      INTEGER IDM
C
C IDM is a dummy variable for STINIT and STREAM.
C
C Generate some data
C
      CALL MKDAT(U,V,M,N)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Select normalization transformation 0 (user coordinates are the same
C as NDC coordinates), so that title is drawn at top of plot.
C
      CALL GSELNT (0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ (.5,.9765,'Example Streamlines Plot',16.,
     1     0.,0.)
C
C Define normalization transformation 1, and set up linear scaling.
C
      CALL SET(0.1, 0.9, 0.1, 0.9,1.0, 21., 1.0, 25.,1)
C
C Tell Streamlines that SET has been called, and
C set spacing of stream lines.
C
      CALL STSETI('SET -- Set Call Flag', 0)
      CALL STSETR('SSP -- Stream Spacing', 0.015)
C
C Initialize Streamlines, and draw streamlines
C
      CALL STINIT(U,M,V,M,IDM,IDM,M,N,WRK,2*M*N)
      CALL STREAM(U,V,IDM,IDM,IDM,WRK)
C
C Close Frame
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

      SUBROUTINE MKDAT(U,V,M,N)

      REAL U(M,N),V(M,N)
C
C Specify horizontal and vertical vector components U and V on
C the rectangular grid. And set up a special value area near the
C center.
C
      TPIMX = 2.*3.14/REAL(M)
      TPJMX = 2.*3.14/REAL(N)
      DO  20 J=1,N
         DO  10 I=1,M
            U(I,J) = SIN(TPIMX*(REAL(I)-1.))
            V(I,J) = SIN(TPJMX*(REAL(J)-1.))
  10     CONTINUE
  20  CONTINUE

      RETURN
      END
