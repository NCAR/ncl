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
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C Select normalization transformation 0 (user coordinates are the same
C as NDC coordinates), so that title is drawn at top of plot.
C
      CALL GSELNT (0)
C
C Call PLCHLQ to write the plot title.
C
      CALL PLCHLQ (.5,.9765,'Example Streamlines Plot',16.,
     1            0.,0.)
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
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
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
      TPIMX = 2.*3.14/FLOAT(M)
      TPJMX = 2.*3.14/FLOAT(N)
      DO  20 J=1,N
         DO  10 I=1,M
            U(I,J) = SIN(TPIMX*(FLOAT(I)-1.))
            V(I,J) = SIN(TPJMX*(FLOAT(J)-1.))
  10     CONTINUE
  20  CONTINUE

      RETURN
      END
