C
C	$Id: stex01.f,v 1.4 1993-02-26 23:49:57 dbrown Exp $
C
      PROGRAM STEX01
C
      PARAMETER ( M=20 , N=36 , NPR=155)
      DIMENSION A(M,N),B(M,N)
      DIMENSION WRK(2*M*N)
C
C     Open GKS, open workstation, activate workstation.
C
      CALL GOPKS (6,ISZ)
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1)
C
C
      CALL SET (0.05,0.95,0.05,0.95,-20.0,20.0,-20.0,20.0,1)
      CALL STSETI('MAP -- Mapping Mode', 2)
      CALL STSETI('SET -- Do Set Call', 0)
      CALL STSETR('XC1 -- Lower X Bound', 1.0)
      CALL STSETR('XCM -- Upper X Bound', 20.0)
      CALL STSETR('YC1 -- Lower Y Bound', 0.0)
      CALL STSETR('YCN -- Upper Y Bound', 360.0)
C     CALL STSETI('TRT -- Transform Type', 1)
      DO 20 I = 1,M
         DO 10 J = 1,N
            A(I,J)=1.0
            B(I,J)=0.0
 10      CONTINUE
 20   CONTINUE
C
      CALL STINIT(A,M,B,M,IDM,IDM,M,N,WRK,2*M*N)
      CALL GSPLCI(7)
      CALL STREAM(A,B,IDM,IDM,IDM,WRK)
      CALL GSPLCI(2)
      DO 120 I = 1,M
         DO 110 J = 1,N
            A(I,J)=0.0
            B(I,J)=-1.0
 110     CONTINUE
 120  CONTINUE
      CALL STINIT(A,M,B,M,IDM,IDM,M,N,WRK,2*M*N)
      CALL STREAM(A,B,IDM,IDM,IDM,WRK)
      CALL FRAME
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END
