C
C	$Id: ffex04.f,v 1.2 1994-07-08 17:44:20 haley Exp $
C
      PROGRAM FFEX04
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
C
      PARAMETER ( M=20 , N=36 , NPR=155)
      DIMENSION U(M,N),V(M,N)
      DIMENSION WRK(2*M*N)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL STSETR('DFM -- Differential Magnitude', 0.01)
C
      CALL STSETI('MAP -- Mapping Mode', 2)
      CALL STSETR('WDL -- Window Left', -20.0)
      CALL STSETR('WDR -- Window Right', 20.0)
      CALL STSETR('WDB -- Window Bottom', -20.0)
      CALL STSETR('WDT -- Window Top', 20.0)
      CALL STSETR('XC1 -- Lower X Bound', 1.0)
      CALL STSETR('XCM -- Upper X Bound', 20.0)
      CALL STSETR('YC1 -- Lower Y Bound', 0.0)
      CALL STSETR('YCN -- Upper Y Bound', 360.0)
C     
      DO 1000 K=1,0,-1
C
         CALL STSETI('TRT -- Transform Type', K)
C
         DO 20 I = 1,M
            DO 10 J = 1,N
               U(I,J)=1.0
               V(I,J)=0.0
 10         CONTINUE
 20      CONTINUE
C
         CALL STINIT(U,M,V,M,IDM,IDM,M,N,WRK,2*M*N)
         CALL STREAM(U,V,IDM,IDM,IDM,WRK)
C
         DO 120 I = 1,M
            DO 110 J = 1,N
               U(I,J)=0.0
               V(I,J)=1.0
 110        CONTINUE
 120     CONTINUE
C
         CALL STINIT(U,M,V,M,IDM,IDM,M,N,WRK,2*M*N)
         CALL STREAM(U,V,IDM,IDM,IDM,WRK)
C
         CALL PERIM(1,0,1,0)
         CALL FRAME
C
 1000 CONTINUE
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
