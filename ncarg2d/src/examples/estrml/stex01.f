C
      PROGRAM STEX01
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
C Example illustrating polar coordinates using Streamlines
C
      PARAMETER (M=20, N=36)
      DIMENSION A(M,N),B(M,N)
      DIMENSION WRK(2*M*N)
C
C     Open GKS, open workstation, activate workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C Define a GKS color table
C
      CALL DFCLRS(IWKID)
C
C Do the SET call, set the mapping mode and data coordinate boundaries
C appropriately for polar coordinate mapping
C
      CALL SET (0.05,0.95,0.05,0.95,-20.0,20.0,-20.0,20.0,1)
      CALL STSETI('MAP -- Mapping Mode', 2)
      CALL STSETI('SET -- Do Set Call', 0)
      CALL STSETR('XC1 -- Lower X Bound', 1.0)
      CALL STSETR('XCM -- Upper X Bound', 20.0)
      CALL STSETR('YC1 -- Lower Y Bound', 0.0)
      CALL STSETR('YCN -- Upper Y Bound', 360.0)
C     CALL STSETI('TRT -- Transform Type', 1)
C
C Set up a uniform field parallel to the radius
C
      DO 20 I = 1,M
         DO 10 J = 1,N
            A(I,J)=1.0
            B(I,J)=0.0
 10      CONTINUE
 20   CONTINUE
C
C Render the field using one color
C
      CALL GSPLCI(4)
      CALL STINIT(A,M,B,M,IDM,IDM,M,N,WRK,2*M*N)
      CALL STREAM(A,B,IDM,IDM,IDM,WRK)
C
C Set up a uniform field perpendicular to the radius
C
      DO 120 I = 1,M
         DO 110 J = 1,N
            A(I,J)=0.0
            B(I,J)=1.0
 110     CONTINUE
 120  CONTINUE
C
C Change the color and render the next field
C
      CALL GSPLCI(15)
      CALL STINIT(A,M,B,M,IDM,IDM,M,N,WRK,2*M*N)
      CALL STREAM(A,B,IDM,IDM,IDM,WRK)
      CALL FRAME
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
C
C ==============================================================
C
      SUBROUTINE DFCLRS(IWKID)
C
C Define a set of RGB color triples for colors 0 through 15.
C
      PARAMETER (NCLRS=16)
      DIMENSION RGBV(3,NCLRS)
C
C Define the RGB color triples needed below.
C
      DATA RGBV / 
     +     0.00 , 0.00 , 0.00 ,
     +     1.00 , 1.00 , 1.00 ,
     +     0.70 , 0.70 , 0.70 ,
     +     0.75 , 0.50 , 1.00 ,
     +     1.00 , 0.00 , 1.00 ,
     +     0.00 , 0.00 , 1.00 ,
     +     0.00 , 0.50 , 1.00 ,
     +     0.00 , 1.00 , 1.00 ,
     +     0.00 , 1.00 , 0.60 ,
     +     0.00 , 1.00 , 0.00 ,
     +     0.70 , 1.00 , 0.00 ,
     +     1.00 , 1.00 , 0.00 ,
     +     1.00 , 0.75 , 0.00 ,
     +     1.00 , 0.38 , 0.38 ,
     +     1.00 , 0.00 , 0.38 ,
     +     1.00 , 0.00 , 0.00 /
C
C Define 16 different color indices, for indices 0 through 15.  The
C color corresponding to index 0 is black and the color corresponding
C to index 1 is white.
C
      DO 101 I=1,NCLRS
         CALL GSCR (IWKID,I-1,RGBV(1,I),RGBV(2,I),RGBV(3,I))
 101  CONTINUE
C
C Done.
C
        RETURN
C
      END
C
