C
C	$Id: stex01.f,v 1.3 1993-02-20 00:46:23 dbrown Exp $
C
      PROGRAM STEX01
C
C     This program produces plots illustrating non-trivial
C     usage of the NCAR Utilities.  This program is an
C     exact copy of an implementation that runs on a UNIX
C     machine at NCAR.  To implement this program on another
C     machine only the following steps should be required:
C
C       1.)  Modify the OPEN to the EZMAP dataset that
C            appears in the main program.
C
C       2.)  Modify the OPEN to the file of random numbers
C            RANFDAT in subroutine RANDNO.
C
C     The utilities AUTOGRPH, VELVCT, CONREC, EZMAP, GRIDAL,
C     and DASHCHAR are required.  Since CONREC and VELVCT plots
C     are overlaid on EZMAP, the arithmetic statement functions
C     for FX and FY in these two utilities should be changed
C     to the statement:
C
C      EXTERNAL FX,FY
C
C     This will force the functions FX and FY supplied in this
C     package to be loaded.  The documentation for FX explains
C     the transformations provided.
C
      PARAMETER ( M=20 , N=36 , NPR=155)
      PARAMETER (PI=3.14159 , TWOPI=2.*PI , EPS=PI/6.)
      DIMENSION A(M,N),B(M,N)
      DIMENSION WRK(2*M*N)
      COMMON  /TRANS/  MA,NA,ITRANS,A1,A2,D1,D2,ALNMN,ALNMX,ALTMN,ALTMX
      CHARACTER*1  ZERO
      CHARACTER*16 LDASH(1)
      DATA  LDASH(1) /'$$''$$''$$''$$''$$''$'/
C
C Modification for UNIX Version.
C       OPEN statement removed.
C End of modification for UNIX Version.
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
      CALL STREAM(A,B,WRK,IDM,IDM)
      CALL GSPLCI(2)
      DO 120 I = 1,M
         DO 110 J = 1,N
            A(I,J)=0.0
            B(I,J)=-1.0
 110     CONTINUE
 120  CONTINUE
      CALL STINIT(A,M,B,M,IDM,IDM,M,N,WRK,2*M*N)
      CALL STREAM(A,B,WRK,IDM,IDM)
      CALL FRAME
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END
