C
C	$Id: stex01.f,v 1.1 1993-01-17 04:30:41 haley Exp $
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
c$$$      CALL VVSETI('map -- mapping mode', 1)
c$$$      CALL VVSETI('set -- do set call', 0)
c$$$      CALL VVSETR('XC1 -- LOWER X BOUND', -120.0)
c$$$      CALL VVSETR('XCM -- UPPER X BOUND', -75.0)
c$$$      CALL VVSETR('YC1 -- LOWER Y BOUND', 25.0)
c$$$      CALL VVSETR('YCM -- UPPER Y BOUND', 50.0)
c$$$      CALL VVSETR('VFR - Vector fractional floor', 0.75)
c$$$      call vvseti('cpm - compatibility mode', -4)
c$$$      CALL GSPLCI(3)
c$$$      CALL VELVCT (A(1,10),60,A(1,25),60,MA,NA,0.,0.,1,0,0,0.)
c
      CALL SET (0.05,0.95,0.05,0.95,-20.0,20.0,-20.0,20.0,1)
      CALL STSETI('map -- mapping mode', 2)
      CALL STSETI('set -- do set call', 0)
      CALL STSETR('XC1 -- LOWER X BOUND', 1.0)
      CALL STSETR('XCM -- UPPER X BOUND', 20.0)
      CALL STSETR('YC1 -- LOWER Y BOUND', 0.0)
      CALL STSETR('YCM -- UPPER Y BOUND', 360.0)
c     CALL STSETI('TRT -- TRANSFORM TYPE', 1)
      do 20 i = 1,M
         do 10 j = 1,N
            a(i,j)=1.0
            b(i,j)=0.0
 10      continue
 20   continue
c      call stsetr('ssp - stream spacing', 0.05)
c      call stseti('ckp - check progress', 50)
c      call stseti('ckx - check crossover', 3)
      CALL STINIT(A,M,B,M,idm,idm,M,N,wrk,2*m*n)
      CALL GSPLCI(7)
      CALL STREAM(A,B,WRK,idm,idm)
      CALL GSPLCI(2)
      do 120 i = 1,M
         do 110 j = 1,N
            a(i,j)=0.0
            b(i,j)=-1.0
 110     continue
 120  continue
      CALL STINIT(A,M,B,M,idm,idm,M,N,wrk,2*m*n)
      CALL STREAM(A,B,WRK,idm,idm)
      CALL FRAME
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END
