C	$Id: vvex02.f,v 1.1 1993-01-17 04:29:53 haley Exp $
C
      PROGRAM BNCHMK
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
      PARAMETER ( M=70 , N=70 , NPR=155)
      PARAMETER (PI=3.14159 , TWOPI=2.*PI , EPS=PI/6.)
      DIMENSION A(M,NPR),B(M,N),ZDAT(M,N)
C      EQUIVALENCE (A(1,5),B(1,1))
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
      ZERO = CHAR(0)
C
C     Generate the input array.
C
      CALL GENARA(A,B,M,N)
C
      CALL GENDAT (ZDAT,60,60,60,25,25,-1E+2, +1E+2)
C
      CALL DFCLRS
C
C     Frame 6 -- VELVCT overlaid on EZMAP.
C
      CALL GSLWSC(1.0)
c     
      do 800 i=1,14,1
         call vvseti('PAI -- parameter array index', i)
         call vvseti('CLR -- gks color index', i+1)
 800  continue
c
      call vvsetr('lwd -- vector linewidth', 2.25)
c      call vvsetr('amn -- arrow minimum size', 0.01)
      call vvseti('vpo -- vector position method', 0)
      call vvseti('set -- SET call flag', 0)
      call vvseti('map -- mapping flag', 1)
      call vvseti('cpm -- compatibility mode', 1)
c
      do 1000 i=1,10,1 
C
C Aternately color vectors based on magnitude or scalar array value
C
         ISGN=1
         IF (MOD(I,2) .EQ. 0) THEN
            ISGN=-1
         END IF
         CALL VVSETI('CTV -- COLOR THRESHOLDS VALUE', 14*ISGN)
C
C Do 10 different easy map projections
C
         IF (I .EQ. 3) THEN
            CALL SUPMAP (3,0.,80.,70.,90.,80.,0.,0.,2,20,4,0,IERS)
         ELSE
            CALL SUPMAP (I,0.,0.,0.,0.,0.,0.,0.,1,20,2,0,IERS)
         END IF
C
C Treat the data as as grid covering the full globe
C
         CALL VVSETR('XC1 -- LOWER X BOUND', -180.0)
         CALL VVSETR('XCM -- UPPER X BOUND', 180.0)
         CALL VVSETR('YC1 -- LOWER X BOUND', -90.0)
         CALL VVSETR('YCM -- UPPER Y BOUND', 90.0)
         MA = 25
         NA = 25
         IDM = 0
C         CALL VVSETI('PLR -- VECTOR POLAR FLAG', 2)
         CALL VVINIT (A(3,20),60,B,60,ZDAT,60,MA,NA,IDM,IDM)
         CALL VVGETI('NLV -- NUMBER OF LEVELS', NLV)
         CALL VVSETR('VFR -- VECTOR FRACTIONAL MINIMUM', 0.33)
         IF (I .GE. 5) THEN
            CALL VVGETR('VMN -- Minimum Vector', VMN)
            CALL  VVGETR('VMX -- Maximum Vector', VMX)
            CALL VVSETR('VLM -- Vector Low Magnitude',
     +           VMN+(VMX-VMN)/5.0)
            CALL VVGETR('VLM -- Vector Low Magnitude',VLM)
c            write(*,*) vlm,vmn,vmx
            CALL VVGETR('VFR -- Vector Fractioal Minimum',VFR)
            CALL VVGETR('DMX -- Distance of Max Vector',DMX)
            CALL VVSETR('VML -- Vector Max Length', DMX*2.0)
         END IF
C         write (*,*) 'nlv,vmn,vmx,vlm,vfr', nlv,vmn,vmx,vlm,vfr
C
         CALL VVECTR (A(3,20),B,ZDAT,IDM,IDM,IDM)
         CALL FRAME
 1000 CONTINUE
c
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END




