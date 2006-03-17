
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
      PARAMETER ( M=70 , N=150 , NPR=155)
      PARAMETER (PI=3.14159 , TWOPI=2.*PI , EPS=PI/6.)
      DIMENSION A(M,NPR),B(M,N)
      EQUIVALENCE (A(1,5),B(1,1))
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
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
      ZERO = CHAR(0)
C
C     Generate the input array.
C
      CALL GENARA(B,A,M,N)
C
C     Frame 1 -- three curves on same plot using AUTOGRAPH.
C
      CALL AGSETI('DASH/LENGTH.',16)
      CALL DISPLA(2,0,1)
      DO 160 J=1,3
      YT = 0.3*REAL(J)
      YB = 0.3*REAL(J-1)+.05
      CALL SET(.1,.9,YB,YT,0.,1.,0.,1.,1)
      CALL ANOTAT(ZERO,ZERO,3,2,1,LDASH)
      CALL EZY(A(1,3*J),60,' $')
  160 CONTINUE
      CALL FRAME
C
C     Frame 2 -- CONREC plot with polar coordinate transformation.
C
      ITRANS = 1
      D1 = 20.
      MA = 30
      NA = 70
      CALL GENARA(B,A,MA,NA+12)
      XMIN = -60.
      XMAX =  60.
      YMIN = -80.
      YMAX =  80.
      CALL SET(.1,.9,.1,.9,XMIN,XMAX,YMIN,YMAX,1)
      CALL LABMOD ('(F5.0)','(F5.0)',5,5,0,0,0,0,0)
      CALL PERIML(6,2,6,2)
      D2 = 30.
      A1 = EPS
      A2 = TWOPI-.5*A1
      DELX = 1.25*(D1+D2)
      DELY = 1.50*(D1+D2)
      CALL SET(.05,.95,.05,.95,-DELX,DELX,-DELY,DELY,1)
      CALL CONREC (A(1,4),MA,MA,NA,0.,0.,0.,1,-1,0)
      CALL FRAME
C
C     Frame 3 -- CONREC overlaid on EZMAP.
C
      MA = 55
      NA = 45
      CALL GENARA(B,A,MA,NA)
      ITRANS = 2
      CALL SUPMAP (2,40.,-90.,0.,0.,0.,0.,0.,1,20,4,0,IERS)
      ALNMN = -160.
      ALNMX = -20.
      ALTMN = 0.
      ALTMX = 60.
      CALL CONREC (A,MA,MA,NA,0.,0.,0.,1,0,-585)
      CALL FRAME
C
C     Frame 4 -- CONREC overlaid on EZMAP.
C
      MA = 55
      NA = 45
      CALL GENARA(B,A,MA,NA)
      ITRANS = 2
      CALL SUPMAP (3,45.,-100.,45.,50.,-130.,20.,-75.,2,10,3,1,IERS)
      ALNMN = -120.
      ALNMX = -75.
      ALTMN = 25.
      ALTMX = 50.
      CALL CONREC (A,MA,MA,NA,0.,0.,0.,1,-1,1)
      CALL FRAME
C
C     Frame 5 --  CONREC with modified background.
C
      ITRANS = 0
      CALL GENARA(B,A,40,40)
      CALL SET(.1,.9,.1,.9,-1.,1.,-2.,2.,1)
      CALL LABMOD ('(F3.0)','(F3.0)',3,3,0,0,0,0,0)
      CALL PERIML(2,10,4,5)
      CALL CONREC(A(1,1),40,40,40,-1.,1.,.5,-1,-1,-682)
      CALL FRAME
C
C     Frame 6 -- VELVCT overlaid on EZMAP.
C
      CALL GENARA(B,A,60,60)
      ITRANS = 2
      CALL SUPMAP (3,45.,-100.,45.,50.,-130.,20.,-75.,2,15,3,0,IERS)
      ALNMN = -120.
      ALNMX = -75.
      ALTMN = 25.
      ALTMX = 50.
      CALL VVSETI('MAP -- Mapping Mode', 1)
      CALL VVSETI('SET -- Do SET Call Flag', 0)
      CALL VVSETR('XC1 -- Lower X Bound', -120.0)
      CALL VVSETR('XCM -- Upper X Bound', -75.0)
      CALL VVSETR('YC1 -- Lower Y Bound', 25.0)
      CALL VVSETR('YCN -- Upper Y Bound', 50.0)
      MA = 25
      NA = 25
      IDM=0
      CALL VVINIT (A(1,10),60,A(1,25),60,IDM,IDM,MA,NA,IDM,IDM)
      CALL VVECTR (A(1,10),A(1,25),IDM,IDM,IDM,IDM)
      CALL FRAME
C
C     Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END

      SUBROUTINE GENARA (A,B,ID,JD)
C
C     This subroutine generates a smooth array in output array B.
C     The array is dependent on the random number function RANDNO.
C     RANDNO can be replaced by a random number generator on a
C     given machine, or for consistency across machines, the
C     supplied function RANDNO may be used.  RANDNO reads its random
C     numbers from the file RANFDAT.
C
      DIMENSION A(ID,JD), B(ID,JD)
C
      DO 1 I=1,ID
      DO 1 J=1,JD
    1 A(I,J)=0.
C
      NN=(ID+JD)/10
      AA=1.
      DI=ID-4
      DJ=JD-4
C
    2 DO 5 K=1,NN
      II=3.+DI*RANDNO()
      JJ=3.+DJ*RANDNO()
      DO 4 J=1,JD
      JE=ABS(J-JJ)
      DO 3 I=1,ID
      IE=ABS(I-II)
      EE=MAX(IE,JE)
      A(I,J)=A(I,J)+AA*(.8**EE)
    3 CONTINUE
    4 CONTINUE
    5 CONTINUE
C
      IF (AA.NE.1.) GO TO 6
      AA=-1.
      GO TO 2
C
    6 DO 8 J=1,JD
      JM1=MAX(1,J-1)
      JP1=MIN(JD,J+1)
      DO 7 I=1,ID
      IM1=MAX(1,I-1)
      IP1=MIN(ID,I+1)
      B(I,J)=(4.*A(I,J)+2.*(A(I,JM1)+A(IM1,J)+A(IP1,J)+A(I,JP1))
     1                 +A(IM1,JM1)+A(IP1,JM1)+A(IM1,JP1)+A(IP1,JP1))/16.
    7 CONTINUE
    8 CONTINUE
      RETURN
      END

       SUBROUTINE OPENR (IUNIT)
       CHARACTER*128 FILENM
       SAVE IOPEN
       DATA IOPEN / 0 /
       IF (IOPEN.EQ.0) THEN
           CALL GNGPAT (FILENM,"database",ISTATUS)
           IF (ISTATUS .NE. -1) THEN
               DO 101 I=1,120
                   IF (FILENM(I:I).EQ.CHAR(0)) THEN
                       FILENM(I:I+8)='/ranfdata'
                       GO TO 102
                   END IF
  101          CONTINUE
               GO TO 104
           ELSE 
               GO TO 103
           ENDIF
 102       OPEN (UNIT=IUNIT,FILE=FILENM,STATUS='OLD',FORM='FORMATTED',
     +                                                       ERR=104)
          IOPEN=1
      END IF
      RETURN
 103  WRITE (6,*) FILENM
      GO TO 105
 104  WRITE (6,*) 'ERROR IN OPENING FILE OF RANDOM NUMBERS: ',FILENM
 105  CONTINUE
      STOP
      END

      FUNCTION RANDNO()
C
C     This function is used to produce random numbers for the
C     GENARA calls.  The random numbers are read from file
C     RANFDAT, and in this way consistency is maintained across
C     machines (the results on one machine should be the same
C     as on another.)  If consistency is not required, the
C     function RANDNO can be replaced by a local random number
C     generator.  RANFDAT contains 2000 random numbers in
C     250 card-image lines in format 8F10.8 .
C
      SAVE ICNT,A
      DIMENSION A(2000)
      DATA ICNT/0/
C
      ICNT = ICNT+1
C
C     Read in random numbers if this is the first function call.
C
      IF (ICNT .EQ. 1) THEN
C Modification for UNIX Version.
	CALL OPENR(8)
C End of modification for UNIX Version.
        DO 3 I=1,250
        INDX = 8*(I-1)+1
        JNDX = INDX+7
        READ(8,501) (A(LL),LL=INDX,JNDX)
    3   CONTINUE
      END IF
C
      RANDNO = A(ICNT)
      RETURN
C
  500 FORMAT(' RANDNO--ERROR IN READ')
  501 FORMAT(8F10.8)
C
      END

      FUNCTION FX(X,Y)
C
      COMMON  /TRANS/  MA,NA,ITRANS,A1,A2,D1,D2,ALNMN,ALNMX,ALTMN,ALTMX
C
C     This function implements three transformations on the input
C     coordinate pair (X,Y) .  The coordinate (X,Y) is assumed to
C     be contained in the rectangle 1 to MA by 1 to NA.  The
C     transformation selected depends on the value of ITRANS:
C
C       ITRANS=0     The identity transformation is used.
C
C       ITRANS=1     Polar transformation.  The rectangular region
C                    bounded by the corner points (1,1), (MA,1),
C                    (MA,NA), (1,NA)  is transfomed onto the region
C                    in polar coordinates bounded by the polar
C                    coordinates (D1,A1), (D1,A2), (D2,A2), (D1,A2)
C                    where the angles A1 and A1 are expressed in radians.
C
C       ITRANS=2     Provides a linear mapping of the rectangular
C                    input array onto the region  ALNMN  to  ALNMX
C                    by  ALTMN  to  ALTMX.  The four parameters
C                    ALNMN, ALMNX, ALTMN, and ALTMX are assumed
C                    to specify two longitudes (ALNMN--minimum
C                    longitude; ALNMX--maximum longitude) and
C                    and two latitudes (ALTMN--minimum latitude;
C                    ALTMX--maximum latitude.)  The latitudes and
C                    longitudes in the transformed space are then
C                    projected onto the plot using the current
C                    EZMAP projection (hence EZMAP must be called
C                    prior to calling FX with ITRANS=2.)
C
      GOTO (10,20,30)  ITRANS+1
C
C     Identity transformation.
C
10    CONTINUE
      FX = X
      RETURN
C
C     Polar coordinate transformation.
C
20    CONTINUE
      FX=(D1+D2*(X-1.)/(REAL(MA)-1.))*COS(A1+A2*(Y-1.)/(REAL(NA)-1.))
      RETURN
C
C     EZMAP overlaying.
C
30    CONTINUE
      XLON = ALNMN + (X-1.)*(ALNMX-ALNMN)/(REAL(MA)-1.)
      YLAT = ALTMN + (Y-1.)*(ALTMX-ALTMN)/(REAL(NA)-1.)
      CALL MAPTRN (YLAT, XLON, FXLON, YDUM)
      FX = FXLON
      RETURN
      END
      
      FUNCTION FY(X,Y)
C
      COMMON  /TRANS/  MA,NA,ITRANS,A1,A2,D1,D2,ALNMN,ALNMX,ALTMN,ALTMX
C
C     The function FY behaves in an analagous manner to FX as
C     described above.
C
      GOTO (10,20,30)  ITRANS+1
C
C     The identity transformation.
C
10    CONTINUE
      FY = Y
      RETURN
C
C     Polar coordinate transformation.
C
20    CONTINUE
      FY=(D1+D2*(X-1.)/(REAL(MA)-1.))*SIN(A1+A2*(Y-1.)/(REAL(NA)-1.))
      RETURN
C
C     EZMAP overlaying.
C
30    CONTINUE
      XLON = ALNMN + (X-1.)*(ALNMX-ALNMN)/(REAL(MA)-1.)
      YLAT = ALTMN + (Y-1.)*(ALTMX-ALTMN)/(REAL(NA)-1.)
      CALL MAPTRN(YLAT,XLON,XDUM,FYLAT)
      FY = FYLAT
      RETURN
      END
