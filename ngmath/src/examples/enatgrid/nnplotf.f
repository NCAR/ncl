
      SUBROUTINE DRWCON(NX,NY,XI,YI,ZDAT)
C
C  Use the NCAR Graphics CONPACK package to draw a color contour 
C  plot of the data in ZDAT.
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
      DIMENSION ZDAT(NX,NY)
      DIMENSION RWRK(2000),IWRK(1000),IAMA(20000)
      DIMENSION XCRA(1000),YCRA(1000),IAIA(10),IGIA(10)
C
      EXTERNAL CPCOLR,CPDRPL
C
C  Open GKS if not open; open and activate a workstation; define
C  some colors.
C
      JTYPE = IWTYPE
      CALL GQOPS(ISTATE)
      IF (ISTATE .EQ. 0) THEN
        CALL GOPKS (IERRF, ISZDM)
        IF (JTYPE .EQ. 1) THEN
          CALL NGSETC('ME','con.ncgm')
        ELSE IF ( (JTYPE .GE. 20) .AND. (JTYPE .LE. 31) ) THEN
          CALL NGSETC('ME','con.ps')
        ENDIF
        CALL GOPWK (IWKID, LUNIT, JTYPE)
        CALL GACWK (IWKID)
        CALL GSCR(IWKID, 0, 1.00, 1.00, 1.00)
        CALL GSCR(IWKID, 1, 0.00, 0.00, 0.00)
        CALL GSCR(IWKID, 2, 0.00, 1.00, 1.00)
        CALL GSCR(IWKID, 3, 0.00, 1.00, 0.00)
        CALL GSCR(IWKID, 4, 0.70, 1.00, 0.00)
        CALL GSCR(IWKID, 5, 1.00, 1.00, 0.00)
        CALL GSCR(IWKID, 6, 1.00, 0.75, 0.00)
        CALL GSCR(IWKID, 7, 1.00, 0.50, 0.50)
        CALL GSCR(IWKID, 8, 1.00, 0.00, 0.00)
      ENDIF
C
      IERR = 0
C
      CALL CPSETI('CLS - CONTOUR LEVEL SELECTOR',0)
      CALL CPSETI('NCL - NUMBER OF CONTOUR LEVELS',7)
C
      DO 103 I=1,7
        CALL CPSETI('PAI - parameter array index',I)
        CALL CPSETR('CLV - contour level',10.*REAL(I))
        CALL CPSETI('CLU - contour level use',3)
        CALL CPSETI('LLC - contour label color',1)
  103 CONTINUE
C
C Initialize the drawing of the contour plot.
C
      CALL CPSETR('VPL - viewport left',0.05)
      CALL CPSETR('VPR - viewport right',0.95)
      CALL CPSETR('VPB - viewport bottom',0.05)
      CALL CPSETR('VPT - viewport top',0.95)
      CALL PCSETI('FN  - font number (Helvetica bold)' ,22)
      CALL PCSETI('CC  - font color',1)
      CALL CPSETR('T2D - tension of 2D splines',4.)
      CALL CPSETI('LLP - line label positioning, penalty scheme',3)
      CALL CPSETI('LLO - line label orientation',1)
      CALL CPSETC('LOT - low labels off',' ')
      CALL CPSETR('CWM - character width multiplier',2.5)
      CALL CPSETC('ILT - informational label off',' ')
      CALL CPRECT(ZDAT,NX,NX,NY,RWRK,2000,IWRK,1000)
C
C Initialize the area map and put the contour lines into it.
C
      CALL ARINAM (IAMA,20000)
      CALL CPCLAM (ZDAT,RWRK,IWRK,IAMA)
      CALL CPLBAM (ZDAT,RWRK,IWRK,IAMA)
C
C Color the map.
C
      CALL ARSCAM (IAMA,XCRA,YCRA,1000,IAIA,IGIA,7,CPCOLR)
C
C Put black contour lines over the colored map.
C
      CALL GSPLCI (1)
      CALL CPCLDM (ZDAT,RWRK,IWRK,IAMA,CPDRPL)
      CALL CPLBDR (ZDAT,RWRK,IWRK)
      CALL PERIM(1,0,1,0)
C
      CALL FRAME
C
C  Close down GKS.
C
      IF (ISTATE .EQ. 0) THEN
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
      ENDIF
C
      RETURN
      END
      SUBROUTINE CPCOLR (XCRA,YCRA,NCRA,IAIA,IGIA,NAIA)
C
      DIMENSION XCRA(*),YCRA(*),IAIA(*),IGIA(*)
C
      DO 102 I=1,NAIA
        IF (IGIA(I) .EQ. 3) IFLL = IAIA(I)
  102 CONTINUE
      IF (IFLL.GE.1 .AND. IFLL.LE.8) THEN
        CALL GSFACI (IFLL+1)
        CALL GFA (NCRA-1,XCRA,YCRA)
      END IF
C
      RETURN
      END
      SUBROUTINE DRWSRF(NX,NY,X,Y,Z,S1,S2,S3,IWK)
C
C  Procedure DRWSRF uses the NCAR Graphics function SRFACE to
C  draw a surface plot of the data values in Z.
C 
C  The point of observation is calculated from the 3D coordinate
C  (S1, S2, S3); the point looked at is the center of the surface.
C 
C   NX     -  Dimension of the X-axis variable X.
C   NY     -  Dimension of the Y-axis variable Y.
C   X      -  An array of X-axis values.
C   Y      -  An array of Y-axis values.
C   Z      -  An array dimensioned for NX x NY containing data
C             values for each (X,Y) coordinate.
C   S1     -  X value for the eye position.
C   S2     -  Y value for the eye position.
C   S3     -  Z value for the eye position.
C   IWK    -  Work space dimensioned for at least 2*NX*NY.
C 
C  
      DIMENSION X(NX),Y(NY),Z(NX,NY),IWK(*)
C
      PARAMETER (IERRF=6, LUNIT=2, IWKID=1, IWTYPE=8)
      DIMENSION S(6)
C
C  Open GKS, open and activate a workstation.
C
      JTYPE = IWTYPE
      CALL GQOPS(ISTATE)
      IF (ISTATE .EQ. 0) THEN
        CALL GOPKS (IERRF, ISZDM)
        IF (JTYPE .EQ. 1) THEN
          CALL NGSETC('ME','srf.ncgm')
        ELSE IF ( (JTYPE .GE. 20) .AND. (JTYPE .LE. 31) ) THEN
          CALL NGSETC('ME','srf.ps')
        ENDIF
        CALL GOPWK (IWKID, LUNIT, JTYPE)
        CALL GSCR(IWKID,0,1.,1.,1.)
        CALL GSCR(IWKID,1,0.,0.,0.)
        CALL GACWK (IWKID)
      ENDIF
C
C  Find the extreme values.
C
      XMN =  X(1)
      XMX =  X(1)
      YMN =  Y(1)
      YMX =  Y(1)
      ZMN =  Z(1,1)
      ZMX =  Z(1,1)
C
      DO 10 I=2,NX
        XMN = MIN(XMN,X(I))
        XMX = MAX(XMX,X(I))
   10 CONTINUE
C
      DO 11 I=1,NY
        YMN = MIN(YMN,Y(I))
        YMX = MAX(YMX,Y(I))
   11 CONTINUE
C
      DO 12 I=1,NX
        DO 13 J=1,NY
          ZMN = MIN(ZMN,Z(I,J))
          ZMX = MAX(ZMX,Z(I,J))
   13   CONTINUE
   12 CONTINUE
C
      IF (S1.EQ.0. .AND. S2.EQ.0. .AND. S3.EQ.0.) THEN
        ST1 = -3.
        ST2 = -1.5
        ST3 = 0.75
      ELSE
        ST1 = S1
        ST2 = S2
        ST3 = S3
      ENDIF
      S(1) = 5.*ST1*(XMX-XMN)
      S(2) = 5.*ST2*(YMX-YMN)
      S(3) = 5.*ST3*(ZMX-ZMN)
      S(4) = 0.5*(XMX-XMN)
      S(5) = 0.5*(YMX-YMN)
      S(6) = 0.5*(ZMX-ZMN)
C
      CALL SRFACE (X,Y,Z,IWK,NX,NX,NY,S,0.)
C
C  Close down GKS.
C
      IF (ISTATE .EQ. 0) THEN
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
      ENDIF
C
      RETURN
      END
      SUBROUTINE DRWVCT(LX,LY,U,V)
C
C  Where U and V are 2D arrays, this subroutine uses NCAR Graphics to
C  draw a vector plot of the vectors (U(I,J),V(I,J)) 
C  for I=1,LX and J=1,LY.
C
      DIMENSION U(LX,LY),V(LX,LY)
      PARAMETER (IERRF=6, LUNIT=2, IWKID=1, IWTYPE=8)
C
      JTYPE = IWTYPE
      CALL GQOPS(ISTATE)
      IF (ISTATE .EQ. 0) THEN
        CALL GOPKS (IERRF, ISZDM)
        IF (JTYPE .EQ. 1) THEN
          CALL NGSETC('ME','vec.ncgm')
        ELSE IF ( (JTYPE .GE. 20) .AND. (JTYPE .LE. 31) ) THEN
          CALL NGSETC('ME','vec.ps')
        ENDIF
        CALL GOPWK (IWKID, LUNIT, JTYPE)
        CALL GACWK (IWKID)
        CALL GSCR(IWKID, 0, 1.00, 1.00, 1.00)
        CALL GSCR(IWKID, 1, 0.00, 0.00, 0.00)
      ENDIF
C
      CALL VVINIT(U,LX,V,LY,PDUM,1,LX,LY,WRK,1)
      CALL VVSETC('MNT',' ')
      CALL VVSETC('MXT',' ')
      CALL VVECTR(U,V,P,IAM,VVMSKD,WRK)
      CALL FRAME
C
      IF (ISTATE .EQ. 0) THEN
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
      ENDIF
C
      RETURN
      END
