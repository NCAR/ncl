C
      FUNCTION MYF(X,Y,U,V,SFX,SFY,MX,MY)
C
C
C The mapping common block: made available to user mapping routines
C
      COMMON /VVMAP/
     +                IMAP       ,
     +                XVPL       ,XVPR       ,YVPB       ,YVPT       ,
     +                WXMN       ,WXMX       ,WYMN       ,WYMX       ,
     +                XLOV       ,XHIV       ,YLOV       ,YHIV       ,
     +                SXDC       ,SYDC       ,NXCT       ,NYCT       ,
     +                RLEN       ,LNLG       ,INVX       ,INVY       ,
     +                ITRT       ,IWCT       ,FW2W       ,FH2H       ,
     +                DVMN       ,DVMX       ,RBIG       ,IBIG
C
      SAVE /VVMAP/
C
C Math constants
C
      PARAMETER (PDTOR  = 0.017453292519943,
     +           PRTOD  = 57.2957795130823,
     +           P1XPI  = 3.14159265358979,
     +           P2XPI  = 6.28318530717959,
     +           P1D2PI = 1.57079632679489,
     +           P5D2PI = 7.85398163397448) 
C
C --------------------------------------------------------------------
C
      PARAMETER (PRCFAC=1E5,
     +           PVFRAC=0.001,
     +           PFOVFL=1E12,
     +           IPMXCT=10,
     +           PDUVML=2.0)
C
C
C     The function FY behaves in an analagous manner to FX as
C     described above.
C
      GOTO (10,20,30)  IMAP+1
C
C     The identity transformation.
C
10    CONTINUE
      MYF = MY + IFIX(SFY * V)
      RETURN
C
C     EZMAP overlaying.
C
20    CONTINUE
C
      UVLEN=SQRT(U**2+V**2)
C
C
C Check the vector magnitude
C
      IF (IFIX(UVLEN*PRCFAC) .EQ. 0) THEN
         MYF=IBIG
         RETURN
      END IF
C
      CLMT=PVFRAC/UVLEN
C
      XLON = XLOV + (X-1.)*(XHIV-XLOV)/(FLOAT(NXCT)-1.)
      YLAT = YLOV + (Y-1.)*(YHIV-YLOV)/(FLOAT(NYCT)-1.)
C
      CFCT=COS(YLAT*PDTOR)
      ICNT = 0
      ISGN = 1
C
      CALL MAPTRN(YLAT,XLON,X1,Y1)
C
 35   CONTINUE
      CALL MAPTRN(YLAT + ISGN*CLMT*V,XLON + ISGN*CLMT*U/CFCT,X2,Y2)

      VCLEN=SQRT((X2-X1)**2+(Y2-Y1)**2)
C
      IF (VCLEN .GT. RLEN) THEN
         IF (ISGN .EQ. -1) THEN
            MYF=IBIG
            RETURN
         ELSE
            ISGN=-1
            GO TO 35
         END IF
      ELSE IF (IFIX(VCLEN*PRCFAC) .EQ. 0) THEN
         IF (ICNT .LT. 10) THEN
            ICNT = ICNT + 1
            CLMT = CLMT * 2.0
            GO TO 35
         ELSE
            MYF = IBIG
            RETURN
         END IF
      END IF
C
      IF (ABS(Y2) .GE. PFOVFL) THEN
         MYF = IBIG
         RETURN
      END IF
C
      T=ISGN*((Y2-Y1)/VCLEN)*UVLEN
C
      MYF=MY+IFIX(SFY*T)
      RETURN
C
30    CONTINUE
C
C     Polar coordinate transformation.
C
      WRITE(*,*) 'Polar mapping not implemented in this version of MYF'
C
      RETURN
      END
