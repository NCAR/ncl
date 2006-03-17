
      PROGRAM TDEX03
C
C  Illustrate the use of the simplified entry points for Tdpack, 
C  TDEZ2D and TDEZ3D, by drawing a surface and an isosurface.
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
C  Declare arrays for the surface.
C
      PARAMETER (NSX=29, NSY=25)
      DIMENSION XS(NSX),YS(NSY),ZS(NSX,NSY)
C
C  Declare arrays for the isosurface.
C
      PARAMETER (NIX=21, NIY=31, NIZ=19)
      DIMENSION XI(NIX),YI(NIY),ZI(NIZ),UI(NIX,NIY,NIZ)
C
C  Define some constants used in the isosurface creation.
C
      DATA RBIG1,RBIG2,RSML1,RSML2/6.,6.,2.,2./
C
C  Open GKS, open workstation, activate workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Create the data array for the 2-dimensional surface.
C
      DO 40 I=1,NSX
        NSXH = NSX/2
        XS(I) = 0.1*REAL(I-NSXH-1)
        DO 50 J=1,NSY
          NSYH = NSY/2
          YS(J) = 0.1*REAL(J-NSYH-1)
          ZS(I,J) = XS(I) + YS(J)
          T1 = 1.0/((ABS(XS(I)-0.1))**2.75+ABS(YS(J))**2.75+0.09)
          T2 = 1.0/((ABS(XS(I)+0.1))**2.75+ABS(YS(J))**2.75+0.09)
          ZS(I,J) = 0.3*(ZS(I,J)+T1-T2)
   50   CONTINUE
   40 CONTINUE
C
C  Draw the surface.
C
      CALL TDEZ2D(NSX,NSY,XS,YS,ZS,2.5,-154.,80.,6)
      CALL FRAME
C
C  Create the data array for the isosurface.
C
      JCENT1 = REAL(NIY)*.5-RBIG1*.5
      JCENT2 = REAL(NIY)*.5+RBIG2*.5
      DO  30 I=1,NIX
        XI(I) = REAL(I)
        FIMID = I-NIX/2
        DO  20 J=1,NIY
          YI(J) = REAL(J)
          FJMID1 = J-JCENT1
          FJMID2 = J-JCENT2
          DO  10 K=1,NIZ
            ZI(K) = REAL(K)
            FKMID = K-NIZ/2
            F1 = SQRT(RBIG1*RBIG1/(FJMID1*FJMID1+FKMID*FKMID+.1))
            F2 = SQRT(RBIG2*RBIG2/(FIMID*FIMID+FJMID2*FJMID2+.1))
            FIP1 = (1.-F1)*FIMID
            FIP2 = (1.-F2)*FIMID
            FJP1 = (1.-F1)*FJMID1
            FJP2 = (1.-F2)*FJMID2
            FKP1 = (1.-F1)*FKMID
            FKP2 = (1.-F2)*FKMID
            UI(I,J,K) = MIN(FIMID*FIMID+FJP1*FJP1+FKP1*FKP1-RSML1*RSML1,
     +                 FKMID*FKMID+FIP2*FIP2+FJP2*FJP2-RSML2*RSML2)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C
C  Draw the isosurface.
C
      CALL TDEZ3D(NIX,NIY,NIZ,XI,YI,ZI,UI,0.,1.8,-45.,58.,-4)
      CALL FRAME
C
C  Deactivate and close workstation, close GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
