C
C       $Id: wmex02.f,v 1.8 1994-12-15 23:49:30 fred Exp $
C
      PROGRAM WMEX07
C
C  Examples of spline fits for weather fronts and regions.
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
C
      CHARACTER*28 LABEL
C
C  Color index for cross markers.
C
      DATA ICCLR/3/
C
C  Data for two lines and a region illustrating spline fits.
C
      PARAMETER (NS=3)
      DIMENSION XS(NS),YS(NS)
      DATA XS/ 0.10, 0.30, 0.50/ 
      DATA YS/ 0.45, 0.70, 0.75/ 
      PARAMETER (NT=5)
      DIMENSION XT(NT),YT(NT)
      DATA XT/ 0.15, 0.20, 0.50, 0.70, 0.85/
      DATA YT/ 0.05, 0.28, 0.53, 0.58, 0.75/
      PARAMETER (NU=8)
      DIMENSION XU(NU),YU(NU)
      DATA XU/ 0.35, 0.40, 0.60, 0.80, 0.85, 0.70, 0.50, 0.35/
      DATA YU/ 0.10, 0.30, 0.31, 0.43, 0.15, 0.10, 0.05, 0.10/
C
C  Data for picture two illustrating slope control at end points.
C
      PARAMETER (NV=5)
      DIMENSION XV(NV),YV(NV)
      DATA XV/ 0.10, 0.30, 0.50, 0.70, 0.90 /
      DATA YV/ 1.00, 1.08, 1.00, 0.95, 0.94 /
      
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define a color table.
C
      CALL GSCR(IWKID, 0, 1.0, 1.0, 1.0)
      CALL GSCR(IWKID, 1, 0.0, 0.0, 0.0)
      CALL GSCR(IWKID, 2, 1.0, 0.0, 0.0)
      CALL GSCR(IWKID, 3, 0.0, 0.0, 1.0)
C
C  First frame - illustrate spline fits of weather fronts and regions.
C
C
C  Plot title.
C
      CALL PLCHHQ(0.50,0.94,':F26:Spline fits for fronts and regions',
     +            0.03,0.,0.)
      CALL PLCHHQ(0.50,0.88,':F22: - marks the input coordinates',
     +            .025,0.,-.06)
      CALL PCGETR('XB',XB)
      CALL CROSS(XB-.015,0.875,ICCLR)
C
C  Define some parameters.
C
      CALL WMSETR('LIN - line widths of front lines',3.)
      CALL WMSETI('NMS - number of symbols on front line',0)
      CALL WMSETI('WFC - color for warm fronts',2)
C
      CALL WMDRFT(NS,XS,YS)
      DO 10 I=1,NS
        CALL CROSS(XS(I),YS(I),ICCLR)
   10 CONTINUE
      CALL WMDRFT(NT,XT,YT)
      DO 20 I=1,NT
        CALL CROSS(XT(I),YT(I),ICCLR)
   20 CONTINUE
      CALL WMDRRG(NU,XU,YU,'INDEX2',1,XU,YU)
      DO 30 I=1,NU
        CALL CROSS(XU(I),YU(I),ICCLR)
   30 CONTINUE
C
      CALL FRAME
C
C  Second frame - example of end point slope control.
C
      CALL PCSETI('CC',1)
      CALL PLCHHQ(0.50,0.96,':F26:Slope control at endpoints',
     +            0.03,0.,0.)
C
C  Set some parameter values.
C
      CALL WMSETI('NMS - number of symbols on front line',6)
      CALL WMSETR('SL1 - slope at start of front line if SLF=0 or 1',0.)       
      CALL WMSETR('SL2 - slope at end of front line if SLF=0 or 1',-15.)       
      CALL WMSETI('WFC - color for warm fronts',1)
      CALL WMSETR('LIN - line widths of front lines',3.)
      CALL WMSETI('REV - line widths of front lines',1)
      CALL WMSETI('WFC - color for warm fronts',2)
      CALL PCSETI('CC',3)
      DO 40 I=3,0,-1
        CALL WMSETI('SLF - flags whether slopes are from SL1 and SL2',I)      
        DO 50 J=1,NV
          YV(J) = YV(J)-0.22
   50   CONTINUE
        CALL WMDRFT(NV,XV,YV)
        WRITE(LABEL,500) I
  500   FORMAT(':F22:SLF=',I1,', SL1=0., SL2=-15.')  
        CALL PLCHHQ(.7,YV(1)+.08,LABEL,.024,0.,0.)
   40 CONTINUE
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
      STOP
C
      END
      SUBROUTINE CROSS(X,Y,ICLR)
C
C  Draw a green filled cross at (X,Y).
C
      PARAMETER(ID=16)
      PARAMETER(IX=15, IMX=100,IMXH=IMX/2)
      PARAMETER(IMXM=IMX-IX, IMXP=IMX+IX, IMXHM=IMXH-IX, IMXHP=IMXH+IX)
      DIMENSION ICX(ID),ICY(ID),RCX(ID),RCY(ID)
C
      DATA ICX( 1),ICX( 2),ICX( 3),ICX( 4)/    0,    IX,  IMXH, IMXM/
      DATA ICY( 1),ICY( 2),ICY( 3),ICY( 4)/    0,     0, IMXHM,    0/
      DATA ICX( 5),ICX( 6),ICX( 7),ICX( 8)/  IMX,   IMX, IMXHP,  IMX/
      DATA ICY( 5),ICY( 6),ICY( 7),ICY( 8)/    0,    IX,  IMXH, IMXM/
      DATA ICX( 9),ICX(10),ICX(11),ICX(12)/  IMX,  IMXM,  IMXH,   IX/
      DATA ICY( 9),ICY(10),ICY(11),ICY(12)/  IMX,   IMX, IMXHP,  IMX/
      DATA ICX(13),ICX(14),ICX(15),ICX(16)/    0,     0, IMXHM,    0/
      DATA ICY(13),ICY(14),ICY(15),ICY(16)/  IMX,  IMXM,  IMXH,   IX/
C
      DO 10 I=1,ID
        RCX(I) = X-0.00027*(REAL(IMXH)-REAL(ICX(I)))
        RCY(I) = Y-0.00027*(REAL(IMXH)-REAL(ICY(I)))
   10 CONTINUE
      CALL GSFAIS(1)
      CALL GQFACI(IOC)
      CALL GSFACI(ICLR)
      CALL GFA(ID,RCX,RCY)
      CALL GSFACI(IOC)
C
      RETURN
      END
