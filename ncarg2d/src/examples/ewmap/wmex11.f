
      PROGRAM WMEX11
C
C  Example of station model data.
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

      PARAMETER (X1=0.22, X2=0.67)
      CHARACTER*5 IMDAT(10)
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,.4,0.,.4)
C
C  Symbolic station model
C
      CALL WMSETR('WBS',0.20)
      CALL GSLWSC(3.)
      CALL EXSTNM(X2,.73)
      CALL LINE(0.1,.5,0.9,.5)
      CALL PLOTIT(0,0,0)
      SIZ = 0.03
      CALL PCSETI('FN',26)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(X1,.85,'SYMBOLIC',SIZ,0.,0.)
      CALL PLCHHQ(X1,.75,'STATION',SIZ,0.,0.)
      CALL PLCHHQ(X1,.65,'MODEL',SIZ,0.,0.)
      CALL PCSETI('CC',1)
C
C  Sample plotted report
C
      IMDAT(1) = '11212'
      IMDAT(2) = '83320'
      IMDAT(3) = '10011'
      IMDAT(4) = '20000'
      IMDAT(5) = '30000'
      IMDAT(6) = '40147'
      IMDAT(7) = '52028'
      IMDAT(8) = '60111'
      IMDAT(9) = '77060'
      IMDAT(10) = '86792'
      CALL WMSTNM(X2,.23,IMDAT)
      CALL PCSETI('FN',26)
      CALL PCSETI('CC',2)
      CALL PLCHHQ(X1,.35,'SAMPLE',SIZ,0.,0.)
      CALL PLCHHQ(X1,.25,'PLOTTED',SIZ,0.,0.)
      CALL PLCHHQ(X1,.15,'DATA',SIZ,0.,0.)
      CALL PCSETI('CC',1)
C
      CALL FRAME
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
      SUBROUTINE EXSTNM(X,Y)
C
C  Convert X and Y to NDC and work in NDC space.
C
      CALL WMW2NX(1,X,XNDC)
      CALL WMW2NY(1,Y,YNDC)
C
C  Draw a wind barb at 320 degrees and cloud cover symbol.
C
      CALL WMSETI('WBF',1)
      CALL WMBARB(XNDC,YNDC,-5.,8.66)
      CALL WMGETR('WBC',WBC)
      CALL WMGETR('WBS',WBSHFT)
      CALL NGWSYM('N',0,XNDC,YNDC,WBC*WBSHFT,1,0)
C
      SIZ = 0.15*WBSHFT
      CALL PCSETI('FN',21)
      CALL PLCHHQ(XNDC,YNDC,'N',SIZ,0.,0.)
C
C  Direction
C
      CALL WMSETI('RBS',0)
      CALL WMSETR('RMG',.030*WBSHFT)
      CALL WMSETR('THT',0.8*SIZ)
      CALL WMLABT(XNDC-0.7*WBSHFT*0.5,YNDC+0.7*WBSHFT*0.866,'dd',0)
C
C  Wind speed
C
      CALL WMLABT(XNDC-0.92*WBSHFT*0.5,YNDC+0.7*WBSHFT*1.5,'ff',0)
C
C  High clouds (CH).
C
      CALL PLCHHQ(XNDC,YNDC+0.83*WBSHFT,'C:B1:H',SIZ,0.,0.)
C
C  Medium clouds (CM).
C
      CALL PLCHHQ(XNDC,YNDC+0.47*WBSHFT,'C:B1:M',SIZ,0.,0.)
C
C  Current temperature (TT).
C
      CALL PLCHHQ(XNDC-0.7*WBSHFT,YNDC+0.36*WBSHFT,'TT',SIZ,0.,0.)
C
C  Barometric pressure (ppp).
C
      CALL PLCHHQ(XNDC+0.55*WBSHFT,YNDC+0.36*WBSHFT,'ppp',SIZ,0.,0.)
C
C  Visibility (VV).
C
      CALL PLCHHQ(XNDC-.95*WBSHFT,YNDC,'VV',SIZ,0.,0.)
C
C  Present weather (ww).
C
      CALL PLCHHQ(XNDC-0.45*WBSHFT,YNDC,'ww',SIZ,0.,0.)
C
C  Pressure change (pp).
C
      CALL PLCHHQ(XNDC+0.5*WBSHFT,YNDC,'pp',SIZ,0.,0.)
C
C  Pressure tendency (a).
C
      CALL PLCHHQ(XNDC+WBSHFT,YNDC,'a',SIZ,0.,0.)
C
C  Temperature of dewpoint (TD).
C
      CALL PLCHHQ(XNDC-0.65*WBSHFT,YNDC-0.42*WBSHFT,'T:B1:d',SIZ,0.,0.)
C
C  Low clouds (CL).
C
      CALL PLCHHQ(XNDC-0.17*WBSHFT,YNDC-0.42*WBSHFT,'C:B1:L',SIZ,0.,0.)
C
C  Sky cover (NH).
C
      CALL PLCHHQ(XNDC+0.31*WBSHFT,YNDC-0.42*WBSHFT,'N:B1:h',SIZ,0.,0.)
C
C  Past weather (W).
C
      CALL PLCHHQ(XNDC+0.75*WBSHFT,YNDC-0.42*WBSHFT,'W',SIZ,0.,0.)
C
C  Cloud height (h).
C
      CALL PLCHHQ(XNDC-0.12*WBSHFT,YNDC-0.72*WBSHFT,'h',SIZ,0.,0.)
C
C  Precipitation in last 6 hours (RR).
C
      CALL PLCHHQ(XNDC+0.53*WBSHFT,YNDC-0.72*WBSHFT,'RR',SIZ,0.,0.)
C
      END
