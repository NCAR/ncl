C
C	$Id: wmex08.f,v 1.1 1994-12-15 23:49:41 fred Exp $
C
      PROGRAM WMEX03
C
C  Example of wind barbs at various angles and a chart of wind speeds.
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
      PARAMETER (T1=0.90, T2=0.84)
      PARAMETER (X1=0.18, X2=0.46, X3=0.74)       
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,.4,0.,.4)
C
C  Example 01 - chart of wind barbs for various speeds.
C
      CALL PERIM(1,1,1,1)
      CALL LINE(0.000, T1+0.005, 1.000, T1+0.005)
      CALL LINE(0.000, T1-0.005, 1.000, T1-0.005)
      CALL LINE(0.000, T2, 1.000, T2)
      CALL LINE(0.495, 0.000, 0.495, T1-0.005)
      CALL LINE(0.505, 0.000, 0.505, T1-0.005)
C
      CALL PLCHHQ(0.5,0.955,':F25:Wind Speeds',.03,0.,0.)
      CALL PCSETI('FN',21)
      XCL = 0.12
      XCC = 0.26
      XCR = 0.40
      DO 10 I=1,2
        XL = XCL+(I-1)*0.5
        XC = XCC+(I-1)*0.5
        XR = XCR+(I-1)*0.5
        CALL PLCHHQ(XL,0.87,'Symbol',0.022,0.,0.) 
        CALL PLCHHQ(XC,0.87,'Knots',0.022,0.,0.) 
        CALL PLCHHQ(XR,0.87,'Miles/hr.',0.022,0.,0.) 
   10 CONTINUE
      FINC = T2/10.
      SIZE = 0.022
      XCL = 0.16
      CALL GSLWSC(3.)
      CALL NGSETI('WO',1)
      CALL NGSETI('CA',0)
C
      P1 = T2-0.75*FINC
      CALL WMSETR('WBS',0.1)
      CALL WMSETI('COL',1)
      CALL WMGETR('WBS',WSLEN)
      CALL WMBARB(XCL-0.5*WSLEN,P1-0.5*SIZE,0.,0.)
      CALL PLCHHQ(XCC,P1,'Calm',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'Calm',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-1.,0.)
      CALL PLCHHQ(XCC,P1,'1-2',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'1-2',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-5.,0.)
      CALL PLCHHQ(XCC,P1,'3-7',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'3-8',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-10.,0.)
      CALL PLCHHQ(XCC,P1,'8-12',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'9-14',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-15.,0.)
      CALL PLCHHQ(XCC,P1,'13-17',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'15-20',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-20.,0.)
      CALL PLCHHQ(XCC,P1,'18-22',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'21-25',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-25.,0.)
      CALL PLCHHQ(XCC,P1,'23-27',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'26-31',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-30.,0.)
      CALL PLCHHQ(XCC,P1,'28-32',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'32-37',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-35.,0.)
      CALL PLCHHQ(XCC,P1,'33-37',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'38-43',SIZE,0.,0.)
C
      XCL = XCL+0.5
      XCC = XCC+0.5
      XCR = XCR+0.5
      P1 = T2-0.75*FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-40.,0.)
      CALL PLCHHQ(XCC,P1,'38-42',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'44-49',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-45.,0.)
      CALL PLCHHQ(XCC,P1,'43-47',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'50-54',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-50.,0.)
      CALL PLCHHQ(XCC,P1,'48-52',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'55-60',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-55.,0.)
      CALL PLCHHQ(XCC,P1,'53-57',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'61-66',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-60.,0.)
      CALL PLCHHQ(XCC,P1,'58-62',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'67-71',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-65.,0.)
      CALL PLCHHQ(XCC,P1,'63-67',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'72-77',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-70.,0.)
      CALL PLCHHQ(XCC,P1,'68-72',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'78-83',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-75.,0.)
      CALL PLCHHQ(XCC,P1,'73-77',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'84-89',SIZE,0.,0.)
C
      P1 = P1-FINC
      CALL WMBARB(XCL,P1-0.5*SIZE,-105.,0.)
      CALL PLCHHQ(XCC,P1,'103-107',SIZE,0.,0.)
      CALL PLCHHQ(XCR,P1,'119-123',SIZE,0.,0.)
C
      CALL FRAME
C
C  Example 02 - wind barbs at various angles using differing attributes.
C
      CALL PLCHHQ(0.5,0.94,':F26:Wind barb examples',.04,0.,0.)
C
C  Set barb color and size.
C
      CALL WMSETI('COL',2)
      CALL WMSETR('WBS',0.25)
C
C  Draw first barb (all defaults with a wind speed of 71 knots).
C
      CALL WMBARB(X1,.5,26.,71.)
C
C  Second barb - change the angle of the tick marks and the spacing 
C  between ticks and draw another barb.
C
      CALL WMGETR('WBA',WBAO)
      CALL WMGETR('WBD',WBDO)
      CALL WMSETR('WBA',42.)
      CALL WMSETR('WBD',.17)
      CALL WMBARB(X2,.5,0.,75.)
      CALL WMSETR('WBA',WBAO)
      CALL WMSETR('WBD',WBDO)
C
C  Third barb - draw a sky cover symbol at base of the barb (these 
C  are drawn automatically when using WMSTNM to plot station model data).
C
      CALL WMGETR('WBS',WBS)
      CALL WMGETR('WBC',WBC)
      CALL WMSETI('WBF',1)
      CALL WMBARB(X3,.5,-26.,71.)
      CALL NGWSYM('N',0,X3,.5,WBS*WBC,2,0)
C
C  Fourth barb - change the size of the barb and the size of the sky 
C  cover symbol.
C 
      CALL WMSETR('WBS',0.20)
      CALL WMSETR('WBC',0.15)
      CALL WMBARB(X1+0.1,0.1,-26.,71.)
      CALL NGWSYM('N',0,X1+0.1,.1,0.20*0.15,2,0)
C
C  Fifth barb - reset original values for parameters, change wind speed
C               to 45 knots.
C
      CALL WMSETR('WBS',WBS)
      CALL WMSETR('WBC',WBC)
      CALL WMSETI('WBF',0)
      CALL WMBARB(X2,.1,0.,45.)
C
C  Sixth barb - change the tick lengths.
C
      CALL WMSETR('WBT',.6)
      CALL WMBARB(X3-0.1,.1,15.4,42.2)
C
      CALL FRAME
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
