C
C	$Id: wmex03.f,v 1.1 1994-09-09 23:59:11 fred Exp $
C
      PROGRAM WMEX03
C
C  Example of windbarbs at various angles and a chart of wind speeds.
C
C  Define error file, Fortran unit number, and workstation type,
C  and workstation ID.
C
      PARAMETER (IERRF=6, LUNIT=2, IWTYPE=SED_WSTYPE, IWKID=1)
C
C  Example 01 - chart of windbarbs for various speeds.
C
      PARAMETER (T1=0.90, T2=0.84)
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
      CALL GSCR(IWKID,0,1.,1.,1.)
      CALL GSCR(IWKID,1,0.,0.,0.)
      CALL GSCR(IWKID,2,1.,0.,0.)
      CALL GSCR(IWKID,3,0.,0.,1.)
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
        CALL PLCHHQ(XL,0.87,'Sym.',0.022,0.,0.) 
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
C  Example 02 - windbarbs at various angles.
C
C  Draw direction arrows and lebels.
C
      CALL WMSETI('COL',3)
      CALL WMSETR('ARL',7.8)
      CALL WMSETR('ARD',0.)
      CALL WMSETR('ARS',.1)
      CALL WMLABS(0.9,0.4,'Arrow')
      CALL WMSETR('ARD',90.)
      CALL WMLABS(0.5,0.8,'Arrow')
      CALL PCSETI('CC',3)
      CALL PLCHHQ(0.5,0.83,':F22:N',.03,0.,0.)
      CALL PLCHHQ(0.93,0.4,':F22:E',.03,0.,0.)
C
C  Draw windbarbs.
C
      CALL WMSETI('COL',2)
      CALL WMSETR('WBS',0.3)
      CALL GSLWSC(3.)
      SCL = 65.
      DO 20 I=1,13
        ANG = 15.+(I-1)*30.*3.14159/180.
        U = COS(ANG)
        V = SIN(ANG)
        CALL WMBARB(0.5,0.4,SCL*U,SCL*V)
   20 CONTINUE
C
C  Main title.
C
      CALL PCSETI('CC',3)
      CALL PLCHHQ(0.5,0.93,':F25:Windbarbs',.04,0.,0.)
      CALL FRAME
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
C
      STOP
      END
