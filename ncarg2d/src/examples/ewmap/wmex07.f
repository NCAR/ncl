
      PROGRAM WMEX07
C
C  Plot icons for daily weather.
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
C  Positioning coordinates.
C
      PARAMETER (XL1=0.20, XL2=0.50, XL3=0.80, 
     +           Y1 =0.83, Y2 =0.62, Y3 =0.40, Y4 =0.20,
     +           YL1=0.74, YL2=0.52, YL3=0.32, YL4=0.07)
      PARAMETER (SIZLAB=.02)
C
C  Open GKS, open and activate a workstation.
C
      CALL GOPKS (IERRF, ISZDM)
      CALL GOPWK (IWKID, LUNIT, IWTYPE)
      CALL GACWK (IWKID)
C
C  Define a color table.
C
      CALL GSCR(1, 0, 1.00, 1.00, 1.00)
      CALL GSCR(1, 1, 0.00, 0.00, 0.00)
      CALL GSCR(1, 2, 0.00, 0.00, 1.00)
      CALL GSCR(1, 3, 0.00, 1.00, 0.00)
      CALL GSCR(1, 4, 1.00, 0.00, 0.00)
      CALL GSCR(1, 5, 1.00, 1.00, 0.00)
      CALL GSCR(1, 6, 1.00, 0.65, 0.00)
      CALL GSCR(1, 7, 0.85, 0.85, 0.85)
C
C  Plot title.
C
      CALL PLCHHQ(0.50,0.95,':F26:Icons for daily weather',0.033,0.,0.)       
C
C  Cloudy.
C
      CALL WMSETI('CC1 - cloud interior color ',7)
      CALL WMSETI('CC2 - cloud outline color',1)
      CALL WMSETI('CC3 - cloud shadow color',2)
      CALL WMSETR('SHT - size scale',0.013)
      CALL WMLABS(XL1,Y2-0.025,'C')
      CALL PLCHHQ(XL1,YL2,':F22:Cloudy',SIZLAB,0.,0.)       
C
C  Rain.
C
      CALL WMGETI('COL',ICOLD)
      CALL WMSETI('COL - rain color',1)
      CALL WMSETR('SHT - size scale',0.013)
      CALL WMLABS(XL2,Y2,'R')
      CALL WMSETI('COL - restore default color',ICOLD)
      CALL PLCHHQ(XL2,YL2,':F22:Rain',SIZLAB,0.,0.)       
C
C  T-storms.
C
      CALL WMSETI('LC1 - color of interior of lightening bolt ',5)
      CALL WMSETI('LC2 - outline color of lightening bolt',1)
      CALL WMSETI('LC3 - shadow color for lightening bolt',1)
      CALL WMSETR('SHT - size scale',0.013)
      CALL WMLABS(XL2,Y4,'T')
      CALL PLCHHQ(XL2+.02,YL4,':F22:T-storms',SIZLAB,0.,0.)       
C
C  Snow.
C
      CALL WMSETR('SHT - size scale',0.013)
      CALL WMLABS(XL3,Y2,'SN')
      CALL PLCHHQ(XL3,YL2,':F22:Snow',SIZLAB,0.,0.)       
C
C  Rain and snow.
C
      CALL WMSETR('SHT - size scale',0.013)
      CALL WMLABS(XL3,Y3+0.01,'RS')
      CALL PLCHHQ(XL3,YL3,':F22:Rain and snow',SIZLAB,0.,0.)       
C
C  Wind.
C
      CALL WMSETI('COL',2)
      CALL WMSETR('SHT - size scale',0.016)
      CALL GSLWSC(2.)
      CALL WMLABS(XL2,Y1,'WIND')
      CALL PLCHHQ(XL2,YL1,':F22:Windy',SIZLAB,0.,0.)       
      CALL WMSETI('COL',1)
      CALL GSLWSC(1.)
C
C  Mostly cloudy.
C
      CALL WMSETI('SC1 - color of the sun center',5)
      CALL WMSETI('SC2 - color of the sun star points',6)
      CALL WMSETI('SC3 - color of the sun outlines',1)
      CALL WMSETI('SC4 - sun shadow color',1)
      CALL WMSETR('SHT - size scale',0.013)
      CALL WMLABS(XL1,Y3,'MC')
      CALL PLCHHQ(XL1,YL3,':F22:Mostly cloudy',SIZLAB,0.,0.)       
C
C  Sunny.
C
      CALL WMSETR('SHT - size scale',0.013)
      CALL WMLABS(XL1,Y1,'SU')
      CALL PLCHHQ(XL1,YL1,':F22:Sunny',SIZLAB,0.,0.)       
C
C  Mostly sunny.
C
      CALL WMSETR('SHT - size scale',0.013)
      CALL WMLABS(XL2,Y3,'MS')
      CALL PLCHHQ(XL2,YL3,':F22:Mostly sunny',SIZLAB,0.,0.)       
C
C  Intermittent showers.
C
      CALL WMSETR('SHT - size scale',0.012)
      CALL WMLABS(XL3,Y4+.01,'IS')
      CALL PLCHHQ(XL3+0.02,YL4+0.017,':F22:Intermittent',SIZLAB,0.,0.)       
      CALL PLCHHQ(XL3+0.02,YL4-0.017,':F22:showers',SIZLAB,0.,0.)       
C
C  Sun, possible T-storms.
C
      CALL WMSETR('SHT - size scale',0.012)
      CALL WMLABS(XL1,Y4+.01,'IT')
      CALL PLCHHQ(XL1+0.02,YL4+0.017,':F22:Sun, possible',SIZLAB,0.,0.)       
      CALL PLCHHQ(XL1+0.02,YL4-0.017,':F22:T-storms',
     +            SIZLAB,0.,0.)       
C
C  Ice.
C
      CALL WMSETR('SHT - size scale',0.013)
      CALL WMLABS(XL3,Y1,'IC')
      CALL PLCHHQ(XL3,YL1,':F22:Ice',SIZLAB,0.,0.)       
C
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
      STOP
C
      END
