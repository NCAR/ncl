
      PROGRAM WMEX06
C
C  Examples of regional weather and temperature.
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
      CALL GSCR(IWKID, 4, 0.0, 1.0, 0.0)
      CALL GSCR(IWKID, 5, 0.9, 0.9, 0.9)
      CALL GSCR(IWKID, 6, 1.0, 1.0, 0.0)
C
      CALL PLCHHQ(0.50,0.94,':F26:Symbols and labels',0.033,0.,0.)       
C
C  HIs and LOWs
C
      YY = 0.83
      YINC = 0.17
      XINC = 0.12
      CALL PCSETC('FC','%')
      CALL PLCHHQ(0.05,YY,'%F26%HIs and LOWs:',0.025,0.,-1.)       
      CALL PCSETC('FC',':')
      CALL PCGETR('XE',XE)
      XE = XE+0.5*XINC
C
      CALL WMSETI('HIS - shadow color for high symbols',1)
      CALL WMSETI('HIB - background color for high symbols',0)
      CALL WMSETI('HIF - character color for high symbols',1)
      CALL WMSETI('HIC - character color for circumscribed circle',1)
      CALL WMLABS(XE,YY,'HI')
      XE = XE+XINC
C
      CALL WMSETR('SHT',0.04)
      CALL WMSETI('HIS - shadow color for high symbols',3)
      CALL WMSETI('HIB - background color for high symbols',5)
      CALL WMSETI('HIF - character color for high symbols',2)
      CALL WMSETI('HIC - character color for circumscribed circle',4)
      CALL WMLABS(XE,YY,'HI')
      XE = XE+XINC
C
      CALL WMSETR('SHT',0.02)
      CALL WMSETI('LOS - shadow color for low symbols',5)
      CALL WMSETI('LOB - background color for low symbols',1)
      CALL WMSETI('LOF - character color for low symbols',0)
      CALL WMLABS(XE,YY,'LOW')
      XE = XE+XINC
C
      CALL WMSETR('SHT',0.04)
      CALL WMSETI('LOS - shadow color for low symbols',2)
      CALL WMSETI('LOB - background color for low symbols',5)
      CALL WMSETI('LOF - character color for low symbols',3)
      CALL WMLABS(XE,YY,'LOW')
C
C  Arrows.
C
      YY = YY-YINC
      CALL PCSETC('FC - Function code character','%')
      CALL PLCHHQ(0.05,YY,'%F26%Arrows:',0.025,0.,-1.)       
      CALL PCSETC('FC',':')
      CALL PCGETR('XE - coordinate of end of last string',XE)
      XE = XE+0.05
      CALL WMSETR('ARD - arrow direction',270.)
      CALL WMLABS(XE,YY-0.02,'Arrow')
C
      XE = XE+0.1
      CALL WMSETR('ARS - arrow size',.1)
      CALL WMSETR('ARD - arrow direction',55.)
      CALL WMSETR('ARL - scale factor for length of arrow tail',1.)
      CALL WMSETI('AWC - color index for interior of arrow',2)
      CALL WMLABS(XE,YY+0.05,'Arrow')
C
      XE = XE+0.04
      CALL GSLWSC(2.)
      CALL WMSETR('ARS - arrow size',.2)
      CALL WMSETR('ARD - arrow direction',180.)
      CALL WMSETR('ARL - scale factor for length of arrow tail',0.6)
      CALL WMSETI('AWC - color index for interior of arrow',0)
      CALL WMSETI('AOC - color index for arrow outline',3)
      CALL WMLABS(XE,YY,'Arrow')
C
      XE = XE+0.18
      CALL WMSETR('ARD - arrow direction',270.)
      CALL WMSETI('AWC - color index for interior of arrow',2)
      CALL GSLWSC(2.)
      CALL WMLABS(XE,YY-0.05,'Arrow')
      CALL GSLWSC(1.)
C
      XE = XE+0.18
      CALL WMSETR('ARD - arrow direction',0.)
      CALL WMSETI('AWC - color index for interior of arrow',5)
      CALL WMSETI('AOC - color index for arrow outline',2)
      CALL WMSETI('ASC - color index for arrow shadow',3)
      CALL WMLABS(XE,YY,'Arrow')
C
      XE = XE+0.03
      CALL WMDFLT()
      CALL WMSETR('ARS - arrow size',.1)
      CALL WMSETR('ARD - arrow direction',180.)
      CALL WMSETR('ARL - scale factor for length of arrow tail',1.8)
      CALL WMLABS(XE,YY,'Arrow')
C
C  Dots and cities.
C
      YY = YY-YINC
      CALL PCSETC('FC','%')
      CALL PLCHHQ(0.05,YY,'%F26%Dots and City info:',0.025,0.,-1.)       
      CALL PCSETC('FC',':')
      CALL PCGETR('XE',XE)
      XE = XE+0.05
      CALL WMSETI('DBC - color index for dot shadow',2)
      CALL WMSETI('DTC - color index for dot',3)
      CALL WMSETR('DTS - size of dot',0.012)
      CALL WMLABS(XE,YY,'DOT')
      XE = XE+0.10
      CALL WMSETI('RFC - color index for city labels',3)
      CALL WMSETI('CBC - color index background of city labels',0)
      CALL WMSETR('CHT - size of city labels',.02)
      CALL WMSETR('CMG - margins for city labels',.006)
      CALL WMLABC(XE,YY,'Boulder','83/68')
      XE = XE+0.15
      CALL WMSETI('DBC - color index for dot shadow',5)
      CALL WMSETI('DTC - color index for dot',1)
      CALL WMSETR('DTS - size of dot',0.024)
      CALL WMLABS(XE,YY,'DOT')
      XE = XE+0.12
      CALL WMSETI('RFC - color index for city labels',6)
      CALL WMSETI('CBC - color index background of city labels',3)
      CALL WMSETR('CHT - size of city labels',.03)
      CALL WMSETR('CMG - margins for city labels',.006)
      CALL WMLABC(XE,YY,'Tulsa','103/83')
C
C  Regional weather labels.
C
      YY = YY-YINC
      CALL PCSETC('FC','%')
      CALL PLCHHQ(0.05,YY,'%F26%Regional labels:',0.025,0.,-1.)       
      CALL PCSETC('FC',':')
      CALL PCGETR('XE',XE)
      XE = XE+0.13
      CALL WMSETR('WHT - size of label',0.02)
      CALL WMLABW(XE,YY,'TORRID')
      XE = XE+0.30
      CALL WMSETI('RC1 - color index for box outline',4)
      CALL WMSETI('RC2 - color index for character background',5)
      CALL WMSETI('RC3 - color index for box shadow',1)
      CALL WMSETI('RC4 - color index for text',3)
      CALL WMSETI('RC5 - color index for text outlines',2)
      CALL WMSETR('WHT - size of label',0.035)
      CALL WMLABW(XE,YY,'FREEZING')
C
C  Regional temps.
C
      YY = YY-YINC
      CALL PCSETC('FC','%')
      CALL PLCHHQ(0.05,YY,'%F26%Regional temps.:',0.025,0.,-1.)       
      CALL PCSETC('FC',':')
      CALL PCGETR('XE',XE)
      XE = XE+0.07
C
C  Reset (primarily for arrow lengths and sizes).
C
      CALL WMDFLT()
      CALL WMSETR('THT - Height of regional temperature labels',0.032)
      CALL WMSETI('RFC - primary character color',2)
      CALL WMLABT(XE,YY,'80s',0)
      XE = XE+0.12
      CALL WMSETR('ARS - arrow size',0.07)
      CALL WMSETI('ROS - color index for character outlines',0)
      CALL WMSETI('RFC - primary character color',2)
      CALL WMSETI('RLS - color index for shadows',1)
      CALL WMLABT(XE,YY,'80s',2)
      XE = XE+0.03
      CALL WMSETI('ROS - color index for character outlines',1)
      CALL WMSETI('RFC - primary character color',0)
      CALL WMSETI('RLS - color index for shadows',1)
      CALL WMLABT(XE,YY,'80s',6)
      XE = XE+0.15
      CALL WMSETI('ROS - color index for character outlines',-1)
      CALL WMSETI('RFC - primary character color',2)
      CALL WMSETI('RLS - color index for shadows',-1)
      CALL WMSETI('RBS - color index for backgrounds for labels',1)
      CALL WMSETR('RMG - size of margins around characters',0.01)
      CALL WMLABT(XE,YY,'80s',0)
C
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
      STOP
C
      END
