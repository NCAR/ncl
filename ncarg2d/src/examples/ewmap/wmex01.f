
      PROGRAM WMEX01
C
C  Example of weather fronts.
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
      DIMENSION X(2),Y(2)
      DATA X/ 0.40, 0.90/
      DATA Y/ 0.85, 0.85/
      DATA DELY,XP,ALSIZ/0.09,0.375,0.024/
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
      CALL GSCR(IWKID, 3, 0.5, 0.0, 0.7)
      CALL GSCR(IWKID, 4, 0.0, 1.0, 0.0)
      CALL GSCR(IWKID, 5, 1.0, 0.5, 0.0)
      CALL GSCR(IWKID, 6, 0.0, 0.0, 1.0)
C
C  Plot title.
C
      CALL PLCHHQ(0.50,0.95,':F26:Front Types',0.035,0.,0.)
C
C  Define some parameters.
C
      CALL WMSETR('LIN - line widths of warm/cold front lines',3.)
      CALL WMSETR('BEG - space before first front symbol',0.02)
      CALL WMSETR('END - space after last front symbol',0.02)
      CALL WMSETR('BET - space between front symbols',.03)
      CALL WMSETR('SWI - size of symbols on front line',.04)
      CALL WMSETI('WFC - color for warm front symbols',2)
      CALL WMSETI('CFC - color for cold front symbols',6)
C
C  Warm front.
C
      CALL WMSETC('FRO - front type','WARM')
      CALL WMSETI('REV - reverse the direction of the symbols',1)
      CALL WMDRFT(2,X,Y)      
      CALL PLCHHQ(XP,Y(1),':F22:Warm',ALSIZ,0.,1.)
C
C  Warm front, aloft.
C
      Y(1) = Y(1)-DELY
      Y(2) = Y(1)
      CALL WMSETC('FRO - fornt type','WARM')
      CALL WMSETI('ALO - specify aloft',1)
      CALL WMSETI('REV - reverse the direction of the symbols',1)
      CALL WMDRFT(2,X,Y)      
      CALL PLCHHQ(XP,Y(1),':F22:Warm, aloft',ALSIZ,0.,1.)
      CALL WMSETI('ALO - deactivate aloft flag',0)
C
C  Cold front.
C
      Y(1) = Y(1)-DELY
      Y(2) = Y(1)
      CALL WMSETC('FRO - front type','COLD')
      CALL WMSETI('REV - reverse the direction of the symbols',1)
      CALL WMDRFT(2,X,Y)      
      CALL PLCHHQ(XP,Y(1),':F22:Cold',ALSIZ,0.,1.)
C
C  Cold front, aloft.
C
      Y(1) = Y(1)-DELY
      Y(2) = Y(1)
      CALL WMSETC('FRO - front type','COLD')
      CALL WMSETI('ALO - specify aloft',1)
      CALL WMSETI('REV - reverse the direction of the symbols',1)
      CALL WMDRFT(2,X,Y)      
      CALL PLCHHQ(XP,Y(1),':F22:Cold, aloft',ALSIZ,0.,1.)
      CALL WMSETI('ALO - deactivate aloft flag',0)
C
C  Stationary front.
C
      Y(1) = Y(1)-DELY
      Y(2) = Y(1)
      CALL WMSETC('FRO - front type','STATIONARY')
      CALL WMSETI('REV - reverse the direction of the symbols',1)
      CALL WMDRFT(2,X,Y)      
      CALL PLCHHQ(XP,Y(1),':F22:Stationary',ALSIZ,0.,1.)
C
C  Stationary front, aloft.
C
      Y(1) = Y(1)-DELY
      Y(2) = Y(1)
      CALL WMSETC('FRO - front type','STATIONARY')
      CALL WMSETI('REV - reverse the direction of the symbols',1)
      CALL WMSETI('ALO - specify aloft',1)
      CALL WMDRFT(2,X,Y)      
      CALL PLCHHQ(XP,Y(1),':F22:Stationary, aloft',ALSIZ,0.,1.)
      CALL WMSETI('ALO - deactivate aloft flag',0)
C
C  Occluded front.
C
      Y(1) = Y(1)-DELY
      Y(2) = Y(1)
      CALL WMSETI('WFC - color for warm front symbols',3)
      CALL WMSETI('CFC - color for cold front symbols',3)
      CALL WMSETC('FRO - front type','OCCLUDED')
      CALL WMSETI('REV - reverse the direction of the symbols',1)
      CALL WMDRFT(2,X,Y)      
      CALL PLCHHQ(XP,Y(1),':F22:Occluded',ALSIZ,0.,1.)
C
C     CALL WMDFLT
      CALL WMSETR('DWD - line widths for fronts with no symbols',3.)
C
C  Convergence line
C
      Y(1) = Y(1)-DELY
      Y(2) = Y(1)
      CALL WMSETC('FRO - front type','CONVERGENCE')
      CALL WMSETI('COL - convergence lines are orange',5)
      CALL WMDRFT(2,X,Y)      
      CALL PLCHHQ(XP,Y(1),':F22:Convergence line',ALSIZ,0.,1.)
C
C  Instability line.
C
      Y(1) = Y(1)-DELY
      Y(2) = Y(1)
      CALL WMSETC('FRO - front type','SQUALL')
      CALL WMSETI('COL - instability line drawn in black',1)
      CALL WMDRFT(2,X,Y)      
      CALL PLCHHQ(XP,Y(1),':F22:Instability line',ALSIZ,0.,1.)
C
C  Intertropical front.
C
      Y(1) = Y(1)-0.9*DELY
      Y(2) = Y(1)
      CALL WMSETI('T1C - one color for alternating dash pattern',2) 
      CALL WMSETI('T2C - second color for dash pattern',4) 
      CALL WMSETC('FRO front type','TROPICAL')
      CALL WMDRFT(2,X,Y)      
      CALL PLCHHQ(XP,Y(1),':F22:Intertropical',ALSIZ,0.,1.)
C
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
      STOP
C
      END
