
      PROGRAM WMEX04
C
C  Examples of parameter control of fronts.
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
      PARAMETER (NS=2)
      DIMENSION XS(NS),YS(NS)
      DATA XS/ 0.10, 0.90/ 
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
      CALL GSCR(IWKID, 4, 0.4, 0.0, 0.4)
C
C  Plot title.
C
      CALL PLCHHQ(0.50,0.94,
     + ':F26:Parameter control of front attributes', 0.03,0.,0.)
C
C  Various fronts with different attributes.
C
      CALL PCSETI('CC',4)
      FSIZE = .021
      YS(1) = .76
      YS(2) = .76
      CALL PCSETC('FC','%')
      CALL PLCHHQ(XS(1),YS(1)+.06,'%F22%Starting with FRO=''STA'', WFC=2
     + (red), CFC=3 (blue):',FSIZE,0.,-1.)
      CALL WMSETC('FRO','STA')
      CALL WMSETI('WFC',2)
      CALL WMSETI('CFC',3)
      CALL WMDRFT(NS,XS,YS)
C
      YS(1) = .60
      YS(2) = .60
      CALL PLCHHQ(XS(1),YS(1)+.06,'%F22%then setting BEG=0., END=.05, BE
     +T=.03 gives:',FSIZE,0.,-1.)
      CALL WMSETR('BEG',0.00)
      CALL WMSETR('END',0.05)
      CALL WMSETR('BET',0.03)
      CALL WMDRFT(NS,XS,YS)
C
      YS(1) = .44
      YS(2) = .44
      CALL PLCHHQ(XS(1),YS(1)+.06,'%F22%then setting NMS=5 and STY=-1,2,       
     +1,-2,2 gives:',FSIZE,0.,-1.)
      CALL WMSETI('NMS',5)
      CALL WMSETI('PAI',1)
      CALL WMSETI('STY',-1)
      CALL WMSETI('PAI',2)
      CALL WMSETI('STY',2)
      CALL WMSETI('PAI',3)
      CALL WMSETI('STY',1)
      CALL WMSETI('PAI',4)
      CALL WMSETI('STY',-2)
      CALL WMSETI('PAI',5)
      CALL WMSETI('STY',2)
      CALL WMDRFT(NS,XS,YS)
C
      YS(1) = .27
      YS(2) = .27
      CALL PLCHHQ(XS(1),YS(1)+.07,'%F22%then setting SWI=.05 and LIN=12.
     + gives:',FSIZE,0.,-1.)
      CALL WMSETR('SWI',0.05)
      CALL WMSETR('LIN',12.)
      CALL WMDRFT(NS,XS,YS)
C
      YS(1) = .10
      YS(2) = .10
      CALL PLCHHQ(XS(1),YS(1)+.07,'%F22%then setting REV=1 gives:',
     +            .022,0.,-1.)
      CALL WMSETI('REV',1)
      CALL WMDRFT(NS,XS,YS)
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
      STOP
C
      END
