
      PROGRAM WMEX08
C
C  Example of some of the weather symbols for daily weather icons.
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
C  Set up color table.
C
      CALL GSCR(IWKID, 0, 0.87, 0.87, 1.00)
      CALL GSCR(IWKID, 1, 0.00, 0.00, 0.00)
      CALL GSCR(IWKID, 2, 0.00, 0.00, 1.00)
      CALL GSCR(IWKID, 3, 1.00, 1.00, 1.00)
      CALL GSCR(IWKID, 4, 1.00, 1.00, 0.00)
      CALL GSCR(IWKID, 5, 1.00, 0.65, 0.00)
C
C  Sun.
C
      CALL WMSETI('SC1 - color of the center',4)
      CALL WMSETI('SC2 - color of the star points',5)
      CALL WMSETI('SC3 - color of the outlines',1)
      CALL WMSETI('SC4 - shadow color',1)
      CALL WMSETR('SHT - size of sun',.0375)
      CALL WMLABS(0.27, 0.65, 'SU')
C
C  Cloud.
C
      CALL WMSETI('CC1 - primary color',3)
      CALL WMSETI('CC2 - outline color',1)
      CALL WMSETI('CC3 - shadow color',1)
      CALL WMSETR('SHT - size of cloud',0.047)
      CALL WMLABS(0.55, 0.33, 'C')
C
C  Title.
C
      CALL PLCHHQ(.52,.78,':F26:NCAR',.076,0.,-1.)
      CALL PLCHHQ(.52,.67,':F26:Graphics',.0555,0.,-1.)
C
C  Waves.
C
      WAVSIZ = .15
      CALL PCSETI('CC',2)
      CALL PCSETI('TE',1)
      CALL PLCHHQ(.5,.2,':F37:n',WAVSIZ,360.,0.)
      CALL PCSETI('TE',0)
      CALL PCGETR('DL',XL)
      CALL PCGETR('DR',XR)
      XWID = XL+XR
C
      XB = .2
      CALL GSLWSC(6.0)
      DO 10 I=1,4
        CALL PLCHHQ (XB,.15,':F37:n',WAVSIZ,0.,0.)
        XB = XB+XWID
   10 CONTINUE
C
      CALL FRAME
C
      CALL GDAWK(IWKID)
      CALL GCLWK(IWKID)
      CALL GCLKS
      STOP
C
      END
