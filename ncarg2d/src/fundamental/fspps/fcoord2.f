
      PROGRAM FCOORD2
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
C INVOKE DEMO DRIVER
C
      CALL COORD2(IERR)
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
      SUBROUTINE COORD2 (IERROR)
C
C  This example shows the mapping from NCAR Grapics user coordinates to
C   GKS normalized device coordinates.
C
      DIMENSION XCRA(101),YCRA(101)
      DATA DSL,DSR,DSB,DST / .59,.99,.40,.70 /
      DATA PFL,PFR,PFB,PFT / .71,.98,.42,.69 /
      CALL GSCLIP (0)
      CALL GSLWSC (2.)
C
C Employ the new high quality filled fonts in PLOTCHAR
C
      CALL PCSETC('FN','times-roman')
C
      CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
C
      CALL PCSETC('FC','%')
      CALL PLCHHQ (.50,.98,
     +     'Mapping from a window in the user coordinate system',
     +     .02,0.,0.)
      CALL PLCHHQ (.50,.94,
     +     'to a viewport in the normalized device coordinate system',
     +     .02,0.,0.)
      CALL PLCHHQ (.50,.90,
     +     'using a transformation defined by calling SET',
     +     .02,0.,0.)
      CALL PCSETI('FN',21)
      CALL PLCHHQ (.50,.86,
     +     '(Used in calls to most NCAR Graphics routines)',
     +     .016,0.,0.)
      CALL PCSETI('FN',29)
      CALL PLCHHQ (.50,.22,
     + 'Assume a CALL SET (.15,.95,.10,.90,1000.,100.,100.,1000.,2).',
     + .013,0.,0.)
      CALL PLCHHQ (.50,.185,
     +     'The GKS viewport is:  .15, .95, .10, .90',
     +     .013,0.,0.)
      CALL PLCHHQ (.50,.15,
     +     'The GKS window is:  100., 1000., 2., 3.',
     +     .013,0.,0.)
      CALL PLCHHQ (.50,.115,
     +     'The value of ''MI'' is 3 (mirror-imaging of X''s).',
     +     .013,0.,0.)
      CALL PLCHHQ (.50,.08,
     +     'The value of ''LS'' is 2 (log scaling of Y''s).',
     +     .013,0.,0.)
      CALL PCSETI('FN',22)
      CALL SET    (.01,.55,.29,.83,0.,1100.,0.,1100.,1)
      CALL LINE   (   0.,   0.,1100.,   0.)
      CALL LINE   (1100.,   0.,1050.,  25.)
      CALL LINE   (1100.,   0.,1050., -25.)
      CALL PLCHHQ (CFUX(CUFX(1100.)+.015),0.,'X',.015,0.,0.)
      CALL LINE   (   0.,   0.,   0.,1100.)
      CALL LINE   (   0.,1100.,  25.,1050.)
      CALL LINE   (   0.,1100., -25.,1050.)
      CALL PLCHHQ (0.,CFUY(CUFY(1100.)+.015),'Y',.015,0.,0.)
      CALL LINE   ( 100., 100.,1000., 100.)
      CALL LINE   (1000., 100.,1000.,1000.)
      CALL LINE   (1000.,1000., 100.,1000.)
      CALL LINE   ( 100.,1000., 100., 100.)
      CALL PLCHHQ (CFUX(CUFX(100.)+.005),CFUY(CUFY(1000.)-.01),
     +     'WINDOW',.01,0.,-1.)
      CALL PLCHHQ (100.,CFUY(CUFY(100.)-.01),'100',.008,0.,0.)
      CALL PLCHHQ (1000.,CFUY(CUFY(100.)-.01),'1000',.008,0.,0.)
      CALL PLCHHQ (CFUX(CUFX(100.)-.005),100.,'100',.008,0.,1.)
      CALL PLCHHQ (CFUX(CUFX(100.)-.005),1000.,'1000',.008,0.,1.)
      DO 101 I=1,101
         XCRA(I)=200.+7.*REAL(I-1)
         YCRA(I)=200.+700.*(10.**((XCRA(I)-200.)/700.)-1.)/9.+
     +        100.*SIN((XCRA(I)-200.)/30.)
 101  CONTINUE
      CALL CURVE  (XCRA,YCRA,101)
      XCLW=CUFX(100.)
      XCRW=CUFX(1000.)
      YCBW=CUFY(100.)
      YCTW=CUFY(1000.)
      CALL PLOTIT (0,0,0)
      CALL GSLWSC (1.)
      CALL LINE (100.,200.,1000.,200.)
      CALL LINE (100.,300.,1000.,300.)
      CALL LINE (100.,400.,1000.,400.)
      CALL LINE (100.,500.,1000.,500.)
      CALL LINE (100.,600.,1000.,600.)
      CALL LINE (100.,700.,1000.,700.)
      CALL LINE (100.,800.,1000.,800.)
      CALL LINE (100.,900.,1000.,900.)
      CALL LINE (200.,100.,200.,1000.)
      CALL LINE (300.,100.,300.,1000.)
      CALL LINE (400.,100.,400.,1000.)
      CALL LINE (500.,100.,500.,1000.)
      CALL LINE (600.,100.,600.,1000.)
      CALL LINE (700.,100.,700.,1000.)
      CALL LINE (800.,100.,800.,1000.)
      CALL LINE (900.,100.,900.,1000.)
      CALL PLOTIT (0,0,0)
      CALL GSLWSC (2.)
      CALL SET    (DSL,DSR,DSB,DST,0.,1.,0.,1.,1)
      CALL LINE   (0.,0.,1.,0.)
      CALL LINE   (1.,0.,1.,1.)
      CALL LINE   (1.,1.,0.,1.)
      CALL LINE   (0.,1.,0.,0.)
      CALL PLCHHQ (CFUX(CUFX(0.)+.005),CFUY(CUFY(1.)-.01),
     +     'DEVICE',.01,0.,-1.)
      CALL SET    (PFL,PFR,PFB,PFT,0.,1.,0.,1.,1)
      CALL LINE   (0.,0.,1.,0.)
      CALL LINE   (1.,0.,1.,1.)
      CALL LINE   (1.,1.,0.,1.)
      CALL LINE   (0.,1.,0.,0.)
      CALL PLCHHQ (CFUX(CUFX(0.)+.005),CFUY(CUFY(1.)-.01),
     +     'PLOTTER FRAME',.01,0.,-1.)
      CALL PLCHHQ (0.,CFUY(CUFY(0.)-.01),'0',.008,0.,0.)
      CALL PLCHHQ (1.,CFUY(CUFY(0.)-.01),'1',.008,0.,0.)
      CALL PLCHHQ (CFUX(CUFX(0.)-.005),0.,'0',.008,0.,1.)
      CALL PLCHHQ (CFUX(CUFX(0.)-.005),1.,'1',.008,0.,1.)
      VPL=PFL+.15*(PFR-PFL)
      VPR=PFL+.95*(PFR-PFL)
      VPB=PFB+.10*(PFT-PFB)
      VPT=PFB+.90*(PFT-PFB)
      CALL SET    (VPL,VPR,VPB,VPT,1000., 100., 100.,1000.,2)
      CALL LINE   (1000., 100., 100., 100.)
      CALL LINE   ( 100., 100., 100.,1000.)
      CALL LINE   ( 100.,1000.,1000.,1000.)
      CALL LINE   (1000.,1000.,1000., 100.)
      CALL PLCHHQ (CFUX(CUFX(1000.)+.005),CFUY(CUFY(1000.)-.01),
     +     'VIEWPORT',.01,0.,-1.)
      CALL PLCHHQ (1000.,CFUY(CUFY(100.)-.01),'.15',.008,0.,0.)
      CALL PLCHHQ (100.,CFUY(CUFY(100.)-.01),'.95',.008,0.,0.)
      CALL PLCHHQ (CFUX(CUFX(1000.)-.005),100.,'.10',.008,0.,1.)
      CALL PLCHHQ (CFUX(CUFX(1000.)-.005),1000.,'.90',.008,0.,1.)
      CALL CURVE  (XCRA,YCRA,101)
      CALL PLOTIT (0,0,0)
      CALL GSLWSC (1.)
      CALL LINE (100.,200.,1000.,200.)
      CALL LINE (100.,300.,1000.,300.)
      CALL LINE (100.,400.,1000.,400.)
      CALL LINE (100.,500.,1000.,500.)
      CALL LINE (100.,600.,1000.,600.)
      CALL LINE (100.,700.,1000.,700.)
      CALL LINE (100.,800.,1000.,800.)
      CALL LINE (100.,900.,1000.,900.)
      CALL LINE (200.,100.,200.,1000.)
      CALL LINE (300.,100.,300.,1000.)
      CALL LINE (400.,100.,400.,1000.)
      CALL LINE (500.,100.,500.,1000.)
      CALL LINE (600.,100.,600.,1000.)
      CALL LINE (700.,100.,700.,1000.)
      CALL LINE (800.,100.,800.,1000.)
      CALL LINE (900.,100.,900.,1000.)
      CALL PLOTIT (0,0,0)
      CALL GSLWSC (2.)
      XCLV=CUFX(1000.)
      XCRV=CUFX(100.)
      YCBV=CUFY(100.)
      YCTV=CUFY(1000.)
      CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
      CALL PLOTIT (0,0,0)
      CALL GSLWSC (1.)
      CALL DASHDC ('$''',3,1)
      CALL LINED  (XCLW,YCBW,XCRV,YCBV)
      CALL LINED  (XCRW,YCBW,XCLV,YCBV)
      CALL LINED  (XCRW,YCTW,XCLV,YCTV)
      CALL LINED  (XCLW,YCTW,XCRV,YCTV)
      CALL FRAME
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT ('     COORD2 TEST SUCCESSFUL',24X,
     1     'SEE PLOTS TO VERIFY PERFORMANCE')
C
      END
