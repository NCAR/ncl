      PROGRAM NGSET2
C
C  This example shows the mapping from NCAR Grapics user coordinates to
C   GKS normalized device coordinates.
C
	DIMENSION XCRA(101),YCRA(101)
	DATA DSL,DSR,DSB,DST / .59,.99,.40,.70 /
	DATA PFL,PFR,PFB,PFT / .71,.98,.42,.69 /
	CALL OPNGKS
	CALL GSCLIP (0)
	CALL GSLWSC (2.)
	CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
	CALL PLCHMQ (.50,.98,
     +            'MAPPING FROM A WINDOW IN THE USER COORDINATE SYSTEM',
     +                                                       .015,0.,0.)
	CALL PLCHMQ (.50,.95,
     +       'TO A VIEWPORT IN THE NORMALIZED DEVICE COORDINATE SYSTEM',
     +                                                       .015,0.,0.)
	CALL PLCHMQ (.50,.92,
     +                  'USING A TRANSFORMATION DEFINED BY CALLING SET',
     +                                                       .015,0.,0.)
	CALL PLCHMQ (.50,.89,
     +                 '(USED IN CALLS TO MOST NCAR GRAPHICS ROUTINES)',
     +                                                       .012,0.,0.)
	CALL PLCHMQ (.50,.24,
     +   'Assume a CALL SET (.15,.95,.10,.90,1000.,100.,100.,1000.,2).',
     +                                                       .012,0.,0.)
	CALL PLCHMQ (.50,.21,
     +                  'The GKS viewport will be:  .15, .95, .10, .90',
     +                                                       .012,0.,0.)
	CALL PLCHMQ (.50,.18,
     +                   'The GKS window will be:  100., 1000., 2., 3.',
     +                                                       .012,0.,0.)
	CALL PLCHMQ (.50,.15,
     +        'The value of ''MI'' will be 3 (mirror-imaging of X''s).',
     +                                                       .012,0.,0.)
	CALL PLCHMQ (.50,.12,
     +           'The value of ''LS'' will be 2 (log scaling of Y''s).',
     +                                                       .012,0.,0.)
	CALL PLCHMQ (.50,.06,
     +                                                       'Figure 2',
     +                                                       .012,0.,0.)
	CALL SET    (.01,.55,.29,.83,0.,1100.,0.,1100.,1)
	CALL LINE   (   0.,   0.,1100.,   0.)
	CALL LINE   (1100.,   0.,1050.,  25.)
	CALL LINE   (1100.,   0.,1050., -25.)
	CALL PLCHMQ (CFUX(CUFX(1100.)+.015),0.,'X',.015,0.,0.)
	CALL LINE   (   0.,   0.,   0.,1100.)
	CALL LINE   (   0.,1100.,  25.,1050.)
	CALL LINE   (   0.,1100., -25.,1050.)
	CALL PLCHMQ (0.,CFUY(CUFY(1100.)+.015),'Y',.015,0.,0.)
	CALL LINE   ( 100., 100.,1000., 100.)
	CALL LINE   (1000., 100.,1000.,1000.)
	CALL LINE   (1000.,1000., 100.,1000.)
	CALL LINE   ( 100.,1000., 100., 100.)
	CALL PLCHMQ (CFUX(CUFX(100.)+.005),CFUY(CUFY(1000.)-.01),
     +               'WINDOW',.01,0.,-1.)
	CALL PLCHMQ (100.,CFUY(CUFY(100.)-.01),'100',.008,0.,0.)
	CALL PLCHMQ (1000.,CFUY(CUFY(100.)-.01),'1000',.008,0.,0.)
	CALL PLCHMQ (CFUX(CUFX(100.)-.005),100.,'100',.008,0.,1.)
	CALL PLCHMQ (CFUX(CUFX(100.)-.005),1000.,'1000',.008,0.,1.)
	DO 101 I=1,101
	  XCRA(I)=200.+7.*REAL(I-1)
	  YCRA(I)=200.+700.*(10.**((XCRA(I)-200.)/700.)-1.)/9.+
     +            100.*SIN((XCRA(I)-200.)/30.)
  101   CONTINUE
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
	CALL PLCHMQ (CFUX(CUFX(0.)+.005),CFUY(CUFY(1.)-.01),
     +               'DEVICE',.01,0.,-1.)
	CALL SET    (PFL,PFR,PFB,PFT,0.,1.,0.,1.,1)
	CALL LINE   (0.,0.,1.,0.)
	CALL LINE   (1.,0.,1.,1.)
	CALL LINE   (1.,1.,0.,1.)
	CALL LINE   (0.,1.,0.,0.)
	CALL PLCHMQ (CFUX(CUFX(0.)+.005),CFUY(CUFY(1.)-.01),
     +               'PLOTTER FRAME',.01,0.,-1.)
	CALL PLCHMQ (0.,CFUY(CUFY(0.)-.01),'0',.008,0.,0.)
	CALL PLCHMQ (1.,CFUY(CUFY(0.)-.01),'1',.008,0.,0.)
	CALL PLCHMQ (CFUX(CUFX(0.)-.005),0.,'0',.008,0.,1.)
	CALL PLCHMQ (CFUX(CUFX(0.)-.005),1.,'1',.008,0.,1.)
	VPL=PFL+.15*(PFR-PFL)
	VPR=PFL+.95*(PFR-PFL)
	VPB=PFB+.10*(PFT-PFB)
	VPT=PFB+.90*(PFT-PFB)
	CALL SET    (VPL,VPR,VPB,VPT,1000., 100., 100.,1000.,2)
	CALL LINE   (1000., 100., 100., 100.)
	CALL LINE   ( 100., 100., 100.,1000.)
	CALL LINE   ( 100.,1000.,1000.,1000.)
	CALL LINE   (1000.,1000.,1000., 100.)
	CALL PLCHMQ (CFUX(CUFX(1000.)+.005),CFUY(CUFY(1000.)-.01),
     +               'VIEWPORT',.01,0.,-1.)
	CALL PLCHMQ (1000.,CFUY(CUFY(100.)-.01),'.15',.008,0.,0.)
	CALL PLCHMQ (100.,CFUY(CUFY(100.)-.01),'.95',.008,0.,0.)
	CALL PLCHMQ (CFUX(CUFX(1000.)-.005),100.,'.10',.008,0.,1.)
	CALL PLCHMQ (CFUX(CUFX(1000.)-.005),1000.,'.90',.008,0.,1.)
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
	CALL CLSGKS
	STOP
      END
