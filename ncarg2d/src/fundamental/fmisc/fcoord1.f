C
C OPEN GKS, OPEN WORKSTATION OF TYPE 1, ACTIVATE WORKSTATION
C
      CALL GOPKS (6,IDUM) 
      CALL GOPWK (1, 2, 1)
      CALL GACWK (1) 
C
C INVOKE DEMO DRIVER
C
      CALL SPSET1(IERR)
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (1)
      CALL GCLWK (1)
      CALL GCLKS
      STOP
      END
      SUBROUTINE SPSET1 (IERROR)
C
C  This example shows the mapping from GKS world coordinates to
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
	CALL PLCHHQ (.50,.98,
     +           'MAPPING FROM A WINDOW IN THE WORLD COORDINATE SYSTEM',
     +                                                       .015,0.,0.)
	CALL PLCHHQ (.50,.95,
     +       'TO A VIEWPORT IN THE NORMALIZED DEVICE COORDINATE SYSTEM',
     +                                                       .015,0.,0.)
	CALL PLCHHQ (.50,.92,'(USED IN CALLS TO GKS ROUTINES LIKE GCA, G
     +FA, GPL, GPM, AND GTX)',
     +                                                       .012,0.,0.)
	CALL PLCHHQ (.50,.15,
     +        'Assume the current window is   100., 1000., 100., 1000.',
     +                                                       .012,0.,0.)
	CALL PLCHHQ (.50,.12,
     +            'Assume the current viewport is   .15, .95, .10, .90',
     +                                                       .012,0.,0.)
	CALL PLCHHQ (.50,.06,
     +                                                       'Figure 1',
     +                                                       .012,0.,0.)
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
     +               'WINDOW',.01,0.,-1.)
	CALL PLCHHQ (100.,CFUY(CUFY(100.)-.01),'100',.008,0.,0.)
	CALL PLCHHQ (1000.,CFUY(CUFY(100.)-.01),'1000',.008,0.,0.)
	CALL PLCHHQ (CFUX(CUFX(100.)-.005),100.,'100',.008,0.,1.)
	CALL PLCHHQ (CFUX(CUFX(100.)-.005),1000.,'1000',.008,0.,1.)
	DO 101 I=1,101
	  XCRA(I)=200.+7.*REAL(I-1)
	  YCRA(I)=550.+425.*SIN(EXP((XCRA(I)-200.)/300.)-1.)
  101   CONTINUE
	CALL CURVE  (XCRA,YCRA,101)
	XCLW=CUFX(100.)
	XCRW=CUFX(1000.)
	YCBW=CUFY(100.)
	YCTW=CUFY(1000.)
	CALL SET    (DSL,DSR,DSB,DST,0.,1.,0.,1.,1)
	CALL LINE   (0.,0.,1.,0.)
	CALL LINE   (1.,0.,1.,1.)
	CALL LINE   (1.,1.,0.,1.)
	CALL LINE   (0.,1.,0.,0.)
	CALL PLCHHQ (CFUX(CUFX(0.)+.005),CFUY(CUFY(1.)-.01),
     +               'DEVICE',.01,0.,-1.)
	CALL SET    (PFL,PFR,PFB,PFT,0.,1.,0.,1.,1)
	CALL LINE   (0.,0.,1.,0.)
	CALL LINE   (1.,0.,1.,1.)
	CALL LINE   (1.,1.,0.,1.)
	CALL LINE   (0.,1.,0.,0.)
	CALL PLCHHQ (CFUX(CUFX(0.)+.005),CFUY(CUFY(1.)-.01),
     +               'PLOTTER FRAME',.01,0.,-1.)
	CALL PLCHHQ (0.,CFUY(CUFY(0.)-.01),'0',.008,0.,0.)
	CALL PLCHHQ (1.,CFUY(CUFY(0.)-.01),'1',.008,0.,0.)
	CALL PLCHHQ (CFUX(CUFX(0.)-.005),0.,'0',.008,0.,1.)
	CALL PLCHHQ (CFUX(CUFX(0.)-.005),1.,'1',.008,0.,1.)
	VPL=PFL+.15*(PFR-PFL)
	VPR=PFL+.95*(PFR-PFL)
	VPB=PFB+.10*(PFT-PFB)
	VPT=PFB+.90*(PFT-PFB)
	CALL SET    (VPL,VPR,VPB,VPT, 100.,1000., 100.,1000.,1)
	CALL LINE   ( 100., 100.,1000., 100.)
	CALL LINE   (1000., 100.,1000.,1000.)
	CALL LINE   (1000.,1000., 100.,1000.)
	CALL LINE   ( 100.,1000., 100., 100.)
	CALL PLCHHQ (CFUX(CUFX(100.)+.005),CFUY(CUFY(1000.)-.01),
     +               'VIEWPORT',.01,0.,-1.)
	CALL PLCHHQ (100.,CFUY(CUFY(100.)-.01),'.15',.008,0.,0.)
	CALL PLCHHQ (1000.,CFUY(CUFY(100.)-.01),'.95',.008,0.,0.)
	CALL PLCHHQ (CFUX(CUFX(100.)-.005),100.,'.10',.008,0.,1.)
	CALL PLCHHQ (CFUX(CUFX(100.)-.005),1000.,'.90',.008,0.,1.)
	CALL CURVE  (XCRA,YCRA,101)
	XCLV=CUFX(100.)
	XCRV=CUFX(1000.)
	YCBV=CUFY(100.)
	YCTV=CUFY(1000.)
	CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
	CALL PLOTIT (0,0,0)
	CALL GSLWSC (1.)
	CALL DASHDC ('$''',3,1)
	CALL LINED  (XCLW,YCBW,XCLV,YCBV)
	CALL LINED  (XCRW,YCBW,XCRV,YCBV)
	CALL LINED  (XCRW,YCTW,XCRV,YCTV)
	CALL LINED  (XCLW,YCTW,XCLV,YCTV)
	CALL FRAME
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT ('     SPSET1 TEST SUCCESSFUL',24X,
     1        'SEE PLOTS TO VERIFY PERFORMANCE')
C
      END
