
      PROGRAM FCOORD1
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
      CALL COORD1(IERR)
C
C     DEACTIVATE AND CLOSE WORKSTATION, CLOSE GKS.
C
      CALL GDAWK (IWKID)
      CALL GCLWK (IWKID)
      CALL GCLKS
      STOP
      END
      SUBROUTINE COORD1 (IERROR)
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
      CALL PCSETC('FC','%')
C
      CALL SET    (0.,1.,0.,1.,0.,1.,0.,1.,1)
C
      CALL PLCHHQ (.50,.97,
     +     'Mapping from a window in the world coordinate system',
     +     .02,0.,0.)
      CALL PLCHHQ (.50,.93,
     +     'to a viewport in the normalized device coordinate system',
     +     .02,0.,0.)
      CALL PCSETI('FN',21)
      CALL PLCHHQ (.50,.89,
     +'(Used in calls to GKS routines like GCA, GFA, GPL, GPM, and GTX)'
     +,.016,0.,0.)
      CALL PCSETI('FN',29)
      CALL PLCHHQ (.50,.19,
     +     'The window is:  100., 1000., 100., 1000.',
     +     .015,0.,0.)
      CALL PLCHHQ (.50,.15,
     +     'The viewport is:  .15, .95, .10, .90',
     +     .015,0.,0.)
      CALL SET    (.01,.55,.29,.83,0.,1100.,0.,1100.,1)
      CALL PCSETI('FN',22)
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
      CALL PLCHHQ (CFUX(CUFX(100.)+.005),CFUY(CUFY(1000.)-.02),
     +     'WINDOW',.01,0.,-1.)
      CALL PLCHHQ (100.,CFUY(CUFY(100.)-.01),'100',.009,0.,0.)
      CALL PLCHHQ (1000.,CFUY(CUFY(100.)-.01),'1000',.009,0.,0.)
      CALL PLCHHQ (CFUX(CUFX(100.)-.005),100.,'100',.009,0.,1.)
      CALL PLCHHQ (CFUX(CUFX(100.)-.005),1000.,'1000',.009,0.,1.)
      DO 101 I=1,101
         XCRA(I)=200.+7.*REAL(I-1)
         YCRA(I)=550.+425.*SIN(EXP((XCRA(I)-200.)/300.)-1.)
 101  CONTINUE
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
      CALL SET    (VPL,VPR,VPB,VPT, 100.,1000., 100.,1000.,1)
      CALL LINE   ( 100., 100.,1000., 100.)
      CALL LINE   (1000., 100.,1000.,1000.)
      CALL LINE   (1000.,1000., 100.,1000.)
      CALL LINE   ( 100.,1000., 100., 100.)
      CALL PLCHHQ (CFUX(CUFX(100.)+.005),CFUY(CUFY(1000.)-.01),
     +     'VIEWPORT',.01,0.,-1.)
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
      CALL DASHDC ('$''',3,1)
      CALL LINED  (XCLW,YCBW,XCLV,YCBV)
      CALL LINED  (XCRW,YCBW,XCRV,YCBV)
      CALL LINED  (XCRW,YCTW,XCRV,YCTV)
      CALL LINED  (XCLW,YCTW,XCLV,YCTV)
      CALL FRAME
      WRITE (6,1001)
      RETURN
C
 1001 FORMAT ('     COORD1 TEST SUCCESSFUL',24X,
     1     'SEE PLOTS TO VERIFY PERFORMANCE')
C
      END
