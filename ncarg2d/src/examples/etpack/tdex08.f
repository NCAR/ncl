
      PROGRAM TDEX08
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.  Use one
C of the following:
C
C       PARAMETER (IERF=6,LUNI=2,IWTY=1 ,IWID=1)  !  NCGM
C       PARAMETER (IERF=6,LUNI=2,IWTY=8 ,IWID=1)  !  X Windows
C       PARAMETER (IERF=6,LUNI=2,IWTY=20,IWID=1)  !  PostScript
C       PARAMETER (IERF=6,LUNI=2,IWTY=11,IWID=1)  !  PDF, Portrait
C       PARAMETER (IERF=6,LUNI=2,IWTY=12,IWID=1)  !  PDF, Landscape
C
        PARAMETER (IERF=6,LUNI=2,IWTYPE=1,IWTY=IWTYPE,IWID=1)
C
C Declare variables to hold labels.
C
        CHARACTER*64 UNLB,VNLB,WNLB,UILB,VILB,WILB
C
C Set the desired minimum and maximum values of U, V, and W.
C
        DATA UMIN,VMIN,WMIN,UMAX,VMAX,WMAX / -1.,-1.,-1.,1.,1.,1. /
C
C Set the desired values of parameters determining the eye position.
C ANG1 is a bearing angle, ANG2 is an elevation angle, and RMUL is a
C multiplier of the length of the diagonal of the data box, specifying
C the distance from the center of the box to the eye.
C
        DATA ANG1,ANG2,RMUL / -35.,25.,2.9 /
C
C ISTE is a flag that says whether to do a simple image (ISTE=0),
C a one-frame stereo image (ISTE=-1), or a two-frame stereo image
C (ISTE=+1).
C
        DATA ISTE / -1 /
C
C ASTE is the desired angle (in degrees) between the lines of sight for
C a pair of stereo views.
C
        DATA ASTE / 4. /
C
C WOSW is the width of the stereo windows to be used in one-frame stereo
C images; the width is stated as a fraction of the width of the plotter
C frame.  (The windows are centered vertically; horizontally, they are
C placed as far apart as possible in the plotter frame.)  The value used
C must be positive and non-zero; it may be slightly greater than .5, if
C it is desired that the stereo windows should overlap slightly.
C
        DATA WOSW / .5 /
C
C Define the number of points to be used in defining the curve.
C
        PARAMETER (NCRV=1001)
C
C Declare some arrays in which to define the curve.
C
        DIMENSION XCRV(NCRV),YCRV(NCRV),ZCRV(NCRV)
C
C Define the conversion constant from degrees to radians.
C
        DATA DTOR / .017453292519943 /
C
C Define labels for the edges of the box.
C
        DATA UNLB / ' -1 -.8 -.6 -.4 -.2 0 .2 .4 .6 .8 1 ' /
        DATA VNLB / ' -1 -.8 -.6 -.4 -.2 0 .2 .4 .6 .8 1 ' /
        DATA WNLB / ' -1 -.8 -.6 -.4 -.2 0 .2 .4 .6 .8 1 ' /
C
        DATA UILB / 'U Coordinate Values' /
        DATA VILB / 'V Coordinate Values' /
        DATA WILB / 'W Coordinate Values' /
C
C Define the curve.
C
        DO 101 I=1,NCRV
          XCRV(I)=.75*COS(DTOR*(REAL(I-1)/REAL(NCRV-1))*720.)
          YCRV(I)=.75*SIN(DTOR*(REAL(I-1)/REAL(NCRV-1))*720.)
          ZCRV(I)=-.75+1.5*REAL(I-1)/REAL(NCRV-1)
  101   CONTINUE
C
C Open GKS.
C
        CALL GOPKS (IERF,ISZDM)
        CALL GOPWK (IWID,LUNI,IWTY)
        CALL GACWK (IWID)
C
C Turn clipping off.
C
        CALL GSCLIP (0)
C
C Select font number 25, turn on the outlining of filled fonts, set the
C line width to 1, and turn off the setting of the outline color.
C
        CALL PCSETI ('FN - FONT NUMBER',25)
        CALL PCSETI ('OF - OUTLINE FLAG',1)
        CALL PCSETR ('OL - OUTLINE LINE WIDTH',1.)
        CALL PCSETR ('OC - OUTLINE LINE COLOR',-1.)
C
C Use a 25-degree (instead of the default 20-degree) field of view.
C
        CALL TDSETI ('FOV',25)
C
C Make TDPACK characters a bit bigger.
C
        CALL TDSETR ('CS1',1.25)
C
C Find the midpoint of the data box (to be used as the point looked at).
C
        UMID=.5*(UMIN+UMAX)
        VMID=.5*(VMIN+VMAX)
        WMID=.5*(WMIN+WMAX)
C
C Determine the distance (R) from which the data box will be viewed and,
C given that, the eye position.
C
        R=RMUL*SQRT((UMAX-UMIN)**2+(VMAX-VMIN)**2+(WMAX-WMIN)**2)
C
        UEYE=UMID+R*COS(DTOR*ANG1)*COS(DTOR*ANG2)
        VEYE=VMID+R*SIN(DTOR*ANG1)*COS(DTOR*ANG2)
        WEYE=WMID+R*SIN(DTOR*ANG2)
C
C Initialize the stereo offset argument to do either a single view or
C a left-eye view (whichever is selected by the value of ISTE).
C
        IF (ISTE.EQ.0) THEN
          OTEP=0.
        ELSE
          OTEP=-R*TAN(DTOR*ASTE/2.)
        END IF
C
C Initialize TDPACK.
C
  102   CALL TDINIT (UEYE,VEYE,WEYE,UMID,VMID,WMID,
     +                              UMID,VMID,WMID+R,OTEP)
C
C If stereo views are being done, do the requested thing, either by
C redoing the SET call to put them side by side on the same frame,
C or by calling FRAME to put them on separate frames.
C
        IF (OTEP.NE.0.) THEN
          IF (ISTE.LT.0) THEN
            CALL GETSET (XVPL,XVPR,YVPB,YVPT,XWDL,XWDR,YWDB,YWDT,LNLG)
            IF (OTEP.LT.0.) THEN
              CALL SET  (1.-WOSW,1.,.5-.5*WOSW,.5+.5*WOSW,
     +                           XWDL,XWDR,YWDB,YWDT,LNLG)
            ELSE
              CALL SET  (  0., WOSW,.5-.5*WOSW,.5+.5*WOSW,
     +                           XWDL,XWDR,YWDB,YWDT,LNLG)
            END IF
          ELSE
            IF (OTEP.GT.0.) CALL FRAME
          END IF
        END IF
C
C Draw labels for the axes.
C
        CALL TDLBLS (UMIN,VMIN,WMIN,UMAX,VMAX,WMAX,
     +               UNLB,VNLB,WNLB,UILB,VILB,WILB,1)
C
C Draw the sides of the box that could be hidden.
C
        CALL TDGRDS (UMIN,VMIN,WMIN,UMAX,VMAX,WMAX,
     +               .1*(UMAX-UMIN),.1*(VMAX-VMIN),.1*(WMAX-WMIN),
     +                                                       12,1)
C
C Draw the curve in pieces, with an arrowhead on each one.
C
          DO 103 J=1,NCRV,100
C
C Compute the number of points on this piece.
C
            NPTS=MIN(100,NCRV-J+1)
C
C Draw a curve.  The fourth argument says whether or not there is to be
C an arrowhead on the end of the curve and, if so, how many points are
C to be used in drawing its conical base (48, in the third case) and how
C many of those points are to be connected to the tip (12, in the third
C case).
C
C           CALL TDCURV (XCRV(J),YCRV(J),ZCRV(J),NPTS,    0,.2,.1)
C           CALL TDCURV (XCRV(J),YCRV(J),ZCRV(J),NPTS,    1,.2,.1)
            CALL TDCURV (XCRV(J),YCRV(J),ZCRV(J),NPTS,12048,.2,.1)
C
  103     CONTINUE
C
C Draw the sides of the box that could not be hidden.
C
        CALL TDGRDS (UMIN,VMIN,WMIN,UMAX,VMAX,WMAX,
     +               .1*(UMAX-UMIN),.1*(VMAX-VMIN),.1*(WMAX-WMIN),
     +                                                       12,0)
C
C If a left-eye view has just been done, loop back for a right-eye view.
C
        IF (OTEP.LT.0.) THEN
          OTEP=-OTEP
          GO TO 102
        END IF
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL GDAWK (IWID)
        CALL GCLWK (IWID)
        CALL GCLKS
C
C Done.
C
        STOP
C
      END
