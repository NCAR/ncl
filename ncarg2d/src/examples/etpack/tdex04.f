
      PROGRAM TDEX04
C
C Define the error file, the Fortran unit number, the workstation type,
C and the workstation ID to be used in calls to GKS routines.
C
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)   ! NCGM
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=8,  IWKID=1)   ! X Windows
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=11, IWKID=1)   ! PDF
C       PARAMETER (IERRF=6, LUNIT=2, IWTYPE=20, IWKID=1)   ! PostScript
C
        PARAMETER (IERRF=6, LUNIT=2, IWTYPE=1,  IWKID=1)
C
C This is a modified version of the TDPACK example "tdex01".  The two
C surfaces have been modified to illustrate new capabilities.  The
C color of the simple surface ranges from blue at the bottom to red at
C the top, while the lumpy doughnut that intersects it is shown in
C yellow.  In addition, cyan lines are used to indicate selected planes
C of constant U, V, or W.
C
C Create parameters specifying the maximum sizes of the arrays defining
C data and the arrays required for dealing with the list of triangles.
C
        PARAMETER (IMAX=81,JMAX=81,KMAX=81,MTRI=400000)
C
C Set the desired values of the dimensions of the data arrays.  Note
C that IDIM must not exceed IMAX, that JDIM must not exceed JMAX, and
C that KDIM must not exceed KMAX.  NLYR is the number of vertical
C layers to be used in rendering the simple surface.  There is an
C inverse relationship between the number of layers you use for the
C surface (which determines how many different colors will be used)
C and the number of shades of those colors that you can generate.
C (The shades are used to give a visual sense of the angle between
C the line of sight and the normal to the surface.)
C
        PARAMETER (IDIM=81,JDIM=81,KDIM=81,NLYR=10)
C
C Declare local dimensioned variables to hold data defining a simple
C surface and an isosurface.
C
        DIMENSION U(IMAX),V(JMAX),W(KMAX),S(IMAX,JMAX),F(IMAX,JMAX,KMAX)
C
C Declare a local array to hold the triangle list and a couple of
C temporary variables to be used in sorting the list.
C
        DIMENSION RTRI(10,MTRI),RTWK(MTRI,2),ITWK(MTRI)
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
C       DATA ISTE /  0 /
C       DATA ISTE / +1 /
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
C Set the desired value of the flag that says whether the basic color
C scheme will be white on black (IBOW=0) or black on white (IBOW=1).
C
        DATA IBOW / 1 /
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
C Define the limits for the cyan stripes indicating certain values of
C U, V, or W.
C
C       DATA USMN,USMX /  .44 ,  .46 /
C       DATA VSMN,VSMX / -.56 , -.54 /
C       DATA WSMN,WSMX / -.16 , -.14 /
C
        DATA USMN,USMX / -.01 , +.01 /
        DATA VSMN,VSMX / -.01 , +.01 /
        DATA WSMN,WSMX / -.01 , +.01 /
C
C Open GKS.
C
        CALL GOPKS (IERRF, ISZDM)
        CALL GOPWK (IWKID, LUNIT, IWTYPE)
        CALL GACWK (IWKID)
C
C Turn clipping off.
C
        CALL GSCLIP (0)
C
C Double the line width.
C
        CALL GSLWSC (2.)
C
C Define the background color and the basic foreground color (either
C black and white or white and black).
C
        IF (IBOW.EQ.0) THEN
          CALL GSCR (1,0,0.,0.,0.)
          CALL GSCR (1,1,1.,1.,1.)
        ELSE
          CALL GSCR (1,0,1.,1.,1.)
          CALL GSCR (1,1,0.,0.,0.)
        END IF
C
C Define the primary colors (2 = red; 3 = green; 4 = blue; 5 = cyan;
C 6 = magenta; and 7 = yellow).
C
        CALL GSCR   (1,2,1.,0.,0.)
        CALL GSCR   (1,3,0.,1.,0.)
        CALL GSCR   (1,4,0.,0.,1.)
        CALL GSCR   (1,5,0.,1.,1.)
        CALL GSCR   (1,6,1.,0.,1.)
        CALL GSCR   (1,7,1.,1.,0.)
C
C Now we need a bunch more colors.  Each of the NLYR layers of the
C simple surface is to be a different color and the isosurface is
C to be made yet another color.  Additionally, we want to vary the
C shading of each surface layer and of the isosurface as implied by
C the angle between the line of sight and the local normal.  As we
C define the colors required, we define the required rendering styles
C for TDPACK.  ILCU is the index of the last color used so far.
C
        ILCU=7
C
C NSHD is the largest number of shades of each color that we can
C generate.  (Note the trade-off between the number of levels and
C the number of shades at each level.  With 31 levels, we get about
C 7 shades of each color, which is not really enough.)
C
        NSHD=(256-(ILCU+1))/(NLYR+1)
C
C Generate NSHD shades of each of NLYR+1 colors and use them to
C create NLYR+1 different rendering styles.  Note that I have set
C up the shades to run from the brightest shade down to .2 times
C the brightest shade.  This looks pretty good to my eye, but
C others may prefer something different.
C
        DO 101 I=1,NLYR+1
          IF (I.LE.NLYR) THEN
C           Colors run from blue to red.
            R=REAL(I-1)/REAL(NLYR-1)
            G=0.
            B=1.-R
          ELSE IF (I.EQ.NLYR+1) THEN
C           Colors are all yellow.
            R=1.
            G=1.
            B=0.
          END IF
          CALL TDSTRS (I,1,1,ILCU+1,ILCU+NSHD,-1,-1,0,0.,0.,0.)
          DO 100 J=1,NSHD
            ILCU=ILCU+1
            P=REAL(NSHD-J+1)/REAL(NSHD)
            CALL GSCR (1,ILCU,.2+.8*P*R,.2+.8*P*G,.2+.8*P*B)
  100     CONTINUE
  101   CONTINUE
C
C Define one more rendering style, for the stripes that will be used
C to mark selected planes of constant U, V, or W.  This involves only
C a single color (cyan, color index 5) because we want the stripes to
C really stand out.
C
        CALL TDSTRS (NLYR+2,1,1,5,5,-1,-1,0,0.,0.,0.)
C
C Fill data arrays defining a simple surface and an isosurface.  The
C simple surface is defined by the equation "w=s(u,v)"; the function
C "s" is approximated by the contents of the array S: S(I,J) is the
C value of s(U(I),V(J)), where I goes from 1 to IDIM and J from 1 to
C JDIM.  The isosurface is defined by the equation f(u,v,w)=1.; the
C function f is approximated by the contents of the array F: F(I,J,K)
C is the value of f(U(I),V(J),W(K)), where I goes from 1 to IDIM, J
C from 1 to JDIM, and K from 1 to KDIM.
C
        DO 102 I=1,IDIM
          U(I)=UMIN+(REAL(I-1)/REAL(IDIM-1))*(UMAX-UMIN)
  102   CONTINUE
C
        DO 103 J=1,JDIM
          V(J)=VMIN+(REAL(J-1)/REAL(JDIM-1))*(VMAX-VMIN)
  103   CONTINUE
C
        DO 104 K=1,KDIM
          W(K)=WMIN+(REAL(K-1)/REAL(KDIM-1))*(WMAX-WMIN)
  104   CONTINUE
C
        DO 107 I=1,IDIM
          DO 106 J=1,JDIM
            S(I,J)=2.*EXP(-2.*(U(I)**2+V(J)**2))-1.
            DO 105 K=1,KDIM
              F(I,J,K)=1.25*U(I)**2+1.25*V(J)**2+1.25*W(K)**2
              F(I,J,K)=F(I,J,K)*(1.75+.85*
     +                           SIN(90.*DTOR+5.*ATAN2(V(J),U(I))))
  105       CONTINUE
  106     CONTINUE
  107   CONTINUE
C
C Select font number 25, turn on the outlining of filled fonts, set the
C line width to 1, and turn off the setting of the outline color.
C
        CALL PCSETI ('FN - FONT NUMBER',25)
        CALL PCSETI ('OF - OUTLINE FLAG',1)
        CALL PCSETR ('OL - OUTLINE LINE WIDTH',1.)
        CALL PCSETR ('OC - OUTLINE LINE COLOR',-1.)
C
C Make TDPACK characters a bit bigger.
C
        CALL TDSETR ('CS1',1.25)
C
C Initialize the count of triangles in the triangle list.
C
        NTRI=0
C
C Add to the triangle list triangles representing a simple surface;
C initially, use rendering style 1; later, we will examine all the
C triangles and make the rendering style for each a function of W.
C
        CALL TDSTRI (U,IDIM,V,JDIM,S,IMAX,RTRI,MTRI,NTRI,1)
C
        IF (NTRI.EQ.MTRI) THEN
          PRINT * , 'TRIANGLE LIST OVERFLOW IN TDSTRI'
          STOP
        END IF
C
C Cut all the triangles generated so far into pieces, using planes that
C cut the data box into NLYR slices perpendicular to the vertical axis.
C
        ULYR=(UMAX-UMIN)/REAL(NLYR)
        VLYR=(VMAX-VMIN)/REAL(NLYR)
        WLYR=(WMAX-WMIN)/REAL(NLYR)
C
        DO 108 I=1,NLYR-1
          CALL TDCTRI (RTRI,MTRI,NTRI,3,WMIN+REAL(I)*WLYR)
  108   CONTINUE
C
C Now, add to the triangle list triangles representing an isosurface;
C use rendering style NLYR+1.
C
        CALL TDITRI (U,IDIM,V,JDIM,W,KDIM,F,IMAX,JMAX,1.,
     +                             RTRI,MTRI,NTRI,NLYR+1)
C
        IF (NTRI.EQ.MTRI) THEN
          PRINT * , 'TRIANGLE LIST OVERFLOW IN TDITRI'
          STOP
        END IF
C
C Put some more slices through the triangles, so that we can put some
C cyan stripes on the surfaces for certain values of U, V, and W.
C
        IF (USMN.LT.USMX) THEN
          CALL TDCTRI (RTRI,MTRI,NTRI,1,USMN)
          CALL TDCTRI (RTRI,MTRI,NTRI,1,USMX)
        END IF
C
        IF (VSMN.LT.VSMX) THEN
          CALL TDCTRI (RTRI,MTRI,NTRI,2,VSMN)
          CALL TDCTRI (RTRI,MTRI,NTRI,2,VSMX)
        END IF
C
        IF (WSMN.LT.WSMX) THEN
          CALL TDCTRI (RTRI,MTRI,NTRI,3,WSMN)
          CALL TDCTRI (RTRI,MTRI,NTRI,3,WSMX)
        END IF
C
C Now, examine the triangles; change all those that have a rendering
C style of 1 to instead have a rendering style that increases with
C the third coordinate of the triangle.  However, if the values of
C U, V, or W are in the ranges where we want a cyan stripe, we use
C rendering style NLYR+2.
C
        DO 109 I=1,NTRI
          UPOS=(RTRI(1,I)+RTRI(4,I)+RTRI(7,I))/3.
          VPOS=(RTRI(2,I)+RTRI(5,I)+RTRI(8,I))/3.
          WPOS=(RTRI(3,I)+RTRI(6,I)+RTRI(9,I))/3.
          IF (RTRI(10,I).EQ.1.) THEN
            RTRI(10,I)=REAL(MAX(1,MIN(NLYR,1+INT((WPOS-WMIN)/WLYR))))
          END IF
          IF ((UPOS.GE.USMN.AND.UPOS.LE.USMX).OR.
     +        (VPOS.GE.VSMN.AND.VPOS.LE.VSMX).OR.
     +        (WPOS.GE.WSMN.AND.WPOS.LE.WSMX)) RTRI(10,I)=REAL(NLYR+2)
  109   CONTINUE
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
  110   CALL TDINIT (UEYE,VEYE,WEYE,UMID,VMID,WMID,
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
C Order the triangles in the triangle list.
C
        CALL TDOTRI (RTRI,MTRI,NTRI,RTWK,ITWK,1)
C
        IF (NTRI.EQ.MTRI) THEN
          PRINT * , 'TRIANGLE LIST OVERFLOW IN TDOTRI'
          STOP
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
C Draw the triangles in the triangle list.
C
        CALL TDDTRI (RTRI,MTRI,NTRI,ITWK)
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
          GO TO 110
        END IF
C
C Advance the frame.
C
        CALL FRAME
C
C Close GKS.
C
        CALL GDAWK (IWKID)
        CALL GCLWK (IWKID)
        CALL GCLKS
C
C Done.
C
        STOP
C
      END
