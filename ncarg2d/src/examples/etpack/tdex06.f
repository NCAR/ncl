
        PROGRAM TDEX06
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
C This example shows how a user can directly generate the list of
C triangles defining a surface and then render the surface using
C TDPACK routines.
C
C Consider the surface defined by the following equations (for values
C of u and v between 0 and 2 pi).
C
C     R (A,U,V) = A * [ COS(U) + COS(U-V) ] + B
C   PHI (A,U,V) = V
C     Z (A,U,V) = A * [ SIN(U) + SIN(U-V) ]
C
C where R, PHI, and Z are the usual cylindrical coordinates (radius,
C angle, and height).
C
C 0 < A < 1 is the range of interest and B is any convenient value
C greater than A.
C
C Define A, B, and the sum of the two for scaling.
C
        PARAMETER (A=1.,B=3.,BMIN=-A-B,BMAX=+A+B)
C
C Define a parameter specifying the maximum size of the array required
C for the list of triangles.
C
        PARAMETER (MTRI=100000)
C
C Define values of pi, two times pi, and pi over 180.
C
        PARAMETER (PI=3.14159265358979323846,TWOPI=2.*PI,DTOR=PI/180.)
C
C Declare a local array to hold the triangle list and a couple of
C temporary variables to be used in sorting the list.
C
        DIMENSION RTRI(10,MTRI),RTWK(MTRI,2),ITWK(MTRI)
C
C Declare variables to hold labels.
C
        CHARACTER*64 XNLB,YNLB,ZNLB,XILB,YILB,ZILB
C
C Set the desired minimum and maximum values of U and V (for the grid
C over which the surface is generated).
C
        DATA UMIN,UMAX /  0. , TWOPI /
        DATA VMIN,VMAX /  0. , TWOPI /
C
C Set the desired minimum and maximum values of X, Y, and Z.
C
        DATA XMIN,XMAX / BMIN , BMAX /
        DATA YMIN,YMAX / BMIN , BMAX /
        DATA ZMIN,ZMAX / BMIN , BMAX /
C
C Set the values determining the resolution of the grid over which the
C surface is generated.  Note that, if each of IDIM and JDIM is one more
C than a multiple of 12, the assignment of rendering styles works out
C best.
C
        DATA IDIM,JDIM / 109 , 109 /
C
C Set the desired values of parameters determining the eye position.
C ANG1 is a bearing angle, ANG2 is an elevation angle, and RMUL is a
C multiplier of the length of the diagonal of the data box, specifying
C the distance from the center of the box to the eye.
C
        DATA ANG1,ANG2,RMUL / 215. , 35. , 2.9 /
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
C Set the desired value of the flag that says whether the basic color
C scheme will be white on black (IBOW=0) or black on white (IBOW=1).
C
        DATA IBOW / 1 /
C
C Set the desired value of the flag that says whether shading of the
C surfaces will be done using gray scales (ICLR=0) or colors (ICLR=1).
C
        DATA ICLR / 1 /
C
C Set the desired values of the shading parameters.  Values of SHDE
C near 0 give brighter colors and values near 1 give pastel shades.
C Values of SHDR near 0 give a narrow range of shades and values near
C 1 give a wide range of shades.
C
        DATA SHDE,SHDR / .1 , .8 /
C
C Define labels for the edges of the box.
C
        DATA XNLB / ' -4 -3 -2 -1 0 1 2 3 4 ' /
        DATA YNLB / ' -4 -3 -2 -1 0 1 2 3 4 ' /
        DATA ZNLB / ' -4 -3 -2 -1 0 1 2 3 4 ' /
C
        DATA XILB / 'X Coordinate Values' /
        DATA YILB / 'Y Coordinate Values' /
        DATA ZILB / 'Z Coordinate Values' /
C
C Define arithmetic statement functions for r, phi, and h as functions
C of U and V.
C
        RVAL(U,V)=A*(COS(U)+COS(U-V))+B
        PVAL(U,V)=V
        HVAL(U,V)=A*(SIN(U)+SIN(U-V))
C
C Define arithmetic statement functions to transform cylindrical
C coordinates into Cartesian coordinates.
C
        XVAL(R,P,H)=R*COS(P)
        YVAL(R,P,H)=R*SIN(P)
        ZVAL(R,P,H)=H
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
C Define colors to use.
C
        CALL TDCLRS (1,IBOW,SHDE,SHDR,11,26,8)
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
C Define TDPACK rendering styles 1 through 7, using black-and-white
C shading or colored shading, whichever is selected.  The indices
C 1-7 can then be used as final arguments in calls to TDITRI, TDSTRI,
C and TDMTRI.
C
        IF (ICLR.EQ.0) THEN
C
C Rendering styles 1-7 are all gray on both sides:
C
          CALL TDSTRS (1,27,42, 27, 42,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (2,27,42, 27, 42,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (3,27,42, 27, 42,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (4,27,42, 27, 42,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (5,27,42, 27, 42,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (6,27,42, 27, 42,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (7,27,42, 27, 42,-1,-1,1,0.,0.,0.)
C
        ELSE
C
C Rendering styles 1-7 are all gray on one side.  The other side is
C 1) gray, 2) red, 3) green, 4) blue, 5) cyan, 6) magenta, 7) yellow.
C
          CALL TDSTRS (1,27,42, 27, 42,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (2,27,42, 43, 58,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (3,27,42, 59, 74,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (4,27,42, 75, 90,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (5,27,42, 91,106,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (6,27,42,107,122,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (7,27,42,123,138,-1,-1,1,0.,0.,0.)
C
        END IF
C
C Initialize the count of triangles in the triangle list.
C
        NTRI=0
C
C For each box on a rectangular grid in the UV plane, generate two
C triangles and add them to the triangle list.  Each triangle is
C transformed from cylindrical coordinates to Cartesian coordinates.
C The rendering style is made a function of the U coordinate; try
C uncommenting the second expression for ISRS to see what happens
C when the rendering style is made a function of the V coordinate.
C
        DO 102 I=1,IDIM-1
          UVMI=UMIN+(REAL(I-1)/REAL(IDIM-1))*(UMAX-UMIN)
          UVMA=UMIN+(REAL(I  )/REAL(IDIM-1))*(UMAX-UMIN)
          IURS=MAX(1,MIN(12,1+INT(12.*((UVMI+UVMA)/2.-UMIN)/
     +                                                    (UMAX-UMIN))))
          DO 101 J=1,JDIM-1
            VVMI=VMIN+(REAL(J-1)/REAL(JDIM-1))*(VMAX-VMIN)
            VVMA=VMIN+(REAL(J  )/REAL(JDIM-1))*(VMAX-VMIN)
            IVRS=MAX(1,MIN(12,1+INT(12.*((VVMI+VVMA)/2.-VMIN)/
     +                                                    (VMAX-VMIN))))
            ISRS=MOD(IURS-1       ,3)+2
C           ISRS=MOD(       IVRS-1,3)+2
C           ISRS=MOD(IURS-1+IVRS-1,3)+2
            RV00=RVAL(UVMI,VVMI)
            PV00=PVAL(UVMI,VVMI)
            HV00=HVAL(UVMI,VVMI)
            RV01=RVAL(UVMI,VVMA)
            PV01=PVAL(UVMI,VVMA)
            HV01=HVAL(UVMI,VVMA)
            RV10=RVAL(UVMA,VVMI)
            PV10=PVAL(UVMA,VVMI)
            HV10=HVAL(UVMA,VVMI)
            RV11=RVAL(UVMA,VVMA)
            PV11=PVAL(UVMA,VVMA)
            HV11=HVAL(UVMA,VVMA)
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI(1,NTRI)=XVAL(RV10,PV10,HV10)
              RTRI(2,NTRI)=YVAL(RV10,PV10,HV10)
              RTRI(3,NTRI)=ZVAL(RV10,PV10,HV10)
              RTRI(4,NTRI)=XVAL(RV00,PV00,HV00)
              RTRI(5,NTRI)=YVAL(RV00,PV00,HV00)
              RTRI(6,NTRI)=ZVAL(RV00,PV00,HV00)
              RTRI(7,NTRI)=XVAL(RV01,PV01,HV01)
              RTRI(8,NTRI)=YVAL(RV01,PV01,HV01)
              RTRI(9,NTRI)=ZVAL(RV01,PV01,HV01)
              RTRI(10,NTRI)=REAL(ISRS)
            END IF
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI(1,NTRI)=XVAL(RV01,PV01,HV01)
              RTRI(2,NTRI)=YVAL(RV01,PV01,HV01)
              RTRI(3,NTRI)=ZVAL(RV01,PV01,HV01)
              RTRI(4,NTRI)=XVAL(RV11,PV11,HV11)
              RTRI(5,NTRI)=YVAL(RV11,PV11,HV11)
              RTRI(6,NTRI)=ZVAL(RV11,PV11,HV11)
              RTRI(7,NTRI)=XVAL(RV10,PV10,HV10)
              RTRI(8,NTRI)=YVAL(RV10,PV10,HV10)
              RTRI(9,NTRI)=ZVAL(RV10,PV10,HV10)
              RTRI(10,NTRI)=REAL(ISRS)
            END IF
  101     CONTINUE
  102   CONTINUE
C
C Find the midpoint of the data box (to be used as the point looked at).
C
        XMID=.5*(XMIN+XMAX)
        YMID=.5*(YMIN+YMAX)
        ZMID=.5*(ZMIN+ZMAX)
C
C Determine the distance (R) from which the data box will be viewed and,
C given that, the eye position.
C
        R=RMUL*SQRT((XMAX-XMIN)**2+(YMAX-YMIN)**2+(ZMAX-ZMIN)**2)
C
        XEYE=XMID+R*COS(DTOR*ANG1)*COS(DTOR*ANG2)
        YEYE=YMID+R*SIN(DTOR*ANG1)*COS(DTOR*ANG2)
        ZEYE=ZMID+R*SIN(DTOR*ANG2)
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
  109   CALL TDINIT (XEYE,YEYE,ZEYE,XMID,YMID,ZMID,
     +                              XMID,YMID,ZMID+R,OTEP)
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
        CALL TDLBLS (XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     +               XNLB,YNLB,ZNLB,XILB,YILB,ZILB,1)
C
C Draw the sides of the box that could be hidden.
C
        CALL TDGRDS (XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     +               .1*(XMAX-XMIN),.1*(YMAX-YMIN),.1*(ZMAX-ZMIN),
     +                                                       12,1)
C
C Draw the triangles in the triangle list.
C
        CALL TDDTRI (RTRI,MTRI,NTRI,ITWK)
C
C Draw the sides of the box that could not be hidden.
C
        CALL TDGRDS (XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     +               .1*(XMAX-XMIN),.1*(YMAX-YMIN),.1*(ZMAX-ZMIN),
     +                                                       12,0)
C
C If a left-eye view has just been done, loop back for a right-eye view.
C
        IF (OTEP.LT.0.) THEN
          OTEP=-OTEP
          GO TO 109
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
