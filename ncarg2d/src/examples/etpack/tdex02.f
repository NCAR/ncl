
      PROGRAM TDEX02
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
C Declare the size of the triangle array.
C
        PARAMETER (MTRI=100000)
C
C Declare an array to hold the list of triangles and a couple of scratch
C arrays to be used in ordering the triangles in the list.
C
        DIMENSION RTRI(10,MTRI),RTWK(MTRI,2),ITWK(MTRI)
C
C Define the positions of the eye, the origin of the X/Y plane, and the
C so-called "third point" (which defines the plane of bilateral symmetry
C of the observer).
C
        DATA UEYE,VEYE,WEYE /  6.0 , 5.0 , 2.5 /
        DATA UORG,VORG,WORG /  0.5 , 0.5 , 0.5 /
        DATA UTHI,VTHI,WTHI /  0.5 , 0.5 , 0.9 /
C
C Set a flag that says whether to do a simple image (ISTE=0) or
C a one-frame stereo image (ISTE=-1) or a two-frame stereo image
C (ISTE=+1).
C
        DATA ISTE / -1 /
C
C WOSW is the width of each stereo window, as a fraction of the width of
C the plotter frame.
C
        DATA WOSW / .55 /
C
C Set a flag that says how to set the position of the light source:
C IPLS=0 implies a light source at the position of the observer; IPLS=1
C implies a light source at the position of the sun; IPLS=2 implies a
C light source 90 degrees of longitude away from the sun and on the
C equator.
C
        DATA IPLS / 2 /
C
C Set the default value of the flag that says whether the basic color
C scheme will be white on black (IBOW=0) or black on white (IBOW=1).
C
        DATA IBOW / 1 /
C
C Set the desired value of the flag that says whether shading of the
C surfaces will be done using gray scales (ICLR=0) or colors (ICLR=1).
C
        DATA ICLR / 1 /
C
C Define default values of the shading parameters.  Use SHDE near 0
C for brighter colors and SHDE near 1 for pastel shades.  Use SHDR
C near 0 for a small range of shades and SHDR near 1 for a full range
C of shades.
C
        DATA SHDE,SHDR / .01 , .99 /
C
C Define the latitude and longitude of the sun.
C
        DATA SLAT,SLON / 25. , 80. /
C
C Define the cone angles (smaller value first).  Uncomment one of these.
C
C       DATA CON1,CON2,CON3,CON4,CON5,CON6 / 15.,23.,28.,34.,0.,.4 /
C       DATA CON1,CON2,CON3,CON4,CON5,CON6 / 15.,23.,15.,23.,0.,.4 /
C       DATA CON1,CON2,CON3,CON4,CON5,CON6 / 65.,73.,15.,23.,0.,.4 /
        DATA CON1,CON2,CON3,CON4,CON5,CON6 / 65.,73.,15.,23.,0.,.4 /
C
C Define the distances from the center of the earth (smaller values to
C larger values, earth radius is .5).
C
        DATA DST1,DST2,DST3,DST4,DST5,DST6 / 0.8,0.9,1.25,1.34,.5,2. /
C
C Define the degrees-to-radians conversion constant.
C
        DATA DTOR / .017453292519943 /
C
C Compute the position on the surface of the globe where the sun is
C directly overhead.  (This seems to be a good position to look at.)
C
        UAIM=.5+.5*COS(.017453292519943*SLAT)*COS(.017453292519943*SLON)
        VAIM=.5+.5*COS(.017453292519943*SLAT)*SIN(.017453292519943*SLON)
        WAIM=.5+.5*SIN(.017453292519943*SLAT)
C
C Determine where the light source is.
C
        IF (IPLS.EQ.0) THEN
          CALL TDSETR ('LSU',0.)
          CALL TDSETR ('LSV',0.)
          CALL TDSETR ('LSW',0.)
        ELSE IF (IPLS.EQ.1) THEN
          CALL TDSETR ('LSU',.5+1000.*COS(DTOR*SLAT)*COS(DTOR*SLON))
          CALL TDSETR ('LSV',.5+1000.*COS(DTOR*SLAT)*SIN(DTOR*SLON))
          CALL TDSETR ('LSW',.5+1000.*SIN(DTOR*SLAT)               )
        ELSE
          QLAT=0.
          QLON=SLON-90.
          CALL TDSETR ('LSU',.5+1000.*COS(DTOR*QLAT)*COS(DTOR*QLON))
          CALL TDSETR ('LSV',.5+1000.*COS(DTOR*QLAT)*SIN(DTOR*QLON))
          CALL TDSETR ('LSW',.5+1000.*SIN(DTOR*QLAT)               )
        END IF
C
C Set the shading flag.
C
        CALL TDSETI ('SHD',1)
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
        CALL TDCLRS (1,IBOW,SHDE,SHDR,11,42,7)
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
C Define TDPACK rendering styles 1, 2, and 3, using black-and-white
C shading or colored shading, whichever is selected.
C
        IF (ICLR.EQ.0) THEN
          CALL TDSTRS (1,43,74, 43, 74,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (2,43,74, 43, 74,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (3,43,74, 43, 74,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (4,42,42, 42, 42,-1,-1,1,0.,0.,0.)
        ELSE
          CALL TDSTRS (1,43,74,107,138,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (2,43,74, 75,106,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (3,43,74,139,170,-1,-1,1,0.,0.,0.)
          CALL TDSTRS (4,42,42, 42, 42,-1,-1,1,0.,0.,0.)
        END IF
C
C Initialize the count of triangles in the triangle list.
C
        NTRI=0
C
C Generate a set of triangles representing the earth.  It would probably
C be better to generate these by subdividing the faces of an icosahedron
C and projecting the vertices of the small triangles out to the surface
C of the globe, but that code is somewhat more complicated to write.
C
        DO 103 ILON=0,358,2
          RLN1=.017453292519943*REAL(ILON  )
          RLN2=.017453292519943*REAL(ILON+2)
          DO 102 ILAT=-90,88,2
            RLT1=.017453292519943*REAL(ILAT  )
            RLT2=.017453292519943*REAL(ILAT+5)
            XCSW=.5+.5*COS(RLT1)*COS(RLN1)
            YCSW=.5+.5*COS(RLT1)*SIN(RLN1)
            ZCSW=.5+.5*SIN(RLT1)
            XCSE=.5+.5*COS(RLT1)*COS(RLN2)
            YCSE=.5+.5*COS(RLT1)*SIN(RLN2)
            ZCSE=.5+.5*SIN(RLT1)
            XCNE=.5+.5*COS(RLT2)*COS(RLN2)
            YCNE=.5+.5*COS(RLT2)*SIN(RLN2)
            ZCNE=.5+.5*SIN(RLT2)
            XCNW=.5+.5*COS(RLT2)*COS(RLN1)
            YCNW=.5+.5*COS(RLT2)*SIN(RLN1)
            ZCNW=.5+.5*SIN(RLT2)
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCSW
              RTRI( 2,NTRI)=YCSW
              RTRI( 3,NTRI)=ZCSW
              RTRI( 4,NTRI)=XCSE
              RTRI( 5,NTRI)=YCSE
              RTRI( 6,NTRI)=ZCSE
              RTRI( 7,NTRI)=XCNW
              RTRI( 8,NTRI)=YCNW
              RTRI( 9,NTRI)=ZCNW
              RTRI(10,NTRI)=1.
            END IF
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCNW
              RTRI( 2,NTRI)=YCNW
              RTRI( 3,NTRI)=ZCNW
              RTRI( 4,NTRI)=XCSE
              RTRI( 5,NTRI)=YCSE
              RTRI( 6,NTRI)=ZCSE
              RTRI( 7,NTRI)=XCNE
              RTRI( 8,NTRI)=YCNE
              RTRI( 9,NTRI)=ZCNE
              RTRI(10,NTRI)=1.
            END IF
  102     CONTINUE
  103   CONTINUE
C
C Generate a set of triangles representing the inner annulus of the
C inner grid element.
C
        NSTP=MAX(1,INT((CON2-CON1)/2.))
        RSTP=(CON2-CON1)/REAL(NSTP)
C
        DO 105 ISTP=1,NSTP
          RLTI=90.-CON1-REAL(ISTP-1)*RSTP
          RLTO=RLTI-RSTP
          RLTI=.017453292519943*RLTI
          RLTO=.017453292519943*RLTO
          DO 104 ILON=0,358,2
            RLN1=.017453292519943*REAL(ILON  +ISTP-1)
            RLN2=.017453292519943*REAL(ILON+1+ISTP-1)
            RLN3=.017453292519943*REAL(ILON+2+ISTP-1)
            RLN4=.017453292519943*REAL(ILON+3+ISTP-1)
            XCP1=COS(RLTI)*COS(RLN1)
            YCP1=COS(RLTI)*SIN(RLN1)
            ZCP1=SIN(RLTI)
            CALL NGRITD (2,90.-SLAT,XCP1,YCP1,ZCP1)
            CALL NGRITD (3,    SLON,XCP1,YCP1,ZCP1)
            XCP1=.5+DST1*XCP1
            YCP1=.5+DST1*YCP1
            ZCP1=.5+DST1*ZCP1
            XCP2=COS(RLTO)*COS(RLN2)
            YCP2=COS(RLTO)*SIN(RLN2)
            ZCP2=SIN(RLTO)
            CALL NGRITD (2,90.-SLAT,XCP2,YCP2,ZCP2)
            CALL NGRITD (3,    SLON,XCP2,YCP2,ZCP2)
            XCP2=.5+DST1*XCP2
            YCP2=.5+DST1*YCP2
            ZCP2=.5+DST1*ZCP2
            XCP3=COS(RLTI)*COS(RLN3)
            YCP3=COS(RLTI)*SIN(RLN3)
            ZCP3=SIN(RLTI)
            CALL NGRITD (2,90.-SLAT,XCP3,YCP3,ZCP3)
            CALL NGRITD (3,    SLON,XCP3,YCP3,ZCP3)
            XCP3=.5+DST1*XCP3
            YCP3=.5+DST1*YCP3
            ZCP3=.5+DST1*ZCP3
            XCP4=COS(RLTO)*COS(RLN4)
            YCP4=COS(RLTO)*SIN(RLN4)
            ZCP4=SIN(RLTO)
            CALL NGRITD (2,90.-SLAT,XCP4,YCP4,ZCP4)
            CALL NGRITD (3,    SLON,XCP4,YCP4,ZCP4)
            XCP4=.5+DST1*XCP4
            YCP4=.5+DST1*YCP4
            ZCP4=.5+DST1*ZCP4
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP1
              RTRI( 2,NTRI)=YCP1
              RTRI( 3,NTRI)=ZCP1
              RTRI( 4,NTRI)=XCP3
              RTRI( 5,NTRI)=YCP3
              RTRI( 6,NTRI)=ZCP3
              RTRI( 7,NTRI)=XCP2
              RTRI( 8,NTRI)=YCP2
              RTRI( 9,NTRI)=ZCP2
              RTRI(10,NTRI)=2.
            END IF
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP3
              RTRI( 2,NTRI)=YCP3
              RTRI( 3,NTRI)=ZCP3
              RTRI( 4,NTRI)=XCP4
              RTRI( 5,NTRI)=YCP4
              RTRI( 6,NTRI)=ZCP4
              RTRI( 7,NTRI)=XCP2
              RTRI( 8,NTRI)=YCP2
              RTRI( 9,NTRI)=ZCP2
              RTRI(10,NTRI)=2.
            END IF
  104     CONTINUE
  105   CONTINUE
C
C Generate a set of triangles representing the outer annulus of the
C inner grid element.
C
        NSTP=MAX(1,INT((CON2-CON1)/2.))
        RSTP=(CON2-CON1)/REAL(NSTP)
C
        DO 107 ISTP=1,NSTP
          RLTI=90.-CON1-REAL(ISTP-1)*RSTP
          RLTO=RLTI-RSTP
          RLTI=.017453292519943*RLTI
          RLTO=.017453292519943*RLTO
          DO 106 ILON=0,358,2
            RLN1=.017453292519943*REAL(ILON  +ISTP-1)
            RLN2=.017453292519943*REAL(ILON+1+ISTP-1)
            RLN3=.017453292519943*REAL(ILON+2+ISTP-1)
            RLN4=.017453292519943*REAL(ILON+3+ISTP-1)
            XCP1=COS(RLTI)*COS(RLN1)
            YCP1=COS(RLTI)*SIN(RLN1)
            ZCP1=SIN(RLTI)
            CALL NGRITD (2,90.-SLAT,XCP1,YCP1,ZCP1)
            CALL NGRITD (3,    SLON,XCP1,YCP1,ZCP1)
            XCP1=.5+DST2*XCP1
            YCP1=.5+DST2*YCP1
            ZCP1=.5+DST2*ZCP1
            XCP2=COS(RLTO)*COS(RLN2)
            YCP2=COS(RLTO)*SIN(RLN2)
            ZCP2=SIN(RLTO)
            CALL NGRITD (2,90.-SLAT,XCP2,YCP2,ZCP2)
            CALL NGRITD (3,    SLON,XCP2,YCP2,ZCP2)
            XCP2=.5+DST2*XCP2
            YCP2=.5+DST2*YCP2
            ZCP2=.5+DST2*ZCP2
            XCP3=COS(RLTI)*COS(RLN3)
            YCP3=COS(RLTI)*SIN(RLN3)
            ZCP3=SIN(RLTI)
            CALL NGRITD (2,90.-SLAT,XCP3,YCP3,ZCP3)
            CALL NGRITD (3,    SLON,XCP3,YCP3,ZCP3)
            XCP3=.5+DST2*XCP3
            YCP3=.5+DST2*YCP3
            ZCP3=.5+DST2*ZCP3
            XCP4=COS(RLTO)*COS(RLN4)
            YCP4=COS(RLTO)*SIN(RLN4)
            ZCP4=SIN(RLTO)
            CALL NGRITD (2,90.-SLAT,XCP4,YCP4,ZCP4)
            CALL NGRITD (3,    SLON,XCP4,YCP4,ZCP4)
            XCP4=.5+DST2*XCP4
            YCP4=.5+DST2*YCP4
            ZCP4=.5+DST2*ZCP4
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP1
              RTRI( 2,NTRI)=YCP1
              RTRI( 3,NTRI)=ZCP1
              RTRI( 4,NTRI)=XCP2
              RTRI( 5,NTRI)=YCP2
              RTRI( 6,NTRI)=ZCP2
              RTRI( 7,NTRI)=XCP3
              RTRI( 8,NTRI)=YCP3
              RTRI( 9,NTRI)=ZCP3
              RTRI(10,NTRI)=2.
            END IF
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP3
              RTRI( 2,NTRI)=YCP3
              RTRI( 3,NTRI)=ZCP3
              RTRI( 4,NTRI)=XCP2
              RTRI( 5,NTRI)=YCP2
              RTRI( 6,NTRI)=ZCP2
              RTRI( 7,NTRI)=XCP4
              RTRI( 8,NTRI)=YCP4
              RTRI( 9,NTRI)=ZCP4
              RTRI(10,NTRI)=2.
            END IF
  106     CONTINUE
  107   CONTINUE
C
C Generate a set of triangles representing the inner cone surface of
C the inner grid element.
C
        RLAT=.017453292519943*(90.-CON1)
C
        NSTP=MAX(1,INT((DST2-DST1)/.02))
        RSTP=(DST2-DST1)/REAL(NSTP)
C
        DO 109 ISTP=1,NSTP
          DMIN=DST1+REAL(ISTP-1)*RSTP
          DMAX=DMIN+RSTP
          DO 108 ILON=0,358,2
            RLN1=.017453292519943*REAL(ILON  +ISTP-1)
            RLN2=.017453292519943*REAL(ILON+1+ISTP-1)
            RLN3=.017453292519943*REAL(ILON+2+ISTP-1)
            RLN4=.017453292519943*REAL(ILON+3+ISTP-1)
            XCP1=COS(RLAT)*COS(RLN1)
            YCP1=COS(RLAT)*SIN(RLN1)
            ZCP1=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP1,YCP1,ZCP1)
            CALL NGRITD (3,    SLON,XCP1,YCP1,ZCP1)
            XCP1=.5+DMIN*XCP1
            YCP1=.5+DMIN*YCP1
            ZCP1=.5+DMIN*ZCP1
            XCP2=COS(RLAT)*COS(RLN2)
            YCP2=COS(RLAT)*SIN(RLN2)
            ZCP2=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP2,YCP2,ZCP2)
            CALL NGRITD (3,    SLON,XCP2,YCP2,ZCP2)
            XCP2=.5+DMAX*XCP2
            YCP2=.5+DMAX*YCP2
            ZCP2=.5+DMAX*ZCP2
            XCP3=COS(RLAT)*COS(RLN3)
            YCP3=COS(RLAT)*SIN(RLN3)
            ZCP3=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP3,YCP3,ZCP3)
            CALL NGRITD (3,    SLON,XCP3,YCP3,ZCP3)
            XCP3=.5+DMIN*XCP3
            YCP3=.5+DMIN*YCP3
            ZCP3=.5+DMIN*ZCP3
            XCP4=COS(RLAT)*COS(RLN4)
            YCP4=COS(RLAT)*SIN(RLN4)
            ZCP4=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP4,YCP4,ZCP4)
            CALL NGRITD (3,    SLON,XCP4,YCP4,ZCP4)
            XCP4=.5+DMAX*XCP4
            YCP4=.5+DMAX*YCP4
            ZCP4=.5+DMAX*ZCP4
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP1
              RTRI( 2,NTRI)=YCP1
              RTRI( 3,NTRI)=ZCP1
              RTRI( 4,NTRI)=XCP2
              RTRI( 5,NTRI)=YCP2
              RTRI( 6,NTRI)=ZCP2
              RTRI( 7,NTRI)=XCP3
              RTRI( 8,NTRI)=YCP3
              RTRI( 9,NTRI)=ZCP3
              RTRI(10,NTRI)=2.
            END IF
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP3
              RTRI( 2,NTRI)=YCP3
              RTRI( 3,NTRI)=ZCP3
              RTRI( 4,NTRI)=XCP2
              RTRI( 5,NTRI)=YCP2
              RTRI( 6,NTRI)=ZCP2
              RTRI( 7,NTRI)=XCP4
              RTRI( 8,NTRI)=YCP4
              RTRI( 9,NTRI)=ZCP4
              RTRI(10,NTRI)=2.
            END IF
  108     CONTINUE
  109   CONTINUE
C
C Generate a set of triangles representing the outer cone surface of
C the inner grid element.
C
        RLAT=.017453292519943*(90.-CON2)
C
        NSTP=MAX(1,INT((DST2-DST1)/.02))
        RSTP=(DST2-DST1)/REAL(NSTP)
C
        DO 111 ISTP=1,NSTP
          DMIN=DST1+REAL(ISTP-1)*RSTP
          DMAX=DMIN+RSTP
          DO 110 ILON=0,358,2
            RLN1=.017453292519943*REAL(ILON  +ISTP-1)
            RLN2=.017453292519943*REAL(ILON+1+ISTP-1)
            RLN3=.017453292519943*REAL(ILON+2+ISTP-1)
            RLN4=.017453292519943*REAL(ILON+3+ISTP-1)
            XCP1=COS(RLAT)*COS(RLN1)
            YCP1=COS(RLAT)*SIN(RLN1)
            ZCP1=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP1,YCP1,ZCP1)
            CALL NGRITD (3,    SLON,XCP1,YCP1,ZCP1)
            XCP1=.5+DMIN*XCP1
            YCP1=.5+DMIN*YCP1
            ZCP1=.5+DMIN*ZCP1
            XCP2=COS(RLAT)*COS(RLN2)
            YCP2=COS(RLAT)*SIN(RLN2)
            ZCP2=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP2,YCP2,ZCP2)
            CALL NGRITD (3,    SLON,XCP2,YCP2,ZCP2)
            XCP2=.5+DMAX*XCP2
            YCP2=.5+DMAX*YCP2
            ZCP2=.5+DMAX*ZCP2
            XCP3=COS(RLAT)*COS(RLN3)
            YCP3=COS(RLAT)*SIN(RLN3)
            ZCP3=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP3,YCP3,ZCP3)
            CALL NGRITD (3,    SLON,XCP3,YCP3,ZCP3)
            XCP3=.5+DMIN*XCP3
            YCP3=.5+DMIN*YCP3
            ZCP3=.5+DMIN*ZCP3
            XCP4=COS(RLAT)*COS(RLN4)
            YCP4=COS(RLAT)*SIN(RLN4)
            ZCP4=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP4,YCP4,ZCP4)
            CALL NGRITD (3,    SLON,XCP4,YCP4,ZCP4)
            XCP4=.5+DMAX*XCP4
            YCP4=.5+DMAX*YCP4
            ZCP4=.5+DMAX*ZCP4
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP1
              RTRI( 2,NTRI)=YCP1
              RTRI( 3,NTRI)=ZCP1
              RTRI( 4,NTRI)=XCP3
              RTRI( 5,NTRI)=YCP3
              RTRI( 6,NTRI)=ZCP3
              RTRI( 7,NTRI)=XCP2
              RTRI( 8,NTRI)=YCP2
              RTRI( 9,NTRI)=ZCP2
              RTRI(10,NTRI)=2.
            END IF
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP3
              RTRI( 2,NTRI)=YCP3
              RTRI( 3,NTRI)=ZCP3
              RTRI( 4,NTRI)=XCP4
              RTRI( 5,NTRI)=YCP4
              RTRI( 6,NTRI)=ZCP4
              RTRI( 7,NTRI)=XCP2
              RTRI( 8,NTRI)=YCP2
              RTRI( 9,NTRI)=ZCP2
              RTRI(10,NTRI)=2.
            END IF
  110     CONTINUE
  111   CONTINUE
C
C Generate a set of triangles representing the inner annulus of the
C outer grid element.
C
        NSTP=MAX(1,INT((CON4-CON3)/2.))
        RSTP=(CON4-CON3)/REAL(NSTP)
C
        DO 113 ISTP=1,NSTP
          RLTI=90.-CON3-REAL(ISTP-1)*RSTP
          RLTO=RLTI-RSTP
          RLTI=.017453292519943*RLTI
          RLTO=.017453292519943*RLTO
          DO 112 ILON=0,358,2
            RLN1=.017453292519943*REAL(ILON  +ISTP-1)
            RLN2=.017453292519943*REAL(ILON+1+ISTP-1)
            RLN3=.017453292519943*REAL(ILON+2+ISTP-1)
            RLN4=.017453292519943*REAL(ILON+3+ISTP-1)
            XCP1=COS(RLTI)*COS(RLN1)
            YCP1=COS(RLTI)*SIN(RLN1)
            ZCP1=SIN(RLTI)
            CALL NGRITD (2,90.-SLAT,XCP1,YCP1,ZCP1)
            CALL NGRITD (3,    SLON,XCP1,YCP1,ZCP1)
            XCP1=.5+DST3*XCP1
            YCP1=.5+DST3*YCP1
            ZCP1=.5+DST3*ZCP1
            XCP2=COS(RLTO)*COS(RLN2)
            YCP2=COS(RLTO)*SIN(RLN2)
            ZCP2=SIN(RLTO)
            CALL NGRITD (2,90.-SLAT,XCP2,YCP2,ZCP2)
            CALL NGRITD (3,    SLON,XCP2,YCP2,ZCP2)
            XCP2=.5+DST3*XCP2
            YCP2=.5+DST3*YCP2
            ZCP2=.5+DST3*ZCP2
            XCP3=COS(RLTI)*COS(RLN3)
            YCP3=COS(RLTI)*SIN(RLN3)
            ZCP3=SIN(RLTI)
            CALL NGRITD (2,90.-SLAT,XCP3,YCP3,ZCP3)
            CALL NGRITD (3,    SLON,XCP3,YCP3,ZCP3)
            XCP3=.5+DST3*XCP3
            YCP3=.5+DST3*YCP3
            ZCP3=.5+DST3*ZCP3
            XCP4=COS(RLTO)*COS(RLN4)
            YCP4=COS(RLTO)*SIN(RLN4)
            ZCP4=SIN(RLTO)
            CALL NGRITD (2,90.-SLAT,XCP4,YCP4,ZCP4)
            CALL NGRITD (3,    SLON,XCP4,YCP4,ZCP4)
            XCP4=.5+DST3*XCP4
            YCP4=.5+DST3*YCP4
            ZCP4=.5+DST3*ZCP4
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP1
              RTRI( 2,NTRI)=YCP1
              RTRI( 3,NTRI)=ZCP1
              RTRI( 4,NTRI)=XCP3
              RTRI( 5,NTRI)=YCP3
              RTRI( 6,NTRI)=ZCP3
              RTRI( 7,NTRI)=XCP2
              RTRI( 8,NTRI)=YCP2
              RTRI( 9,NTRI)=ZCP2
              RTRI(10,NTRI)=3.
            END IF
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP3
              RTRI( 2,NTRI)=YCP3
              RTRI( 3,NTRI)=ZCP3
              RTRI( 4,NTRI)=XCP4
              RTRI( 5,NTRI)=YCP4
              RTRI( 6,NTRI)=ZCP4
              RTRI( 7,NTRI)=XCP2
              RTRI( 8,NTRI)=YCP2
              RTRI( 9,NTRI)=ZCP2
              RTRI(10,NTRI)=3.
            END IF
  112     CONTINUE
  113   CONTINUE
C
C Generate a set of triangles representing the outer annulus of the
C outer grid element.
C
        NSTP=MAX(1,INT((CON4-CON3)/2.))
        RSTP=(CON4-CON3)/REAL(NSTP)
C
        DO 115 ISTP=1,NSTP
          RLTI=90.-CON3-REAL(ISTP-1)*RSTP
          RLTO=RLTI-RSTP
          RLTI=.017453292519943*RLTI
          RLTO=.017453292519943*RLTO
          DO 114 ILON=0,358,2
            RLN1=.017453292519943*REAL(ILON  +ISTP-1)
            RLN2=.017453292519943*REAL(ILON+1+ISTP-1)
            RLN3=.017453292519943*REAL(ILON+2+ISTP-1)
            RLN4=.017453292519943*REAL(ILON+3+ISTP-1)
            XCP1=COS(RLTI)*COS(RLN1)
            YCP1=COS(RLTI)*SIN(RLN1)
            ZCP1=SIN(RLTI)
            CALL NGRITD (2,90.-SLAT,XCP1,YCP1,ZCP1)
            CALL NGRITD (3,    SLON,XCP1,YCP1,ZCP1)
            XCP1=.5+DST4*XCP1
            YCP1=.5+DST4*YCP1
            ZCP1=.5+DST4*ZCP1
            XCP2=COS(RLTO)*COS(RLN2)
            YCP2=COS(RLTO)*SIN(RLN2)
            ZCP2=SIN(RLTO)
            CALL NGRITD (2,90.-SLAT,XCP2,YCP2,ZCP2)
            CALL NGRITD (3,    SLON,XCP2,YCP2,ZCP2)
            XCP2=.5+DST4*XCP2
            YCP2=.5+DST4*YCP2
            ZCP2=.5+DST4*ZCP2
            XCP3=COS(RLTI)*COS(RLN3)
            YCP3=COS(RLTI)*SIN(RLN3)
            ZCP3=SIN(RLTI)
            CALL NGRITD (2,90.-SLAT,XCP3,YCP3,ZCP3)
            CALL NGRITD (3,    SLON,XCP3,YCP3,ZCP3)
            XCP3=.5+DST4*XCP3
            YCP3=.5+DST4*YCP3
            ZCP3=.5+DST4*ZCP3
            XCP4=COS(RLTO)*COS(RLN4)
            YCP4=COS(RLTO)*SIN(RLN4)
            ZCP4=SIN(RLTO)
            CALL NGRITD (2,90.-SLAT,XCP4,YCP4,ZCP4)
            CALL NGRITD (3,    SLON,XCP4,YCP4,ZCP4)
            XCP4=.5+DST4*XCP4
            YCP4=.5+DST4*YCP4
            ZCP4=.5+DST4*ZCP4
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP1
              RTRI( 2,NTRI)=YCP1
              RTRI( 3,NTRI)=ZCP1
              RTRI( 4,NTRI)=XCP2
              RTRI( 5,NTRI)=YCP2
              RTRI( 6,NTRI)=ZCP2
              RTRI( 7,NTRI)=XCP3
              RTRI( 8,NTRI)=YCP3
              RTRI( 9,NTRI)=ZCP3
              RTRI(10,NTRI)=3.
            END IF
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP3
              RTRI( 2,NTRI)=YCP3
              RTRI( 3,NTRI)=ZCP3
              RTRI( 4,NTRI)=XCP2
              RTRI( 5,NTRI)=YCP2
              RTRI( 6,NTRI)=ZCP2
              RTRI( 7,NTRI)=XCP4
              RTRI( 8,NTRI)=YCP4
              RTRI( 9,NTRI)=ZCP4
              RTRI(10,NTRI)=3.
            END IF
  114     CONTINUE
  115   CONTINUE
C
C Generate a set of triangles representing the inner cone surface of
C the outer grid element.
C
        RLAT=.017453292519943*(90.-CON3)
C
        NSTP=MAX(1,INT((DST4-DST3)/.02))
        RSTP=(DST4-DST3)/REAL(NSTP)
C
        DO 117 ISTP=1,NSTP
          DMIN=DST3+REAL(ISTP-1)*RSTP
          DMAX=DMIN+RSTP
          DO 116 ILON=0,358,2
            RLN1=.017453292519943*REAL(ILON  +ISTP-1)
            RLN2=.017453292519943*REAL(ILON+1+ISTP-1)
            RLN3=.017453292519943*REAL(ILON+2+ISTP-1)
            RLN4=.017453292519943*REAL(ILON+3+ISTP-1)
            XCP1=COS(RLAT)*COS(RLN1)
            YCP1=COS(RLAT)*SIN(RLN1)
            ZCP1=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP1,YCP1,ZCP1)
            CALL NGRITD (3,    SLON,XCP1,YCP1,ZCP1)
            XCP1=.5+DMIN*XCP1
            YCP1=.5+DMIN*YCP1
            ZCP1=.5+DMIN*ZCP1
            XCP2=COS(RLAT)*COS(RLN2)
            YCP2=COS(RLAT)*SIN(RLN2)
            ZCP2=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP2,YCP2,ZCP2)
            CALL NGRITD (3,    SLON,XCP2,YCP2,ZCP2)
            XCP2=.5+DMAX*XCP2
            YCP2=.5+DMAX*YCP2
            ZCP2=.5+DMAX*ZCP2
            XCP3=COS(RLAT)*COS(RLN3)
            YCP3=COS(RLAT)*SIN(RLN3)
            ZCP3=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP3,YCP3,ZCP3)
            CALL NGRITD (3,    SLON,XCP3,YCP3,ZCP3)
            XCP3=.5+DMIN*XCP3
            YCP3=.5+DMIN*YCP3
            ZCP3=.5+DMIN*ZCP3
            XCP4=COS(RLAT)*COS(RLN4)
            YCP4=COS(RLAT)*SIN(RLN4)
            ZCP4=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP4,YCP4,ZCP4)
            CALL NGRITD (3,    SLON,XCP4,YCP4,ZCP4)
            XCP4=.5+DMAX*XCP4
            YCP4=.5+DMAX*YCP4
            ZCP4=.5+DMAX*ZCP4
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP1
              RTRI( 2,NTRI)=YCP1
              RTRI( 3,NTRI)=ZCP1
              RTRI( 4,NTRI)=XCP2
              RTRI( 5,NTRI)=YCP2
              RTRI( 6,NTRI)=ZCP2
              RTRI( 7,NTRI)=XCP3
              RTRI( 8,NTRI)=YCP3
              RTRI( 9,NTRI)=ZCP3
              RTRI(10,NTRI)=3.
            END IF
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP3
              RTRI( 2,NTRI)=YCP3
              RTRI( 3,NTRI)=ZCP3
              RTRI( 4,NTRI)=XCP2
              RTRI( 5,NTRI)=YCP2
              RTRI( 6,NTRI)=ZCP2
              RTRI( 7,NTRI)=XCP4
              RTRI( 8,NTRI)=YCP4
              RTRI( 9,NTRI)=ZCP4
              RTRI(10,NTRI)=3.
            END IF
  116     CONTINUE
  117   CONTINUE
C
C Generate a set of triangles representing the outer cone surface of
C the outer grid element.
C
        RLAT=.017453292519943*(90.-CON4)
C
        NSTP=MAX(1,INT((DST4-DST3)/.02))
        RSTP=(DST4-DST3)/REAL(NSTP)
C
        DO 119 ISTP=1,NSTP
          DMIN=DST3+REAL(ISTP-1)*RSTP
          DMAX=DMIN+RSTP
          DO 118 ILON=0,358,2
            RLN1=.017453292519943*REAL(ILON  +ISTP-1)
            RLN2=.017453292519943*REAL(ILON+1+ISTP-1)
            RLN3=.017453292519943*REAL(ILON+2+ISTP-1)
            RLN4=.017453292519943*REAL(ILON+3+ISTP-1)
            XCP1=COS(RLAT)*COS(RLN1)
            YCP1=COS(RLAT)*SIN(RLN1)
            ZCP1=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP1,YCP1,ZCP1)
            CALL NGRITD (3,    SLON,XCP1,YCP1,ZCP1)
            XCP1=.5+DMIN*XCP1
            YCP1=.5+DMIN*YCP1
            ZCP1=.5+DMIN*ZCP1
            XCP2=COS(RLAT)*COS(RLN2)
            YCP2=COS(RLAT)*SIN(RLN2)
            ZCP2=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP2,YCP2,ZCP2)
            CALL NGRITD (3,    SLON,XCP2,YCP2,ZCP2)
            XCP2=.5+DMAX*XCP2
            YCP2=.5+DMAX*YCP2
            ZCP2=.5+DMAX*ZCP2
            XCP3=COS(RLAT)*COS(RLN3)
            YCP3=COS(RLAT)*SIN(RLN3)
            ZCP3=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP3,YCP3,ZCP3)
            CALL NGRITD (3,    SLON,XCP3,YCP3,ZCP3)
            XCP3=.5+DMIN*XCP3
            YCP3=.5+DMIN*YCP3
            ZCP3=.5+DMIN*ZCP3
            XCP4=COS(RLAT)*COS(RLN4)
            YCP4=COS(RLAT)*SIN(RLN4)
            ZCP4=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP4,YCP4,ZCP4)
            CALL NGRITD (3,    SLON,XCP4,YCP4,ZCP4)
            XCP4=.5+DMAX*XCP4
            YCP4=.5+DMAX*YCP4
            ZCP4=.5+DMAX*ZCP4
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP1
              RTRI( 2,NTRI)=YCP1
              RTRI( 3,NTRI)=ZCP1
              RTRI( 4,NTRI)=XCP3
              RTRI( 5,NTRI)=YCP3
              RTRI( 6,NTRI)=ZCP3
              RTRI( 7,NTRI)=XCP2
              RTRI( 8,NTRI)=YCP2
              RTRI( 9,NTRI)=ZCP2
              RTRI(10,NTRI)=3.
            END IF
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP3
              RTRI( 2,NTRI)=YCP3
              RTRI( 3,NTRI)=ZCP3
              RTRI( 4,NTRI)=XCP4
              RTRI( 5,NTRI)=YCP4
              RTRI( 6,NTRI)=ZCP4
              RTRI( 7,NTRI)=XCP2
              RTRI( 8,NTRI)=YCP2
              RTRI( 9,NTRI)=ZCP2
              RTRI(10,NTRI)=3.
            END IF
  118     CONTINUE
  119   CONTINUE
C
C Generate a set of triangles representing the outer cone surface of
C the outer grid element.
C
        RLAT=.017453292519943*(90.-CON6)
C
        NSTP=MAX(1,INT((DST6-DST5)/.02))
        RSTP=(DST6-DST5)/REAL(NSTP)
C
        DO 121 ISTP=1,NSTP
          DMIN=DST5+REAL(ISTP-1)*RSTP
          DMAX=DMIN+RSTP
          DO 120 ILON=0,358,2
            RLN1=.017453292519943*REAL(ILON  +ISTP-1)
            RLN2=.017453292519943*REAL(ILON+1+ISTP-1)
            RLN3=.017453292519943*REAL(ILON+2+ISTP-1)
            RLN4=.017453292519943*REAL(ILON+3+ISTP-1)
            XCP1=COS(RLAT)*COS(RLN1)
            YCP1=COS(RLAT)*SIN(RLN1)
            ZCP1=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP1,YCP1,ZCP1)
            CALL NGRITD (3,    SLON,XCP1,YCP1,ZCP1)
            XCP1=.5+DMIN*XCP1
            YCP1=.5+DMIN*YCP1
            ZCP1=.5+DMIN*ZCP1
            XCP2=COS(RLAT)*COS(RLN2)
            YCP2=COS(RLAT)*SIN(RLN2)
            ZCP2=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP2,YCP2,ZCP2)
            CALL NGRITD (3,    SLON,XCP2,YCP2,ZCP2)
            XCP2=.5+DMAX*XCP2
            YCP2=.5+DMAX*YCP2
            ZCP2=.5+DMAX*ZCP2
            XCP3=COS(RLAT)*COS(RLN3)
            YCP3=COS(RLAT)*SIN(RLN3)
            ZCP3=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP3,YCP3,ZCP3)
            CALL NGRITD (3,    SLON,XCP3,YCP3,ZCP3)
            XCP3=.5+DMIN*XCP3
            YCP3=.5+DMIN*YCP3
            ZCP3=.5+DMIN*ZCP3
            XCP4=COS(RLAT)*COS(RLN4)
            YCP4=COS(RLAT)*SIN(RLN4)
            ZCP4=SIN(RLAT)
            CALL NGRITD (2,90.-SLAT,XCP4,YCP4,ZCP4)
            CALL NGRITD (3,    SLON,XCP4,YCP4,ZCP4)
            XCP4=.5+DMAX*XCP4
            YCP4=.5+DMAX*YCP4
            ZCP4=.5+DMAX*ZCP4
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP1
              RTRI( 2,NTRI)=YCP1
              RTRI( 3,NTRI)=ZCP1
              RTRI( 4,NTRI)=XCP3
              RTRI( 5,NTRI)=YCP3
              RTRI( 6,NTRI)=ZCP3
              RTRI( 7,NTRI)=XCP2
              RTRI( 8,NTRI)=YCP2
              RTRI( 9,NTRI)=ZCP2
              RTRI(10,NTRI)=4.
            END IF
            IF (NTRI.LT.MTRI) THEN
              NTRI=NTRI+1
              RTRI( 1,NTRI)=XCP3
              RTRI( 2,NTRI)=YCP3
              RTRI( 3,NTRI)=ZCP3
              RTRI( 4,NTRI)=XCP4
              RTRI( 5,NTRI)=YCP4
              RTRI( 6,NTRI)=ZCP4
              RTRI( 7,NTRI)=XCP2
              RTRI( 8,NTRI)=YCP2
              RTRI( 9,NTRI)=ZCP2
              RTRI(10,NTRI)=4.
            END IF
  120     CONTINUE
  121   CONTINUE
C
        PRINT * , 'NUMBER OF TRIANGLES USED = ',NTRI
C
C Initialize the eye position to do a left-eye view.
C
        IF (ISTE.EQ.0) THEN
          OTEP=0.
        ELSE
          OTEP=-.4
        END IF
C
C Initialize TDPACK.
C
  199   CALL TDINIT (UEYE,VEYE,WEYE,UAIM,VAIM,WAIM,
     +                              UAIM,VAIM,WAIM+1.,OTEP)
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
        CALL TDOTRI (RTRI,MTRI,NTRI,RTWK,ITWK,0)
C
C Render the triangles in the triangle list.
C
        CALL TDDTRI (RTRI,MTRI,NTRI,ITWK)
C
C If a left-eye view has just been done, loop back for a right-eye view.
C
        IF (OTEP.LT.0.) THEN
          OTEP=-OTEP
          GO TO 199
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
