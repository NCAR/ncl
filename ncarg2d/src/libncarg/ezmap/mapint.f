C
C $Id: mapint.f,v 1.6 1994-03-18 23:50:11 kennison Exp $
C
      SUBROUTINE MAPINT
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM1/ IPRJ,PHOC,IROD,RSNO,RCSO,RSNR,RCSR
      SAVE /MAPCM1/
      COMMON /MAPDP1/ DSNO,DCSO,DSNR,DCSR
      DOUBLE PRECISION DSNO,DCSO,DSNR,DCSR
      SAVE /MAPDP1/
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM,ISSL
      SAVE /MAPCM2/
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,SLOG,
     +                PNTS(200),IDOS(4)
      SAVE /MAPCM3/
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE /MAPCM4/
      COMMON /MAPCM6/ ELPM,UMNM,UMXM,VMNM,VMXM,UCNM,VCNM,URNM,VRNM
      LOGICAL ELPM
      SAVE /MAPCM6/
      COMMON /MAPCM7/ ULOW,UROW,VBOW,VTOW
      SAVE /MAPCM7/
      COMMON /MAPCMA/ DPLT,DDTS,DSCA,DPSQ,DSSQ,DBTD,DATL
      SAVE /MAPCMA/
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
      COMMON /MAPSAT/ SALT,SSMO,SRSS,ALFA,BETA,RSNA,RCSA,RSNB,RCSB
      SAVE /MAPSAT/
      COMMON /MAPDPS/ DSNA,DCSA,DSNB,DCSB
      DOUBLE PRECISION DSNA,DCSA,DSNB,DCSB
      SAVE /MAPDPS/
C
C Some temporary values have to be double precision.
C
      DOUBLE PRECISION COST,SINT,TMP1,TMP2
C
C Set up alternate names for some of the variables in common.
C
      EQUIVALENCE     (PHIA,FLT1),(ROTA,FLT2)
C
      EQUIVALENCE     (PLA1,AUMN),(PLA2,AUMX),
     +                (PLA3,AVMN),(PLA4,AVMX)
C
C Ensure that the block data routine will load, so that variables will
C have the proper default values.
C
      EXTERNAL MAPBD
C
C Define the necessary constants.
C
      DATA RESL / 10. /
      DATA DTOR / .017453292519943 /
      DATA OV90 / .011111111111111 /
      DATA PI   / 3.14159265358979 /
      DATA RTOD / 57.2957795130823 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPINT - UNCLEARED PRIOR ERROR',1).NE.0) THEN
        IIER=-1
        RETURN
      END IF
C
C Decide whether MAPTRN should use real or double-precision arithmetic.
C The subroutine call is necessary to fool some compilers into storing
C TST1, TST2, and TST3; otherwise, real precision may be used on
C machines on which double precision is necessary.
C
      CALL MAPIN2 (TST1,TST2,TST3)
C
      IF (TST1.NE.TST2.AND.TST2.NE.TST3) THEN
        IROD=0
      ELSE
        IROD=1
      END IF
C
C Check for an error in the projection specifier.
C
      IF (JPRJ.LE.0.OR.JPRJ.GE.10) GO TO 901
C
C IPRJ equals JPRJ until we find out if fast-path projections are to be
C used.  PHOC is just a copy of PHIO.
C
      IPRJ=JPRJ
      PHOC=PHIO
C
      IF (IPRJ.EQ.1) THEN
C
C Compute constants for the Lambert conformal conic.
C
        RSNO=SIGN(1.,.5*(FLT1+FLT2))
        DSNO=DBLE(RSNO)
        CHI1=(90.-RSNO*FLT1)*DTOR
        IF (FLT1.EQ.FLT2) THEN
          DCSO=COS(DBLE(CHI1))
          RCSO=REAL(DCSO)
        ELSE
          CHI2=(90.-RSNO*FLT2)*DTOR
          DCSO=LOG(SIN(DBLE(CHI1))/SIN(DBLE(CHI2)))/
     +         LOG(TAN(.5D0*DBLE(CHI1))/TAN(.5D0*DBLE(CHI2)))
          RCSO=REAL(DCSO)
        END IF
C
      ELSE
C
C Compute constants required for all the other projections.
C
        TMP1=DBLE(ROTA*DTOR)
        TMP2=DBLE(PHIA*DTOR)
        DSNR=SIN(TMP1)
        RSNR=REAL(DSNR)
        DCSR=COS(TMP1)
        RCSR=REAL(DCSR)
        DSNO=SIN(TMP2)
        RSNO=REAL(DSNO)
        DCSO=COS(TMP2)
        RCSO=REAL(DCSO)
C
C Compute constants required only by the cylindrical projections.
C
        IF (IPRJ.GE.7) THEN
C
C See if fast-path transformations can be used.  (PLAT = 0 and ROTA = 0
C or 180.)
C
          IF (ABS(PHIA).GE..0001.OR.(ABS(ROTA).GE..0001.AND.
     +                               ABS(ROTA).LE.179.9999)) THEN
C
C No.  Compute constants for the ordinary cylindrical projections.
C
            SINT=DCSO*DCSR
            COST=SQRT(1.D0-SINT**2)
            TMP1=DSNR/COST
            TMP2=DSNO/COST
            PHIO=PHIO-REAL(ATAN2(TMP1,-DCSR*TMP2))*RTOD
            PHOC=PHIO
            DSNR=TMP1*DCSO
            RSNR=REAL(DSNR)
            DCSR=-TMP2
            RCSR=REAL(DCSR)
            DSNO=SINT
            RSNO=REAL(DSNO)
            DCSO=COST
            RCSO=REAL(DCSO)
C
          ELSE
C
C Yes.  The fast paths are implemented as three additional projections.
C
            IPRJ=IPRJ+3
C
            IF (ABS(ROTA).LT..0001) THEN
              DSNO=1.D0
              RSNO=1.
            ELSE
              DSNO=-1.D0
              RSNO=-1.
              PHIO=PHIO+180.
              PHOC=PHIO
            END IF
C
            DCSO=0.D0
            RCSO=0.
            DSNR=0.D0
            RSNR=0.
            DCSR=1.D0
            RCSR=1.
C
          END IF
C
        END IF
C
      END IF
C
C Now, set UMIN, UMAX, VMIN, and VMAX to correspond to the maximum
C useful area produced by the projection.
C
      GO TO (101,102,101,102,102,103,104,103,105,104,103,105) , IPRJ
C
C Lambert conformal conic and orthographic.  The quantity "R" which is
C used below is the largest accceptable ratio of the lengths of the
C major and minor axes of the elliptical limb of the satellite-view
C projection for which we will show the entire ellipse.  For larger
C values of the angle "alpha", we just use a "camera" with a 120-degree
C field of view; it is debatable whether there is any better course of
C action.
C
  101 IF (IPRJ.NE.3.OR.ABS(SALT).LE.1..OR.ALFA.EQ.0.) THEN
        UMIN=-1.
        UMAX=1.
        VMIN=-1.
        VMAX=1.
      ELSE
        R=4.
        IF (ALFA.LT.
     +      RTOD*ACOS(SQRT((SALT*SALT+R*R-1.)/(R*R*SALT*SALT)))) THEN
          SASQ=SALT*SALT
          A=(1.-RSNA*RSNA*RCSB*RCSB)*SASQ-1.
          B=(1.-RSNA*RSNA*RSNB*RSNB)*SASQ-1.
          C=-2.*SASQ*RSNA*RSNA*RSNB*RCSB
          D= 2.*SASQ*SRSS*RSNA*RCSA*RCSB
          E= 2.*SASQ*SRSS*RSNA*RCSA*RSNB
          F=SSMO*(SSMO*RSNA*RSNA-RCSA*RCSA)
          TEM1=SQRT((2.*C*E-4.*B*D)**2-4.*(C*C-4.*A*B)*(E*E-4.*B*F))
          TEM2=(4.*B*D-2.*C*E+TEM1)/(2.*(C*C-4.*A*B))
          TEM3=(4.*B*D-2.*C*E-TEM1)/(2.*(C*C-4.*A*B))
          UMIN=MIN(TEM2,TEM3)
          UMAX=MAX(TEM2,TEM3)
          TEM1=SQRT((2.*C*D-4.*A*E)**2-4.*(C*C-4.*A*B)*(D*D-4.*A*F))
          TEM2=(4.*A*E-2.*C*D+TEM1)/(2.*(C*C-4.*A*B))
          TEM3=(4.*A*E-2.*C*D-TEM1)/(2.*(C*C-4.*A*B))
          VMIN=MIN(TEM2,TEM3)
          VMAX=MAX(TEM2,TEM3)
        ELSE
          UMIN=-SRSS*TAN(DTOR*60.)
          UMAX=+SRSS*TAN(DTOR*60.)
          VMIN=-SRSS*TAN(DTOR*60.)
          VMAX=+SRSS*TAN(DTOR*60.)
        END IF
      END IF
C
      GO TO 106
C
C Stereographic, Lambert equal area, and Gnomonic.
C
  102 UMIN=-2.
      UMAX=2.
      VMIN=-2.
      VMAX=2.
      GO TO 106
C
C Azimuthal equidistant and Mercator.
C
  103 UMIN=-PI
      UMAX=PI
      VMIN=-PI
      VMAX=PI
      GO TO 106
C
C Cylindrical equidistant.
C
  104 UMIN=-180.
      UMAX=180.
      VMIN=-90.
      VMAX=90.
      GO TO 106
C
C Mollweide.
C
  105 UMIN=-2.
      UMAX=2.
      VMIN=-1.
      VMAX=1.
C
C Compute the quantities used by MAPIT in checking for cross-over.
C
  106 UEPS=.75*(UMAX-UMIN)
      VEPS=.75*(VMAX-VMIN)
C
C As always, the conical projection is the oddball.  Cross-over is not
C detected in u and v, but in longitude, so the value has to be set
C differently.
C
      IF (IPRJ.EQ.1) UEPS=180.
C
C Now, jump to the appropriate limit-setting code.
C
      GO TO (600,200,300,400,500) , ILTS
C
C ILTS=2    Points (PL1,PL2) and (PL3,PL4) are on opposite corners
C ------    of the plot.
C
  200 E=0.
  201 CALL MAPTRN (PLA1,PLA2+E,TEM1,TEM3)
      IF (ICFELL('MAPINT',2).NE.0) RETURN
      CALL MAPTRN (PLA3,PLA4-E,TEM2,TEM4)
      IF (ICFELL('MAPINT',3).NE.0) RETURN
      IF (IPRJ.GE.7.AND.TEM1.GE.TEM2.AND.E.EQ.0.) THEN
        E=.0001
        GO TO 201
      END IF
      UMIN=MIN(TEM1,TEM2)
      UMAX=MAX(TEM1,TEM2)
      VMIN=MIN(TEM3,TEM4)
      VMAX=MAX(TEM3,TEM4)
      IF (UMAX.GE.1.E12) GO TO 904
      GO TO 600
C
C ILTS=3    Four edge points are given.
C ------
C
  300 E=0.
  301 CALL MAPTRN (PLA1,PLB1+E,TEM1,TEM5)
      IF (ICFELL('MAPINT',4).NE.0) RETURN
      CALL MAPTRN (PLA2,PLB2-E,TEM2,TEM6)
      IF (ICFELL('MAPINT',5).NE.0) RETURN
      IF (IPRJ.GE.7.AND.TEM1.GE.TEM2.AND.E.EQ.0.) THEN
        E=.0001
        GO TO 301
      END IF
      CALL MAPTRN (PLA3,PLB3,TEM3,TEM7)
      IF (ICFELL('MAPINT',6).NE.0) RETURN
      CALL MAPTRN (PLA4,PLB4,TEM4,TEM8)
      IF (ICFELL('MAPINT',7).NE.0) RETURN
      UMIN=MIN(TEM1,TEM2,TEM3,TEM4)
      UMAX=MAX(TEM1,TEM2,TEM3,TEM4)
      VMIN=MIN(TEM5,TEM6,TEM7,TEM8)
      VMAX=MAX(TEM5,TEM6,TEM7,TEM8)
      IF (UMAX.GE.1.E12) GO TO 904
      GO TO 600
C
C ILTS=4    Angular distances are given.
C ------
C
  400 CUMI=COS(AUMN*DTOR)
      SUMI=SIN(AUMN*DTOR)
      CUMA=COS(AUMX*DTOR)
      SUMA=SIN(AUMX*DTOR)
      CVMI=COS(AVMN*DTOR)
      SVMI=SIN(AVMN*DTOR)
      CVMA=COS(AVMX*DTOR)
      SVMA=SIN(AVMX*DTOR)
C
      GO TO (904,401,402,403,404,405,406,407,408,406,407,408) , IPRJ
C
C Stereographic.
C
  401 IF (SUMI.LT..0001) THEN
        IF (CUMI.GT.0.) UMIN=0.
      ELSE
        UMIN=-(1.-CUMI)/SUMI
      END IF
      IF (SUMA.LT..0001) THEN
        IF (CUMA.GT.0.) UMAX=0.
      ELSE
        UMAX=(1.-CUMA)/SUMA
      END IF
      IF (SVMI.LT..0001) THEN
        IF (CVMI.GT.0.) VMIN=0.
      ELSE
        VMIN=-(1.-CVMI)/SVMI
      END IF
      IF (SVMA.LT..0001) THEN
        IF (CVMA.GT.0.) VMAX=0.
      ELSE
        VMAX=(1.-CVMA)/SVMA
      END IF
      GO TO 600
C
C Orthographic or satellite-view.
C
  402 IF (ABS(SALT).LE.1.) THEN
        IF (MAX(AUMN,AUMX,AVMN,AVMX).GT.90.) GO TO 902
        UMIN=-SUMI
        UMAX=SUMA
        VMIN=-SVMI
        VMAX=SVMA
      ELSE
        IF (MAX(AUMN,AUMX,AVMN,AVMX).GE.90.) GO TO 902
        UMIN=-SRSS*SUMI/CUMI
        UMAX=+SRSS*SUMA/CUMA
        VMIN=-SRSS*SVMI/CVMI
        VMAX=+SRSS*SVMA/CVMA
      END IF
      GO TO 600
C
C Lambert equal area.
C
  403 IF (SUMI.LT..0001) THEN
        IF (CUMI.GT.0.) UMIN=0.
      ELSE
        UMIN=-2./SQRT(1.+((1.+CUMI)/SUMI)**2)
      END IF
      IF (SUMA.LT..0001) THEN
        IF (CUMA.GT.0.) UMAX=0.
      ELSE
        UMAX=2./SQRT(1.+((1.+CUMA)/SUMA)**2)
      END IF
      IF (SVMI.LT..0001) THEN
        IF (CVMI.GT.0.) VMIN=0.
      ELSE
        VMIN=-2./SQRT(1.+((1.+CVMI)/SVMI)**2)
      END IF
      IF (SVMA.LT..0001) THEN
        IF (CVMA.GT.0.) VMAX=0.
      ELSE
        VMAX=2./SQRT(1.+((1.+CVMA)/SVMA)**2)
      END IF
      GO TO 600
C
C Gnomonic.
C
  404 IF (MAX(AUMN,AUMX,AVMN,AVMX).GE.89.9999) GO TO 902
      UMIN=-SUMI/CUMI
      UMAX=SUMA/CUMA
      VMIN=-SVMI/CVMI
      VMAX=SVMA/CVMA
      GO TO 600
C
C Azimuthal equidistant.
C
  405 UMIN=-AUMN*DTOR
      UMAX=AUMX*DTOR
      VMIN=-AVMN*DTOR
      VMAX=AVMX*DTOR
      GO TO 600
C
C Cylindrical equidistant.
C
  406 UMIN=-AUMN
      UMAX=AUMX
      VMIN=-AVMN
      VMAX=AVMX
      GO TO 600
C
C Mercator.
C
  407 IF (MAX(AVMN,AVMX).GE.89.9999) GO TO 902
      UMIN=-AUMN*DTOR
      UMAX=AUMX*DTOR
      VMIN=-LOG((1.+SVMI)/CVMI)
      VMAX=LOG((1.+SVMA)/CVMA)
      GO TO 600
C
C Mollweide.
C
  408 UMIN=-AUMN*OV90
      UMAX=AUMX*OV90
      VMIN=-SVMI
      VMAX=SVMA
      GO TO 600
C
C ILTS=5    Values in the u/v plane are given.
C ------
C
  500 UMIN=PLA1
      UMAX=PLA2
      VMIN=PLA3
      VMAX=PLA4
C
C Compute the width and height of the plot.
C
  600 DU=UMAX-UMIN
      DV=VMAX-VMIN
C
C Error if map has zero area.
C
      IF (DU.LE.0..OR.DV.LE.0.) GO TO 903
C
C Position the map on the plotter frame.
C
      IF (DU/DV.LT.(XROW-XLOW)/(YTOW-YBOW)) THEN
        ULOW=.5*(XLOW+XROW)-.5*(DU/DV)*(YTOW-YBOW)
        UROW=.5*(XLOW+XROW)+.5*(DU/DV)*(YTOW-YBOW)
        VBOW=YBOW
        VTOW=YTOW
      ELSE
        ULOW=XLOW
        UROW=XROW
        VBOW=.5*(YBOW+YTOW)-.5*(DV/DU)*(XROW-XLOW)
        VTOW=.5*(YBOW+YTOW)+.5*(DV/DU)*(XROW-XLOW)
      END IF
C
C Error if map has essentially zero area.
C
      IF (MIN(UROW-ULOW,VTOW-VBOW)*PLTR.LT.RESL) GO TO 903
C
C Do the required SET call.
C
      CALL SET (ULOW,UROW,VBOW,VTOW,UMIN,UMAX,VMIN,VMAX,1)
      IF (ICFELL('MAPINT',8).NE.0) THEN
        IIER=-1
        RETURN
      END IF
C
C Compute the quantities used by MAPIT to see if points are far enough
C apart to draw the line between them and the quantities used by MAPVP
C to determine the number of dots to interpolate between two points.
C
      DSCA=(UROW-ULOW)*PLTR/DU
      DPSQ=DPLT*DPLT
      DSSQ=DSCA*DSCA
      DBTD=DDTS/DSCA
C
C Set parameters required if an elliptical perimeter is being used.
C
      UCEN=.5*(UMIN+UMAX)
      VCEN=.5*(VMIN+VMAX)
      URNG=.5*(UMAX-UMIN)
      VRNG=.5*(VMAX-VMIN)
C
C Now, compute the latitude/longitude limits which will be required by
C MAPGRD and MAPLOT, if any.
C
      IF (GRID.GT.0..OR.NOUT.NE.0) THEN
C
C At first, assume the whole globe will be projected.
C
        SLAM=-90.
        BLAM=+90.
        SLOM=PHIO-180.
        BLOM=PHIO+180.
C
C Jump if it's obvious that really is the case.
C
        IF (ILTS.EQ.1.AND.(JPRJ.EQ.4.OR.JPRJ.EQ.6.OR.JPRJ.EQ.7.OR.
     +                                             JPRJ.EQ.9)) GO TO 701
C
C Otherwise, the whole globe is not being projected.  The first thing
C to do is to find a point (CLAT,CLON) whose projection is known to be
C on the map.  First, try the pole of the projection.
C
        CLAT=PHIA
        CLON=PHIO
        CALL MAPTRN (CLAT,CLON,U,V)
        IF (ICFELL('MAPINT',9).NE.0) RETURN
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.))
     +                                                         GO TO 611
C
C If that didn't work, try a point based on the limits specifier.
C
        IF (ILTS.EQ.2) THEN
          CLAT=.5*(PLA1+PLA3)
          CLON=.5*(PLA2+PLA4)
        ELSE IF (ILTS.EQ.3) THEN
          TEM1=MIN(PLA1,PLA2,PLA3,PLA4)
          TEM2=MAX(PLA1,PLA2,PLA3,PLA4)
          TEM3=MIN(PLB1,PLB2,PLB3,PLB4)
          TEM4=MAX(PLB1,PLB2,PLB3,PLB4)
          CLAT=.5*(TEM1+TEM2)
          CLON=.5*(TEM3+TEM4)
        ELSE
          GO TO 700
        END IF
        CALL MAPTRN (CLAT,CLON,U,V)
        IF (ICFELL('MAPINT',10).NE.0) RETURN
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.))
     +                                                         GO TO 611
        GO TO 700
C
C Once we have the latitudes and longitudes of a point on the map, we
C find the minimum and maximum latitude and the minimum and maximum
C longitude by running a search point about on a fine lat/lon grid.
C
C Find the minimum latitude.
C
  611   RLAT=CLAT
        RLON=CLON
        DLON=SRCH
  612   RLAT=RLAT-SRCH
        IF (RLAT.LE.-90.) GO TO 621
  613   CALL MAPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MAPINT',11).NE.0) RETURN
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.)) THEN
          DLON=SRCH
          GO TO 612
        END IF
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180..AND.RLON.LT.CLON+180.) GO TO 613
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180..AND.RLON.LT.CLON+180.) GO TO 613
        SLAM=RLAT
C
C Find the maximum latitude.
C
  621   RLAT=CLAT
        RLON=CLON
        DLON=SRCH
  622   RLAT=RLAT+SRCH
        IF (RLAT.GT.90.) GO TO 631
  623   CALL MAPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MAPINT',12).NE.0) RETURN
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.)) THEN
          DLON=SRCH
          GO TO 622
        END IF
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180..AND.RLON.LT.CLON+180.) GO TO 623
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180..AND.RLON.LT.CLON+180.) GO TO 623
        BLAM=RLAT
C
C Find the minimum longitude.
C
  631   RLAT=CLAT
        RLON=CLON
        DLAT=SRCH
  632   RLON=RLON-SRCH
        IF (RLON.LE.CLON-360.) GO TO 651
  633   CALL MAPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MAPINT',13).NE.0) RETURN
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.)) THEN
          DLAT=SRCH
          GO TO 632
        END IF
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90..AND.RLAT.LT.90.) GO TO 633
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90..AND.RLAT.LT.90.) GO TO 633
        SLOM=RLON-SIGN(180.,RLON+180.)+SIGN(180.,180.-RLON)
C
C Find the maximum longitude.
C
  641   RLAT=CLAT
        RLON=CLON
        DLAT=SRCH
  642   RLON=RLON+SRCH
        IF (RLON.GE.CLON+360.) GO TO 651
  643   CALL MAPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MAPINT',14).NE.0) RETURN
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                               .AND.V.LE.VMAX).OR.
     +      (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.)) THEN
          DLAT=SRCH
          GO TO 642
        END IF
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90..AND.RLAT.LT.90.) GO TO 643
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90..AND.RLAT.LT.90.) GO TO 643
        BLOM=RLON-SIGN(180.,RLON+180.)+SIGN(180.,180.-RLON)
        IF (BLOM.LE.SLOM) BLOM=BLOM+360.
        GO TO 701
C
  651   SLOM=PHIO-180.
        BLOM=PHIO+180.
        GO TO 701
C
      END IF
C
C Control comes here if we didn't succeed in setting limits properly.
C
  700 ISSL=0
      GO TO 702
C
C Control comes here if we did succeed in setting limits properly.
C
  701 ISSL=1
C
C Zero the error flag and turn off the initialization-required flag.
C
  702 IIER=0
      INTF=.FALSE.
C
C Set all the variables in the common block passed to MAPTRA.
C
      ELPM=ELPF
      UMNM=UMIN-.000001*(UMAX-UMIN)
      UMXM=UMAX+.000001*(UMAX-UMIN)
      VMNM=VMIN-.000001*(VMAX-VMIN)
      VMXM=VMAX+.000001*(VMAX-VMIN)
      UCNM=UCEN
      VCNM=VCEN
      URNM=URNG
      VRNM=VRNG
C
C Done.
C
      RETURN
C
C Error returns.
C
  901 IIER=5
      CALL SETER ('MAPINT - ATTEMPT TO USE NON-EXISTENT PROJECTION',
     1                                                       IIER,1)
      RETURN
C
  902 IIER=6
      CALL SETER ('MAPINT - ANGULAR LIMITS TOO GREAT',IIER,1)
      RETURN
C
  903 IIER=7
      CALL SETER ('MAPINT - MAP HAS ZERO AREA',IIER,1)
      RETURN
C
  904 IIER=8
      CALL SETER ('MAPINT - MAP LIMITS INAPPROPRIATE',IIER,1)
      RETURN
C
      END
