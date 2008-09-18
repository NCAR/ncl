C
C $Id: mdpint.f,v 1.13 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPINT
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM0/  COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        DOUBLE PRECISION COS1,DTOR,DTRH,OOPI,PI,PIOF,PIOT,RTDD,RTOD,
     +                   SROT,SIN1,TOPI,TSRT
        SAVE   /MAPCM0/
C
        COMMON /MAPCM1/  COSO,COSR,SINO,SINR,IPRJ,IROD
        DOUBLE PRECISION COSO,COSR,SINO,SINR
        INTEGER          IPRJ,IROD
        SAVE   /MAPCM1/
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PDRE,PLA1,PLA2,
     +                   PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLNO,PLTO,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPCM6/  UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM,ELPM
        DOUBLE PRECISION UCNM,UMNM,UMXM,URNM,VCNM,VMNM,VMXM,VRNM
        LOGICAL ELPM
        SAVE   /MAPCM6/
C
        COMMON /MAPCM7/  ULOW,UROW,VBOW,VTOW
        DOUBLE PRECISION ULOW,UROW,VBOW,VTOW
        SAVE   /MAPCM7/
C
        COMMON /MAPCMA/  DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        DOUBLE PRECISION DATL,DBTD,DDTS,DPLT,DPSQ,DSCA,DSSQ
        SAVE   /MAPCMA/
C
        COMMON /MAPCMW/  CSLS,CSLT,SLTD,ISLT
        DOUBLE PRECISION CSLS,CSLT,SLTD
        INTEGER ISLT
        SAVE  /MAPCMW/
C
        COMMON /MAPSAT/  ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        DOUBLE PRECISION ALFA,BETA,DCSA,DCSB,DSNA,DSNB,SALT,SSMO,SRSS
        SAVE   /MAPSAT/
C
        COMMON /USGSC1/  UTPA(15),UUMN,UUMX,UVMN,UVMX,IPRF
        DOUBLE PRECISION UTPA,UUMN,UUMX,UVMN,UVMX
        INTEGER          IPRF
        SAVE   /USGSC1/
C
C Set up alternate names for some of the variables in common.
C
        EQUIVALENCE      (PLA1,AUMN),(PLA2,AUMX),
     +                   (PLA3,AVMN),(PLA4,AVMX)
C
C Declare local variables.
C
        DOUBLE PRECISION A,AUMN,AUMX,AVMN,AVMX,B,C,CLAT,CLON,CUMA,CUMI,
     +                   CVMA,CVMI,D,DLAT,DLON,DU,DV,E,F,R,RESL,RLAT,
     +                   RLN1,RLN2,RLON,RLT1,RLT2,SASQ,SUMA,SUMI,SVMA,
     +                   SVMI,TEM1,TEM2,TEM3,TEM4,TEM5,TEM6,TEM7,TEM8,
     +                   U,UTMP,V,XMAX,XMIN,XPOS,YMAX,YMIN,YPOS
        INTEGER          I,J,NLNS,NLTS
C
C Declare function types.
C
        DOUBLE PRECISION RBGDFE
C
C Define a necessary constant.
C
        DATA RESL / 10.D0 /
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPINT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Jump to a subroutine that is responsible for pre-computing constants
C required by MDPTRN.  The same subroutine may, on occasion, need to be
C called by MDPTRN itself.  (MDPTRN may not call MDPINT because MDPINT
C calls MDPTRN.)
C
        CALL MDPIN1
C
C Zero UOFF and VOFF to turn off the offset feature while we're setting
C up the projection.
C
        UOFF=0.D0
        VOFF=0.D0
C
C Set UMIN, UMAX, VMIN, and VMAX to correspond to the maximum useful
C area produced by the projection.
C
C Projection:   US  LC  ST  OR  LE  GN  AE  
C                   CE  ME  MT  RO  EA  AI  HA  MO  WT  (arbitrary)
C                   CE  ME  MT  RO  EA  AI  HA  MO  WT  (fast-path)
C                       RM
C
        GO TO (100,101,102,101,102,102,103,
     +             104,103,105,106,107,108,109,109,110,
     +             104,103,105,106,107,108,109,109,110,
     +                 111                            ) , IPRJ+1
C
C USGS transformations.  I have some doubts about the appropriateness of
C setting UMIN, UMAX, VMIN, and VMAX in this way.
C
  100   UMIN=UUMN
        UMAX=UUMX
        VMIN=UVMN
        VMAX=UVMX
C
        GO TO 112
C
C Lambert conformal conic and orthographic.  The quantity "R" which is
C used below is the largest acceptable ratio of the lengths of the major
C and minor axes of the elliptical limb of the satellite-view projection
C for which we will show the entire ellipse.  For larger values of the
C angle "alpha", we just use a "camera" with a 120-degree field of view;
C it is debatable whether there is any better course of action.
C
  101   IF (IPRJ.NE.3.OR.ABS(SALT).LE.1.D0.OR.ALFA.EQ.0.D0) THEN
          UMIN=-1.D0
          UMAX=+1.D0
          VMIN=-1.D0
          VMAX=+1.D0
        ELSE
          R=4.D0
          IF (ALFA.LT.RTOD*
     +        ACOS(SQRT((SALT*SALT+R*R-1.D0)/(R*R*SALT*SALT)))) THEN
            SASQ=SALT*SALT
            A=(1.D0-DSNA*DSNA*DCSB*DCSB)*SASQ-1.D0
            B=(1.D0-DSNA*DSNA*DSNB*DSNB)*SASQ-1.D0
            C=-2.D0*SASQ*DSNA*DSNA*DSNB*DCSB
            D= 2.D0*SASQ*SRSS*DSNA*DCSA*DCSB
            E= 2.D0*SASQ*SRSS*DSNA*DCSA*DSNB
            F=SSMO*(SSMO*DSNA*DSNA-DCSA*DCSA)
            TEM1=SQRT((2.D0*C*E-4.D0*B*D)**2-
     +                 4.D0*(C*C-4.D0*A*B)*(E*E-4.D0*B*F))
            TEM2=(4.D0*B*D-2.D0*C*E+TEM1)/(2.D0*(C*C-4.D0*A*B))
            TEM3=(4.D0*B*D-2.D0*C*E-TEM1)/(2.D0*(C*C-4.D0*A*B))
            UMIN=MIN(TEM2,TEM3)
            UMAX=MAX(TEM2,TEM3)
            TEM1=SQRT((2.D0*C*D-4.D0*A*E)**2-
     +                 4.D0*(C*C-4.D0*A*B)*(D*D-4.D0*A*F))
            TEM2=(4.D0*A*E-2.D0*C*D+TEM1)/(2.D0*(C*C-4.D0*A*B))
            TEM3=(4.D0*A*E-2.D0*C*D-TEM1)/(2.D0*(C*C-4.D0*A*B))
            VMIN=MIN(TEM2,TEM3)
            VMAX=MAX(TEM2,TEM3)
          ELSE
            UMIN=-SRSS*TAN(DTOR*60.D0)
            UMAX=+SRSS*TAN(DTOR*60.D0)
            VMIN=-SRSS*TAN(DTOR*60.D0)
            VMAX=+SRSS*TAN(DTOR*60.D0)
          END IF
        END IF
C
        GO TO 112
C
C Stereographic, Lambert equal-area, and Gnomonic.
C
  102   UMIN=-2.D0
        UMAX=+2.D0
        VMIN=-2.D0
        VMAX=+2.D0
        GO TO 112
C
C Azimuthal equidistant and Mercator.
C
  103   UMIN=-PI
        UMAX=+PI
        VMIN=-PI
        VMAX=+PI
        GO TO 112
C
C Cylindrical equidistant.
C
  104   UMIN=-180.D0
        UMAX=+180.D0
        VMIN= -90.D0/CSLT
        VMAX= +90.D0/CSLT
        GO TO 112
C
C Mollweide-type.
C
  105   UMIN=-2.D0
        UMAX=+2.D0
        VMIN=-1.D0
        VMAX=+1.D0
        GO TO 112
C
C Robinson.
C
  106   UMIN=-1.0000D0
        UMAX=+1.0000D0
        VMIN= -.5072D0
        VMAX= +.5072D0
        GO TO 112
C
C Cylindrical equal-area.
C
  107   UMIN=-PI
        UMAX=+PI
        VMIN=-1./CSLS
        VMAX=+1./CSLS
        GO TO 112
C
C Aitoff.
C
  108   UMIN=-PI
        UMAX=+PI
        VMIN=-PIOT
        VMAX=+PIOT
        GO TO 112
C
C Hammer and true Mollweide.
C
  109   UMIN=-TSRT
        UMAX=+TSRT
        VMIN=-SROT
        VMAX=+SROT
        GO TO 112
C
C Winkel tripel.
C
  110   UMIN=-PIOT*(1.D0+CSLT)
        UMAX=+PIOT*(1.D0+CSLT)
        VMIN=-PIOT
        VMAX=+PIOT
        GO TO 112
C
C Rotated Mercator.
C
  111   UMIN=-PI/(ABS(SINR)+ABS(COSR))
        UMAX=+PI/(ABS(SINR)+ABS(COSR))
        VMIN=-PI/(ABS(SINR)+ABS(COSR))
        VMAX=+PI/(ABS(SINR)+ABS(COSR))
        GO TO 112
C
C Compute the quantity used by MAPIT in checking for crossover.  The
C USGS and conical projections are oddballs.
C
  112   IF (IPRJ.EQ.0) THEN
          IF (IPRF.EQ. 3.OR.IPRF.EQ. 4.OR.IPRF.EQ. 5.OR.IPRF.EQ. 7.OR.
     +        IPRF.EQ. 8.OR.IPRF.EQ.16.OR.IPRF.EQ.17.OR.IPRF.EQ.18.OR.
     +        IPRF.EQ.19.OR.IPRF.EQ.21) THEN
            PEPS=270.D0
          ELSE IF (IPRF.EQ.9) THEN
            PEPS=.75D0*(VMAX-VMIN)
          ELSE IF (IPRF.EQ.20) THEN
            PEPS=.75D0*(UMAX-UMIN)
          ELSE IF (IPRF.EQ.22) THEN
            PEPS=.25D0*(UMAX-UMIN)
          ELSE
            PEPS=.75D0*(UMAX-UMIN)
          END IF
        ELSE IF (IPRJ.EQ.1) THEN
          PEPS=270.D0
        ELSE
          PEPS=.75D0*(UMAX-UMIN)
        END IF
C
C Turn off the initialization-required flag; this is done here, rather
C than at the end, because, otherwise, each of the following calls to
C MDPTRN will result in an unnecessary call to MDPIN1.
C
        INTF=.FALSE.
C
C Jump to the appropriate limit-setting code.
C
        GO TO (600,200,300,400,500,550) , ILTS
C
C ILTS=2    Points (PL1,PL2) and (PL3,PL4) are on opposite corners
C ------    of the plot.
C
  200   E=0.D0
  201   CALL MDPTRN (PLA1,PLA2+E,TEM1,TEM3)
        IF (ICFELL('MDPINT',2).NE.0) GO TO 999
        CALL MDPTRN (PLA3,PLA4-E,TEM2,TEM4)
        IF (ICFELL('MDPINT',3).NE.0) GO TO 999
        IF (IPRJ.GE.7.AND.TEM1.GE.TEM2.AND.E.EQ.0.D0) THEN
          E=.000001D0
          GO TO 201
        END IF
        UMIN=MIN(TEM1,TEM2)
        UMAX=MAX(TEM1,TEM2)
        VMIN=MIN(TEM3,TEM4)
        VMAX=MAX(TEM3,TEM4)
        IF (UMAX.GE.1.D12) GO TO 903
        GO TO 600
C
C ILTS=3    Four edge points are given.
C ------
C
  300   E=0.D0
  301   CALL MDPTRN (PLA1,PLB1+E,TEM1,TEM5)
        IF (ICFELL('MDPINT',4).NE.0) GO TO 999
        CALL MDPTRN (PLA2,PLB2-E,TEM2,TEM6)
        IF (ICFELL('MDPINT',5).NE.0) GO TO 999
        IF (IPRJ.GE.7.AND.TEM1.GE.TEM2.AND.E.EQ.0.D0) THEN
          E=.000001D0
          GO TO 301
        END IF
        CALL MDPTRN (PLA3,PLB3,TEM3,TEM7)
        IF (ICFELL('MDPINT',6).NE.0) GO TO 999
        CALL MDPTRN (PLA4,PLB4,TEM4,TEM8)
        IF (ICFELL('MDPINT',7).NE.0) GO TO 999
        UMIN=MIN(TEM1,TEM2,TEM3,TEM4)
        UMAX=MAX(TEM1,TEM2,TEM3,TEM4)
        VMIN=MIN(TEM5,TEM6,TEM7,TEM8)
        VMAX=MAX(TEM5,TEM6,TEM7,TEM8)
        IF (UMAX.GE.1.D12) GO TO 903
        GO TO 600
C
C ILTS=4    Angular distances are given.
C ------
C
  400   CUMI=COS(AUMN*DTOR)
        SUMI=SIN(AUMN*DTOR)
        CUMA=COS(AUMX*DTOR)
        SUMA=SIN(AUMX*DTOR)
        CVMI=COS(AVMN*DTOR)
        SVMI=SIN(AVMN*DTOR)
        CVMA=COS(AVMX*DTOR)
        SVMA=SIN(AVMX*DTOR)
C
C Projection:   US  LC  ST  OR  LE  GN  AE
C                   CE  ME  MT  RO  EA  AI  HA  MO  WT  (arbitrary)
C                   CE  ME  MT  RO  EA  AI  HA  MO  WT  (fast-path)
C                       RM
C
        GO TO (401,903,402,403,404,405,406,
     +             407,408,409,410,411,412,413,414,415,
     +             407,408,409,410,411,412,413,414,415,
     +                 416                            ) , IPRJ+1
C
C USGS transformations.  I have some doubts about the appropriateness of
C setting UMIN, UMAX, VMIN, and VMAX in this way.
C
  401   UMIN=UUMN
        UMAX=UUMX
        VMIN=UVMN
        VMAX=UVMX
C
        GO TO 600
C
C Stereographic.
C
  402   IF (SUMI.LT..000001D0) THEN
          IF (CUMI.GT.0.D0) UMIN=0.D0
        ELSE
          UMIN=-(1.D0-CUMI)/SUMI
        END IF
        IF (SUMA.LT..000001D0) THEN
          IF (CUMA.GT.0.D0) UMAX=0.D0
        ELSE
          UMAX=(1.D0-CUMA)/SUMA
        END IF
        IF (SVMI.LT..000001D0) THEN
          IF (CVMI.GT.0.D0) VMIN=0.D0
        ELSE
          VMIN=-(1.D0-CVMI)/SVMI
        END IF
        IF (SVMA.LT..000001D0) THEN
          IF (CVMA.GT.0.D0) VMAX=0.D0
        ELSE
          VMAX=(1.D0-CVMA)/SVMA
        END IF
        GO TO 600
C
C Orthographic or satellite-view.
C
  403   IF (ABS(SALT).LE.1.D0) THEN
          IF (MAX(AUMN,AUMX,AVMN,AVMX).GT.90.D0) GO TO 901
          UMIN=-SUMI
          UMAX=+SUMA
          VMIN=-SVMI
          VMAX=+SVMA
        ELSE
          IF (MAX(AUMN,AUMX,AVMN,AVMX).GE.90.D0) GO TO 901
          UMIN=-SRSS*SUMI/CUMI
          UMAX=+SRSS*SUMA/CUMA
          VMIN=-SRSS*SVMI/CVMI
          VMAX=+SRSS*SVMA/CVMA
        END IF
        GO TO 600
C
C Lambert equal area.
C
  404   IF (SUMI.LT..000001D0) THEN
          IF (CUMI.GT.0.D0) UMIN=0.D0
        ELSE
          UMIN=-2.D0/SQRT(1.D0+((1.D0+CUMI)/SUMI)**2)
        END IF
        IF (SUMA.LT..000001D0) THEN
          IF (CUMA.GT.0.D0) UMAX=0.D0
        ELSE
          UMAX=2.D0/SQRT(1.D0+((1.D0+CUMA)/SUMA)**2)
        END IF
        IF (SVMI.LT..000001D0) THEN
          IF (CVMI.GT.0.D0) VMIN=0.D0
        ELSE
          VMIN=-2.D0/SQRT(1.D0+((1.D0+CVMI)/SVMI)**2)
        END IF
        IF (SVMA.LT..000001D0) THEN
          IF (CVMA.GT.0.D0) VMAX=0.D0
        ELSE
          VMAX=2.D0/SQRT(1.D0+((1.D0+CVMA)/SVMA)**2)
        END IF
        GO TO 600
C
C Gnomonic.
C
  405   IF (MAX(AUMN,AUMX,AVMN,AVMX).GE.89.999999D0) GO TO 901
        UMIN=-SUMI/CUMI
        UMAX=+SUMA/CUMA
        VMIN=-SVMI/CVMI
        VMAX=+SVMA/CVMA
        GO TO 600
C
C Azimuthal equidistant.
C
  406   UMIN=-AUMN*DTOR
        UMAX=+AUMX*DTOR
        VMIN=-AVMN*DTOR
        VMAX=+AVMX*DTOR
        GO TO 600
C
C Cylindrical equidistant.
C
  407   UMIN=-AUMN
        UMAX=+AUMX
        VMIN=-AVMN/CSLT
        VMAX=+AVMX/CSLT
        GO TO 600
C
C Mercator.
C
  408   IF (MAX(AVMN,AVMX).GE.89.999999D0) GO TO 901
        UMIN=-AUMN*DTOR
        UMAX=+AUMX*DTOR
        VMIN=-LOG((1.D0+SVMI)/CVMI)
        VMAX=+LOG((1.D0+SVMA)/CVMA)
        GO TO 600
C
C Mollweide-type.
C
  409   UMIN=-AUMN/90.D0
        UMAX=+AUMX/90.D0
        VMIN=-SVMI
        VMAX=+SVMA
        GO TO 600
C
C Robinson.
C
  410   UMIN=-AUMN/180.D0
        UMAX=+AUMX/180.D0
        VMIN=-RBGDFE(MAX(-90.D0,MIN(+90.D0,AVMN)))
        VMAX=+RBGDFE(MAX(-90.D0,MIN(+90.D0,AVMX)))
        GO TO 600
C
C Cylindrical equal-area.
C
  411   UMIN=-AUMN*DTOR
        UMAX=+AUMX*DTOR
        VMIN=-SVMI/CSLS
        VMAX=+SVMA/CSLS
        GO TO 600
C
C Aitoff.
C
  412   UMIN=-AUMN*DTOR
        UMAX=+AUMX*DTOR
        VMIN=-AVMN*DTOR
        VMAX=+AVMX*DTOR
        GO TO 600
C
C Hammer.
C
  413   UMIN=-TSRT*SIN(DTOR*AUMN/2.D0)/SQRT(1.D0+COS(DTOR*AUMN/2.D0))
        UMAX=+TSRT*SIN(DTOR*AUMX/2.D0)/SQRT(1.D0+COS(DTOR*AUMX/2.D0))
        VMIN=-SROT*SVMI/SQRT(1.D0+CVMI)
        VMAX=+SROT*SVMA/SQRT(1.D0+CVMA)
        GO TO 600
C
C True Mollweide.
C
  414   UMIN=-SROT*AUMN/90.D0
        UMAX=+SROT*AUMX/90.D0
        CALL MOPROJ (-DTOR*AVMN,0.D0,UTMP,VMIN)
        CALL MOPROJ (+DTOR*AVMX,0.D0,UTMP,VMAX)
        GO TO 600
C
C Winkel tripel.
C
  415   UMIN=-(AUMN/2.D0)*DTOR*(1.D0+CSLT)
        UMAX=+(AUMX/2.D0)*DTOR*(1.D0+CSLT)
        VMIN=-AVMN*DTOR
        VMAX=+AVMX*DTOR
        GO TO 600
C
C Rotated Mercator.  The following code treats angular limit-setting as
C equivalent to maximal limit-setting.  This is because I'm not sure how
C to make angular limit-setting for this projection work.
C
  416   GO TO 600
C
C ILTS=5    Values in the u/v plane are given.
C ------
C
  500   UMIN=PLA1
        UMAX=PLA2
        VMIN=PLA3
        VMAX=PLA4
        GO TO 600
C
C ILTS=6    Ranges of latitudes and longitudes are given, defining a
C ------    region of interest, all of which is to be shown.
C
  550   RLT1=MAX(-90.D0,MIN(90.D0,PLA1))
        RLT2=MAX(-90.D0,MIN(90.D0,PLA3))
        IF (RLT1.GT.RLT2) THEN
          RLAT=RLT1
          RLT1=RLT2
          RLT2=RLAT
        END IF
        RLN1=MOD(PLA2+3600.D0,360.D0)
        RLN2=MOD(PLA4+3600.D0,360.D0)
        IF (RLN2.LE.RLN1) RLN2=RLN2+360.D0
        NLTS=MAX(5,INT((RLT2-RLT1)/GRDR))+2
        NLNS=MAX(5,INT((RLN2-RLN1)/GRDR))+2
        XMIN=+1.D12
        XMAX=-1.D12
        YMIN=+1.D12
        YMAX=-1.D12
        DO 552 J=1,NLTS
          RLAT=RLT1+DBLE(J-1)*(RLT2-RLT1)/DBLE(NLTS-1)
          DO 551 I=1,NLNS
            RLON=RLN1+DBLE(I-1)*(RLN2-RLN1)/DBLE(NLNS-1)
            CALL MDPTRN (RLAT,RLON,XPOS,YPOS)
            IF (XPOS.NE.1.D12) THEN
              XMIN=MIN(XMIN,XPOS)
              XMAX=MAX(XMAX,XPOS)
              YMIN=MIN(YMIN,YPOS)
              YMAX=MAX(YMAX,YPOS)
            END IF
  551     CONTINUE
  552   CONTINUE
        IF (XMIN.NE.1.D12) THEN
          UMIN=XMIN
          UMAX=XMAX
          VMIN=YMIN
          VMAX=YMAX
        END IF
        GO TO 600
C
C Compute the width and height of the plot.
C
  600   DU=UMAX-UMIN
        DV=VMAX-VMIN
C
C Error if map has zero area.
C
        IF (DU.LE.0.D0.OR.DV.LE.0.D0) GO TO 902
C
C Position the map on the plotter frame.
C
        IF (DU/DV.LT.(XROW-XLOW)/(YTOW-YBOW)) THEN
          ULOW=.5D0*(XLOW+XROW)-.5D0*(DU/DV)*(YTOW-YBOW)
          UROW=.5D0*(XLOW+XROW)+.5D0*(DU/DV)*(YTOW-YBOW)
          VBOW=YBOW
          VTOW=YTOW
        ELSE
          ULOW=XLOW
          UROW=XROW
          VBOW=.5D0*(YBOW+YTOW)-.5D0*(DV/DU)*(XROW-XLOW)
          VTOW=.5D0*(YBOW+YTOW)+.5D0*(DV/DU)*(XROW-XLOW)
        END IF
C
C Error if map has essentially zero area.
C
        IF (MIN(UROW-ULOW,VTOW-VBOW)*PDRE.LT.RESL) GO TO 902
C
C Compute the quantities used by MAPIT to see if points are far enough
C apart to draw the line between them and the quantities used by MDPVP
C to determine the number of dots to interpolate between two points.
C
        DSCA=(UROW-ULOW)*PDRE/DU
        DPSQ=DPLT*DPLT
        DSSQ=DSCA*DSCA
        DBTD=DDTS/DSCA
C
C Set parameters required if an elliptical perimeter is being used.
C
        UCEN=.5D0*(UMIN+UMAX)
        VCEN=.5D0*(VMIN+VMAX)
        URNG=.5D0*(UMAX-UMIN)
        VRNG=.5D0*(VMAX-VMIN)
C
C Now, compute the latitude/longitude limits which will be required by
C various other routines.
C
C At first, assume the whole globe will be projected.
C
        SLAM=-90.D0
        BLAM=+90.D0
        SLOM=PLNO-180.D0
        BLOM=PLNO+180.D0
C
C Jump if it's obvious that really is the case.  It is possible that
C something else needs to be done here.
C
        IF (IPRJ.EQ.0) GO TO 701
C
        IF (ILTS.EQ.1.AND.(IPRJ.EQ.4.OR.IPRJ.EQ.6.OR.IPRJ.EQ.7.OR.
     +       (IPRJ.GE.9.AND.JPRJ.LE.16).OR.(IPRJ.GE.18.AND.IPRJ.LE.24)))
     +                                                         GO TO 701
C
C Otherwise, the whole globe is not being projected.  The first thing
C to do is to find a point (CLAT,CLON) whose projection is known to be
C on the map.  First, try the pole of the projection.
C
        CLAT=PLTO
        CLON=PLNO
        CALL MDPTRN (CLAT,CLON,U,V)
        IF (ICFELL('MDPINT',8).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +        (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.D0))
     +                                                         GO TO 611
C
C If that didn't work, try a point based on the limits specifier.
C
        IF (ILTS.EQ.2.OR.ILTS.EQ.6) THEN
          CLAT=.5D0*(PLA1+PLA3)
          CLON=.5D0*(PLA2+PLA4)
        ELSE IF (ILTS.EQ.3) THEN
          TEM1=MIN(PLA1,PLA2,PLA3,PLA4)
          TEM2=MAX(PLA1,PLA2,PLA3,PLA4)
          TEM3=MIN(PLB1,PLB2,PLB3,PLB4)
          TEM4=MAX(PLB1,PLB2,PLB3,PLB4)
          CLAT=.5D0*(TEM1+TEM2)
          CLON=.5D0*(TEM3+TEM4)
        ELSE
          GO TO 700
        END IF
        CALL MDPTRN (CLAT,CLON,U,V)
        IF (ICFELL('MDPINT',9).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +        (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.D0))
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
        IF (RLAT.LE.-90.D0) GO TO 621
  613   CALL MDPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MDPINT',10).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +    (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.D0)) THEN
          DLON=SRCH
          GO TO 612
        END IF
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180.D0.AND.RLON.LT.CLON+180.D0) GO TO 613
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180.D0.AND.RLON.LT.CLON+180.D0) GO TO 613
        SLAM=RLAT
C
C Find the maximum latitude.
C
  621   RLAT=CLAT
        RLON=CLON
        DLON=SRCH
  622   RLAT=RLAT+SRCH
        IF (RLAT.GT.90.D0) GO TO 631
  623   CALL MDPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MDPINT',11).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +    (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.D0)) THEN
          DLON=SRCH
          GO TO 622
        END IF
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180.D0.AND.RLON.LT.CLON+180.D0) GO TO 623
        RLON=RLON+DLON
        DLON=SIGN(ABS(DLON)+SRCH,-DLON)
        IF (RLON.GT.CLON-180.D0.AND.RLON.LT.CLON+180.D0) GO TO 623
        BLAM=RLAT
C
C Find the minimum longitude.
C
  631   RLAT=CLAT
        RLON=CLON
        DLAT=SRCH
  632   RLON=RLON-SRCH
        IF (RLON.LE.CLON-360.D0) GO TO 651
  633   CALL MDPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MDPINT',12).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +    (ELPF.AND.((U-UCEN)/URNG)**2+((V-VCEN)/VRNG)**2.LE.1.D0)) THEN
          DLAT=SRCH
          GO TO 632
        END IF
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90.D0.AND.RLAT.LT.90.D0) GO TO 633
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90.D0.AND.RLAT.LT.90.D0) GO TO 633
        SLOM=RLON+(SIGN(180.D0,180.D0-RLON)-SIGN(180.D0,RLON+180.D0))
C
C Find the maximum longitude.
C
  641   RLAT=CLAT
        RLON=CLON
        DLAT=SRCH
  642   RLON=RLON+SRCH
        IF (RLON.GE.CLON+360.D0) GO TO 651
  643   CALL MDPTRN (RLAT,RLON,U,V)
        IF (ICFELL('MDPINT',13).NE.0) GO TO 999
        IF ((.NOT.ELPF.AND.U.GE.UMIN.AND.U.LE.UMAX.AND.V.GE.VMIN
     +                                            .AND.V.LE.VMAX).OR.
     +           (ELPF.AND.((U-UCEN)/URNG)**2+
     +                      ((V-VCEN)/VRNG)**2.LE.1.D0)) THEN
          DLAT=SRCH
          GO TO 642
        END IF
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90.D0.AND.RLAT.LT.90.D0) GO TO 643
        RLAT=RLAT+DLAT
        DLAT=SIGN(ABS(DLAT)+SRCH,-DLAT)
        IF (RLAT.GT.-90.D0.AND.RLAT.LT.90.D0) GO TO 643
        BLOM=RLON+(SIGN(180.D0,180.D0-RLON)-SIGN(180.D0,RLON+180.D0))
        IF (BLOM.LE.SLOM) BLOM=BLOM+360.D0
        GO TO 701
C
  651   SLOM=PLNO-180.D0
        BLOM=PLNO+180.D0
        GO TO 701
C
C Control comes here if we didn't succeed in setting limits properly.
C
  700   ISSL=0
        GO TO 702
C
C Control comes here if we did succeed in setting limits properly.
C
  701   ISSL=1
C
C Set the offset values and reset the minimum and maximum values.
C
  702   IF (ABS(UMIN-UMAX).LT.OTOL*ABS(UMIN+UMAX).OR.
     +      ABS(VMIN-VMAX).LT.OTOL*ABS(VMIN+VMAX)) THEN
          UOFF=UMIN
          VOFF=VMIN
          UMIN=0.
          VMIN=0.
          UMAX=UMAX-UOFF
          VMAX=VMAX-VOFF
        END IF
C
C Do the required SET call, being careful to use real numbers which are
C within the ranges specified by the double-precision numbers involved.
C
        CALL SET (RDPNUW(ULOW),RDPNDW(UROW),RDPNUW(VBOW),RDPNDW(VTOW),
     +            RDPNUW(UMIN),RDPNDW(UMAX),RDPNUW(VMIN),RDPNDW(VMAX),1)
        IF (ICFELL('MDPINT',14).NE.0) GO TO 999
C
C Set all the variables in the common block passed to MDPTRA.
C
        ELPM=ELPF
        UMNM=UMIN-.000001D0*(UMAX-UMIN)
        UMXM=UMAX+.000001D0*(UMAX-UMIN)
        VMNM=VMIN-.000001D0*(VMAX-VMIN)
        VMXM=VMAX+.000001D0*(VMAX-VMIN)
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
  901   CALL SETER ('MDPINT - ANGULAR LIMITS TOO GREAT',16,1)
        GO TO 999
C
  902   CALL SETER ('MDPINT - MAP HAS ZERO AREA',17,1)
        GO TO 999
C
  903   CALL SETER ('MDPINT - MAP LIMITS INAPPROPRIATE',18,1)
        GO TO 999
C
  999   INTF=.TRUE.
        RETURN
C
      END
