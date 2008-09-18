C
C $Id: mdqini.f,v 1.6 2008-09-18 00:42:18 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDQINI
C
C This routine is called to save all the variables that are needed for
C MDQTRA, MDQTRI, and MDQTRN to carry out the transformation that is in
C effect at the time MDQINI is called.  These routines are intended to
C make it possible to use EZMAP to do two different transformations at
C the same time.
C
C What motivates this is the desire to display a geo-referenced PNG as
C a background on a map.  Given a pixel at a particular X/Y position on
C the map, we use one set of projection parameters to find its lat/lon
C coordinates; then, using those coordinates, we use a different set of
C projection parameters to find out what pixel of the PNG (if any) is
C associated with that lat/lon position, which gives us a color for the
C pixel on the map.
C
C WARNING: The two transformations may not both be USGS transformations!
C This is because I do not, for the moment, wish to duplicate the entire
C USGS package or to put in logic to re-initialize it as necessary.  If
C this becomes an issue, I'll revisit the question later.
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
        COMMON /MAPCM8/  P,Q,R
        DOUBLE PRECISION P,Q,R
        SAVE   /MAPCM8/
C
        COMMON /MAPCMW/  CSLS,CSLT,SLTD,ISLT
        DOUBLE PRECISION CSLS,CSLT,SLTD
        INTEGER ISLT
        SAVE   /MAPCMW/
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
C Check for an uncleared prior error.
C
        IF (ICFELL('MDQINI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDQINI',2).NE.0) RETURN
        END IF
C
C Call MDQIN2 to transfer variables from the common blocks above to a
C special common block used only by the transformation routines with
C names of the form M[AD]QTR[AIN].
C
        CALL MDQIN2 (ALFA,COSO,COSR,CSLS,CSLT,DCSA,DCSB,DSNA,DSNB,
     +               DTOR,DTRH,OOPI,PLNO,  PI,PIOT,ROTA,RTDD,RTOD,
     +               SALT,SINO,SINR,SRSS,SSMO,TOPI,UCNM,UMNM,UMXM,
     +               UOFF,URNM,VCNM,VMNM,VMXM,VOFF,VRNM,UTPA,IPRF,
     +               IPRJ,IROD,ELPM)
C
C Done.
C
        RETURN
C
      END
