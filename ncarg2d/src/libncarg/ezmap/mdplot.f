C
C $Id: mdplot.f,v 1.10 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPLOT
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
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCM3/  ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,
     +                   SLOG,PNTS(200),IDOS(4)
        INTEGER          ITPN,NOUT,NPTS,IGID,IDLS,IDRS,IDOS
        REAL             BLAG,SLAG,BLOG,SLOG,PNTS
        SAVE   /MAPCM3/
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
C Declare local variables.
C
        INTEGER          IGIS,IWGF,K,NSEG
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPLOT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDPLOT',2).NE.0) RETURN
        END IF
C
C If the selected outline type is "NONE", skip to the limb line code.
C
        IF (NOUT.LE.0) GO TO 104
C
C Set the flag IWGF to say whether or not the whole globe is shown by
C the current projection.  If so (IWGF=1), there's no need to waste the
C time required to check each outline point group for intersection with
C the window.
C
        IWGF=0
        IF (BLAM-SLAM.GT.179.D0.AND.BLOM-SLOM.GT.359.D0) IWGF=1
C
C IGIS keeps track of changes in the group identifier, so that the
C color index can be changed when necessary.
C
        IGIS=0
C
C Position to the user-selected portion of the outline dataset.
C
        CALL MDPIO (1)
        IF (ICFELL('MDPLOT',3).NE.0) RETURN
        NSEG=0
C
C Read the next record (group of points).
C
  101   CALL MDPIO (2)
        IF (ICFELL('MDPLOT',4).NE.0) RETURN
        NSEG=NSEG+1
C
C Check for the end of the desired data.
C
        IF (NPTS.EQ.0) GO TO 103
C
C If less than the whole globe is shown by the projection, do a quick
C check for intersection of the box surrounding the point group with
C the area shown.
C
        IF (IWGF.EQ.0) THEN
          IF (DBLE(SLAG).GT.BLAM.OR.DBLE(BLAG).LT.SLAM) GO TO 101
          IF ((DBLE(SLOG     ).GT.BLOM.OR.
     +         DBLE(BLOG     ).LT.SLOM).AND.
     +        (DBLE(SLOG-360.).GT.BLOM.OR.
     +         DBLE(BLOG-360.).LT.SLOM).AND.
     +        (DBLE(SLOG+360.).GT.BLOM.OR.
     +         DBLE(BLOG+360.).LT.SLOM)) GO TO 101
        END IF
C
C See if the user wants to omit this point group.
C
        CALL HLUMAPEOD (NOUT,NSEG,IDOS(NOUT)+IDLS,
     +                            IDOS(NOUT)+IDRS,NPTS,PNTS)
        IF (ICFELL('MDPLOT',5).NE.0) RETURN
        IF (NPTS.LE.1) GO TO 101
C
C If we've switched to a new group, set the color index, dotting, and
C dash pattern for the group.
C
        IF (IGID.NE.IGIS) THEN
          IF (IGIS.NE.0) THEN
            CALL MDPCHI (-4-IGIS,0,0)
            IF (ICFELL('MDPLOT',6).NE.0) RETURN
          END IF
          CALL MDPCHI (4+IGID,IDOT,IOR(ISHIFT(32767,1),1))
          IF (ICFELL('MDPLOT',7).NE.0) RETURN
          IGIS=IGID
        END IF
C
C Plot the group.
C
        CALL MAPIT (PNTS(1),PNTS(2),0)
        IF (ICFELL('MDPLOT',8).NE.0) RETURN
C
        DO 102 K=2,NPTS-1
          CALL MAPIT (PNTS(2*K-1),PNTS(2*K),1)
          IF (ICFELL('MDPLOT',9).NE.0) RETURN
  102   CONTINUE
C
        CALL MAPIT (PNTS(2*NPTS-1),PNTS(2*NPTS),2)
        IF (ICFELL('MDPLOT',10).NE.0) RETURN
C
C Go get another group.
C
        GO TO 101
C
C Reset the color index, dotting, and dash pattern, if necessary.
C
  103   IF (IGIS.NE.0) THEN
          CALL MDPCHI (-4-IGIS,0,0)
          IF (ICFELL('MDPLOT',11).NE.0) RETURN
        END IF
C
C If the limb lines will not be (or have not been) drawn by MAPGRD,
C do it here.
C
  104   IF (GRID.LE.0.D0) THEN
          CALL MDPLMB
          IF (ICFELL('MDPLOT',12).NE.0) RETURN
        END IF
C
C Done.
C
        RETURN
C
      END
