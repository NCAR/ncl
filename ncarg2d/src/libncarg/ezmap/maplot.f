C
C $Id: maplot.f,v 1.10 2000-07-12 16:23:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE MAPLOT
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UCEN,VCEN,URNG,VRNG,BLAM,SLAM,
     +                BLOM,SLOM,ISSL,PEPS
      SAVE   /MAPCM2/
C
      COMMON /MAPCM3/ ITPN,NOUT,NPTS,IGID,IDLS,IDRS,BLAG,SLAG,BLOG,SLOG,
     +                PNTS(200),IDOS(4)
      SAVE   /MAPCM3/
C
      COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,PLA4,
     +                PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,LBLF,PRMF,
     +                ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,SRCH,ILCW,GRLA,
     +                GRLO,GRPO
      LOGICAL         INTF,LBLF,PRMF,ELPF
      SAVE   /MAPCM4/
C
C Define required constants.
C
      DATA PI / 3.14159265358979 /
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPLOT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
      IF (INTF) THEN
        CALL MAPINT
        IF (ICFELL('MAPLOT',2).NE.0) RETURN
      END IF
C
C If the selected outline type is "NONE", do nothing.
C
      IF (NOUT.LE.0) RETURN
C
C Set the flag IWGF to say whether or not the whole globe is shown by
C the current projection.  If so (IWGF=1), there's no need to waste the
C time required to check each outline point group for intersection with
C the window.
C
      IWGF=0
      IF (BLAM-SLAM.GT.179.9999.AND.BLOM-SLOM.GT.359.9999) IWGF=1
C
C IGIS keeps track of changes in the group identifier, so that the
C color index can be changed when necessary.
C
      IGIS=0
C
C Position to the user-selected portion of the outline dataset.
C
      CALL MAPIO (1)
      IF (ICFELL('MAPLOT',3).NE.0) RETURN
      NSEG=0
C
C Read the next record (group of points).
C
  101 CALL MAPIO (2)
      IF (ICFELL('MAPLOT',4).NE.0) RETURN
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
        IF (SLAG.GT.BLAM.OR.BLAG.LT.SLAM) GO TO 101
        IF ((SLOG     .GT.BLOM.OR.BLOG     .LT.SLOM).AND.
     +      (SLOG-360..GT.BLOM.OR.BLOG-360..LT.SLOM).AND.
     +      (SLOG+360..GT.BLOM.OR.BLOG+360..LT.SLOM)) GO TO 101
      END IF
C
C See if the user wants to omit this point group.
C
      CALL HLUMAPEOD (NOUT,NSEG,IDOS(NOUT)+IDLS,
     +                          IDOS(NOUT)+IDRS,NPTS,PNTS)
      IF (ICFELL('MAPLOT',5).NE.0) RETURN
      IF (NPTS.LE.1) GO TO 101
C
C If we've switched to a new group, set the color index, dotting, and
C dash pattern for the group.
C
      IF (IGID.NE.IGIS) THEN
        IF (IGIS.NE.0) THEN
          CALL MAPCHI (-4-IGIS,0,0)
          IF (ICFELL('MAPLOT',6).NE.0) RETURN
        END IF
        CALL MAPCHI (4+IGID,IDOT,IOR(ISHIFT(32767,1),1))
        IF (ICFELL('MAPLOT',7).NE.0) RETURN
        IGIS=IGID
      END IF
C
C Plot the group.
C
      CALL MAPIT (PNTS(1),PNTS(2),0)
      IF (ICFELL('MAPLOT',8).NE.0) RETURN
C
      DO 102 K=2,NPTS-1
        CALL MAPIT (PNTS(2*K-1),PNTS(2*K),1)
        IF (ICFELL('MAPLOT',9).NE.0) RETURN
  102 CONTINUE
C
      CALL MAPIT (PNTS(2*NPTS-1),PNTS(2*NPTS),2)
      IF (ICFELL('MAPLOT',10).NE.0) RETURN
C
C Go get another group.
C
      GO TO 101
C
C Reset the color index, dotting, and dash pattern, if necessary.
C
  103 IF (IGIS.NE.0) THEN
        CALL MAPCHI (-4-IGIS,0,0)
        IF (ICFELL('MAPLOT',11).NE.0) RETURN
      END IF
C
C If the limb lines have not already been drawn, do it now.
C
      IF (GRID.LE.0.) THEN
        CALL MAPLMB
        IF (ICFELL('MAPLOT',12).NE.0) RETURN
      END IF
C
C Draw the limb lines.
C
C     CALL MAPLMB
C     IF (ICFELL('MAPLOT',12).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
