C
C $Id: mdrgsf.f,v 1.1 2001-08-16 23:09:38 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE MDRGSF (IRGL)
C
        INTEGER IRGL
C
C This routine is called to draw RANGS/GSHHS polygons in the current
C EZMAP window.  The argument IRGL specifies the level of resolution
C to be used, from 0 (highest resolution) to 4 (lowest resolution).
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCM4/  GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW,IDOT,IDSH,IDTL,ILCW,
     +                   ILTS,JPRJ,ELPF,INTF,LBLF,PRMF
        DOUBLE PRECISION GRDR,GRID,GRLA,GRLO,GRPO,OTOL,PHIA,PHIO,PLA1,
     +                   PLA2,PLA3,PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,ROTA,
     +                   SRCH,XLOW,XROW,YBOW,YTOW
        INTEGER          IDOT,IDSH,IDTL,ILCW,ILTS,JPRJ
        LOGICAL          ELPF,INTF,LBLF,PRMF
        SAVE   /MAPCM4/
C
        COMMON /MAPRGD/  ICOL(5),ICSF(5),NILN,NILT
        INTEGER          ICOL,ICSF,NILN,NILT
        SAVE   /MAPRGD/
C
C Declare local variables.
C
        DOUBLE PRECISION RLAT,RLON,UPRJ,VPRJ
        INTEGER          ICAT,ICEL,IERR,IFAC,IGCF,ILAT,ILON,IRIM
        REAL             DUMI(4)
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDRGSF - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MDPINT
          IF (ICFELL('MDRGSF',2).NE.0) RETURN
        END IF
C
C Open the RANGS/GSHHS files.
C
        CALL MDRGOF (MAX(0,MIN(4,IRGL)),ICAT,ICEL,IRIM)
        IF (ICFELL('MDRGSF',3).NE.0) RETURN
C
C Save the current state of the GKS clipping flag and turn clipping on.
C
        CALL GQCLIP (IERR,IGCF,DUMI)
C
        IF (IERR.NE.0) THEN
          CALL SETER ('MDRGSF - ERROR EXIT FROM GQCLIP',4,1)
          RETURN
        END IF
C
        CALL GSCLIP (1)
C
C Save the current fill-area color.
C
        CALL GQFACI (IERR,IFAC)
C
        IF (IERR.NE.0) THEN
          CALL SETER ('MDRGSF - ERROR EXIT FROM GQFACI',5,1)
          RETURN
        END IF
C
C Look for 1-degree squares that are visible and plot polygons for them.
C
        DO 103 ILON=0,359
          DO 102 ILAT = -90,89
            CALL MDPTRA (DBLE(ILAT  ),DBLE(ILON  ),UPRJ,VPRJ)
            IF (UPRJ.NE.1.D12) GO TO 101  !  LL VISIBLE
            CALL MDPTRA (DBLE(ILAT  ),DBLE(ILON+1),UPRJ,VPRJ)
            IF (UPRJ.NE.1.D12) GO TO 101  !  LR VISIBLE
            CALL MDPTRA (DBLE(ILAT+1),DBLE(ILON  ),UPRJ,VPRJ)
            IF (UPRJ.NE.1.D12) GO TO 101  !  UL VISIBLE
            CALL MDPTRA (DBLE(ILAT+1),DBLE(ILON+1),UPRJ,VPRJ)
            IF (UPRJ.NE.1.D12) GO TO 101  !  UR VISIBLE
            CALL MDPTRI (UMIN,VMIN,RLAT,RLON)
            IF (RLON.LT.DBLE(ILON)) RLON=RLON+360.D0
            IF (RLON.GT.DBLE(ILON+360)) RLON=RLON-360.D0
            IF (RLAT.GE.DBLE(ILAT).AND.RLAT.LE.DBLE(ILAT+1).AND.
     +          RLON.GE.DBLE(ILON).AND.RLON.LE.DBLE(ILON+1)) GO TO 101
            CALL MDPTRI (UMIN,VMAX,RLAT,RLON)
            IF (RLON.LT.DBLE(ILON)) RLON=RLON+360.D0
            IF (RLON.GT.DBLE(ILON+360)) RLON=RLON-360.D0
            IF (RLAT.GE.DBLE(ILAT).AND.RLAT.LE.DBLE(ILAT+1).AND.
     +          RLON.GE.DBLE(ILON).AND.RLON.LE.DBLE(ILON+1)) GO TO 101
            CALL MDPTRI (UMAX,VMIN,RLAT,RLON)
            IF (RLON.LT.DBLE(ILON)) RLON=RLON+360.D0
            IF (RLON.GT.DBLE(ILON+360)) RLON=RLON-360.D0
            IF (RLAT.GE.DBLE(ILAT).AND.RLAT.LE.DBLE(ILAT+1).AND.
     +          RLON.GE.DBLE(ILON).AND.RLON.LE.DBLE(ILON+1)) GO TO 101
            CALL MDPTRI (UMAX,VMAX,RLAT,RLON)
            IF (RLON.LT.DBLE(ILON)) RLON=RLON+360.D0
            IF (RLON.GT.DBLE(ILON+360)) RLON=RLON-360.D0
            IF (RLAT.GE.DBLE(ILAT).AND.RLAT.LE.DBLE(ILAT+1).AND.
     +          RLON.GE.DBLE(ILON).AND.RLON.LE.DBLE(ILON+1)) GO TO 101
            GO TO 102
  101       CALL MDRGSQ (ICAT,ICEL,IRIM,ILAT,ILON,0,1)
            IF (ICFELL('MDRGSF',6).NE.0) GO TO 104
  102     CONTINUE
  103   CONTINUE
C
C Restore the fill-area color to its original value.
C
  104   CALL GSFACI (IFAC)
C
C Reset the clipping flag.
C
        CALL GSCLIP (IGCF)
C
C Close all RANGS/GSHHS files.
C
        CALL NGCLFI (ICAT)
        CALL NGCLFI (ICEL)
        CALL NGCLFI (IRIM)
C
C Done.
C
        RETURN
C
      END
