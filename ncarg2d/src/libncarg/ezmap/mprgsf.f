C
C $Id: mprgsf.f,v 1.1 2001-05-18 22:49:39 kennison Exp $
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
      SUBROUTINE MPRGSF (IRGL)
C
C This routine is called to draw RANGS/GSHHS polygons in the current
C EZMAP window.  The argument IRGL specifies the level of resolution
C to be used, from 0 (highest resolution) to 4 (lowest resolution).
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM4/ INTF,JPRJ,PHIA,PHIO,ROTA,ILTS,PLA1,PLA2,PLA3,
     +                  PLA4,PLB1,PLB2,PLB3,PLB4,PLTR,GRID,IDSH,IDOT,
     +                  LBLF,PRMF,ELPF,XLOW,XROW,YBOW,YTOW,IDTL,GRDR,
     +                  SRCH,ILCW,GRLA,GRLO,GRPO
        LOGICAL         INTF,LBLF,PRMF,ELPF
        SAVE   /MAPCM4/
C
        COMMON /MAPRGD/ ICOL(5),ICSF(5),NILN,NILT
        SAVE   /MAPRGD/
C
C Define a local array to receive some information we don't care about
C from the GKS routine GQCLIP.
C
        DIMENSION DUMI(4)
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MPRGSF - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If EZMAP needs initialization, do it.
C
        IF (INTF) THEN
          CALL MAPINT
          IF (ICFELL('MPRGSF',2).NE.0) RETURN
        END IF
C
C Open the RANGS/GSHHS files.
C
        CALL MPRGOF (MAX(0,MIN(4,IRGL)),ICAT,ICEL,IRIM)
        IF (ICFELL('MPRGSF',3).NE.0) RETURN
C
C Save the current state of the GKS clipping flag and turn clipping on.
C
        CALL GQCLIP (IERR,IGCF,DUMI)
C
        IF (IERR.NE.0) THEN
          CALL SETER ('MPRGSF - ERROR EXIT FROM GQCLIP',4,1)
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
          CALL SETER ('MPRGSF - ERROR EXIT FROM GQFACI',5,1)
          RETURN
        END IF
C
C Look for 1-degree squares that are visible and plot polygons for them.
C
        DO 103 ILON=0,359
          DO 102 ILAT = -90,89
            CALL MAPTRA (REAL(ILAT  ),REAL(ILON  ),UPRJ,VPRJ)
            IF (UPRJ.NE.1.E12) GO TO 101  !  LL VISIBLE
            CALL MAPTRA (REAL(ILAT  ),REAL(ILON+1),UPRJ,VPRJ)
            IF (UPRJ.NE.1.E12) GO TO 101  !  LR VISIBLE
            CALL MAPTRA (REAL(ILAT+1),REAL(ILON  ),UPRJ,VPRJ)
            IF (UPRJ.NE.1.E12) GO TO 101  !  UL VISIBLE
            CALL MAPTRA (REAL(ILAT+1),REAL(ILON+1),UPRJ,VPRJ)
            IF (UPRJ.NE.1.E12) GO TO 101  !  UR VISIBLE
            CALL MAPTRI (CFUX(.5),CFUY(.5),RLAT,RLON)
            IF (RLAT.LT.REAL(ILAT).OR.RLAT.GT.REAL(ILAT+1)) GO TO 102
            IF (RLON.LT.REAL(ILON)) RLON=RLON+360.
            IF (RLON.GT.REAL(ILON+360)) RLON=RLON-360.
            IF (RLON.LT.REAL(ILON).OR.RLON.GT.REAL(ILON+1)) GO TO 102
  101       CALL MPRGSQ (ICAT,ICEL,IRIM,ILAT,ILON,0.,1)
            IF (ICFELL('MPRGSF',6).NE.0) GO TO 104
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
        CALL CLFILE (ICAT)
        CALL CLFILE (ICEL)
        CALL CLFILE (IRIM)
C
C Done.
C
        RETURN
C
      END
