C
C $Id: mdprs.f,v 1.9 2008-09-18 00:42:17 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE MDPRS
C
C Declare required common blocks.  See MAPBDX for descriptions of these
C common blocks and the variables in them.
C
        COMMON /MAPCM2/  BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG,ISSL
        DOUBLE PRECISION BLAM,BLOM,PEPS,SLAM,SLOM,UCEN,UMAX,UMIN,UOFF,
     +                   URNG,VCEN,VMAX,VMIN,VOFF,VRNG
        INTEGER          ISSL
        SAVE   /MAPCM2/
C
        COMMON /MAPCM7/  ULOW,UROW,VBOW,VTOW
        DOUBLE PRECISION ULOW,UROW,VBOW,VTOW
        SAVE   /MAPCM7/
C
C Check for an uncleared prior error.
C
        IF (ICFELL('MDPRS - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Restore the SET call.
C
        CALL SET (REAL(ULOW),REAL(UROW),REAL(VBOW),REAL(VTOW),
     +            REAL(UMIN),REAL(UMAX),REAL(VMIN),REAL(VMAX),1)
        IF (ICFELL('MDPRS',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
