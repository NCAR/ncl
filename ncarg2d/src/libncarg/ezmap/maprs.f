C
C $Id: maprs.f,v 1.7 1999-04-02 22:59:37 kennison Exp $
C
      SUBROUTINE MAPRS
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UCEN,VCEN,URNG,VRNG,BLAM,SLAM,
     +                BLOM,SLOM,ISSL,PEPS
      SAVE   /MAPCM2/
C
      COMMON /MAPCM7/ ULOW,UROW,VBOW,VTOW
      SAVE   /MAPCM7/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPRS - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Restore the SET call.
C
      CALL SET (ULOW,UROW,VBOW,VTOW,UMIN,UMAX,VMIN,VMAX,1)
      IF (ICFELL('MAPRS',2).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
