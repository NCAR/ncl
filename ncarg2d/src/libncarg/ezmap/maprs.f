C
C	$Id: maprs.f,v 1.1.1.1 1992-04-17 22:32:05 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPRS
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCM2/ UMIN,UMAX,VMIN,VMAX,UEPS,VEPS,UCEN,VCEN,URNG,VRNG,
     +                BLAM,SLAM,BLOM,SLOM,ISSL
      SAVE /MAPCM2/
      COMMON /MAPCM7/ ULOW,UROW,VBOW,VTOW
      SAVE /MAPCM7/
C
C Restore the SET call.
C
      CALL SET (ULOW,UROW,VBOW,VTOW,UMIN,UMAX,VMIN,VMAX,1)
C
C Done.
C
      RETURN
C
      END
