C
C	$Id: mapiq.f,v 1.1.1.1 1992-04-17 22:32:01 ncargd Exp $
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MAPIQ
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCMP/ NPTB,XPTB(50),YPTB(50)
      SAVE /MAPCMP/
C
C Flush the points buffer.
C
      IF (NPTB.GT.0) THEN
        CALL POINTS (XPTB,YPTB,NPTB,0,0)
        NPTB=0
      END IF
C
C Flush PLOTIT's buffer, too.
C
      CALL PLOTIT (0,0,0)
C
C Done.
C
      RETURN
C
      END
