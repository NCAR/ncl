C
C $Id: mapiqd.f,v 1.3 1998-05-23 20:19:45 kennison Exp $
C
      SUBROUTINE MAPIQD
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCMP/ NPTB,XPTB(50),YPTB(50)
      SAVE /MAPCMP/
C
C Check for an uncleared prior error.
C
      IF (ICFELL('MAPIQD - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Flush the points buffer.
C
      IF (NPTB.GT.0) THEN
        CALL POINTS (XPTB,YPTB,NPTB,0,0)
        IF (ICFELL('MAPIQD',2).NE.0) RETURN
        NPTB=0
      END IF
C
C Flush the buffer in DASHPACK.
C
      CALL DPLAST
      IF (ICFELL('MAPIQD',3).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
