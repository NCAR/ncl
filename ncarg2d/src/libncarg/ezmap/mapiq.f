C
C $Id: mapiq.f,v 1.2 1993-12-21 00:33:06 kennison Exp $
C
      SUBROUTINE MAPIQ
C
C Declare required common blocks.  See MAPBD for descriptions of these
C common blocks and the variables in them.
C
      COMMON /MAPCMB/ IIER
      SAVE /MAPCMB/
      COMMON /MAPCMP/ NPTB,XPTB(50),YPTB(50)
      SAVE /MAPCMP/
C
C Flush the points buffer.
C
      IF (NPTB.GT.0) THEN
        CALL POINTS (XPTB,YPTB,NPTB,0,0)
        IF (ICFELL('MAPIQ',1).NE.0) THEN
          IIER=-1
          RETURN
        END IF
        NPTB=0
      END IF
C
C Flush PLOTIT's buffer, too.
C
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('MAPIQ',2).NE.0) THEN
        IIER=-1
        RETURN
      END IF
C
C Done.
C
      RETURN
C
      END
