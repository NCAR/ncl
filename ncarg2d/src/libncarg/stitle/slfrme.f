C
C $Id: slfrme.f,v 1.1 1995-07-28 18:38:03 kennison Exp $
C
      SUBROUTINE SLFRME
C
C The object of calling this routine is really just to advance the
C frame.  The calls to PLOTIF are to work around a GKS/translator
C problem: the background color doesn't get set properly for a totally
C blank frame, which it's easy to have in STITLE.
C
        CALL PLOTIF (0.,0.,0)
        IF (ICFELL('SLFRME',1).NE.0) RETURN
        CALL PLOTIF (0.,0.,1)
        IF (ICFELL('SLFRME',2).NE.0) RETURN
C
        CALL FRAME
        IF (ICFELL('SLFRME',3).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
