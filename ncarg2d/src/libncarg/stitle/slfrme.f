C
C $Id: slfrme.f,v 1.4 2008-07-27 00:17:26 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
