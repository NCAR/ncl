C
C $Id: pwrit.f,v 1.3 1994-03-17 01:44:13 kennison Exp $
C
      SUBROUTINE PWRIT (PX,PY,CH,NC,IS,IO,IC)
      CHARACTER*(*) CH
C
C PWRIT is called to draw a character string in a specified position.
C It is just like WTSTR, but has one extra argument.  NC is the number
C of characters to be written from the string CH.
C
      IF (ICFELL('PWRIT - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      CALL WTSTR (PX,PY,CH(1:NC),IS,IO,IC)
      IF (ICFELL('PWRIT',2).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
