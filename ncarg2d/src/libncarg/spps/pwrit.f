C
C	$Id: pwrit.f,v 1.1.1.1 1992-04-17 22:32:31 ncargd Exp $
C
      SUBROUTINE PWRIT (PX,PY,CH,NC,IS,IO,IC)
      CHARACTER*(*) CH
C
C PWRIT is called to draw a character string in a specified position.
C It is just like WTSTR, but has one extra argument.  NC is the number
C of characters to be written from the string CH.
C
      CALL WTSTR (PX,PY,CH(1:NC),IS,IO,IC)
C
C Done.
C
      RETURN
C
      END
