C
C	$Id: sflush.f,v 1.1.1.1 1992-04-17 22:32:29 ncargd Exp $
C
      SUBROUTINE SFLUSH
C
C SFLUSH currently does nothing except flush the pen-move buffer.
C
      CALL PLOTIF (0.,0.,2)
C
C Done.
C
      RETURN
C
      END
