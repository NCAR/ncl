C
C	$Id: vectd.f,v 1.1.1.1 1992-04-17 22:35:00 ncargd Exp $
C
      SUBROUTINE VECTD (X,Y)
      SAVE
C USER ENTRY POINT.
      CALL FL2INT (X,Y,IIX,IIY)
      CALL CFVLD (2,IIX,IIY)
      RETURN
      END
