C
C	$Id: frstd.f,v 1.1.1.1 1992-04-17 22:35:00 ncargd Exp $
C
      SUBROUTINE FRSTD (X,Y)
      SAVE
C USER ENTRY PPINT.
      CALL FL2INT (X,Y,IIX,IIY)
      CALL CFVLD (1,IIX,IIY)
      RETURN
      END
