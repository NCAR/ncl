C
C	$Id: curved.f,v 1.1.1.1 1992-04-17 22:35:00 ncargd Exp $
C
      SUBROUTINE CURVED (X,Y,N)
      DIMENSION X(N),Y(N)
      SAVE
C
      CALL FRSTD (X(1),Y(1))
      DO 10 I=2,N
         CALL VECTD (X(I),Y(I))
   10 CONTINUE
      CALL LASTD
C
      RETURN
      END
