C
C	$Id: curved.f,v 1.1.1.1 1992-04-17 22:34:59 ncargd Exp $
C
      SUBROUTINE CURVED (X,Y,N)
C USER ENTRY POINT.
C
      DIMENSION X(N),Y(N)
C
      CALL FRSTD (X(1),Y(1))
      DO 10 I=2,N
         CALL VECTD (X(I),Y(I))
   10 CONTINUE
C
      CALL LASTD
C
      RETURN
      END
