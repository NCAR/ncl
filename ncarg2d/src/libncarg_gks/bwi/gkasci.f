C
C	$Id: gkasci.f,v 1.2 1993-01-09 02:07:14 fred Exp $
C
      INTEGER FUNCTION GKASCI (INCODE)
C
C  Translate native character code, right-justified in INCODE,
C  to ASCII code, right-justified in function value.
C
C  The codes are as would be returned by the FORTRAN 77 ICHAR function.
C
C  This routine simply copies in to out on ASCII computers.
C
      INTEGER  INCODE
C
      GKASCI = INCODE
C
      RETURN
C
      END
