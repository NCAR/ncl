C
C $Id: idictl.f,v 1.3 1994-03-16 23:51:13 kennison Exp $
C
      INTEGER FUNCTION IDICTL (ISTR,IDCT,NDCT)
C
      CHARACTER*(*) ISTR
      CHARACTER*2 IDCT(NDCT)
C
C The value of this function is the index in the NDCT-element dictionary
C IDCT of the string ISTR.  Only the first two characters of ISTR and
C IDCT(I) are compared.  If ISTR is not found in the dictionary, the
C function value is zero.
C
      DO 101 I=1,NDCT
        IF (ISTR(1:2).EQ.IDCT(I)) THEN
          IDICTL=I
          RETURN
        END IF
  101 CONTINUE
C
C Not found.  Return a zero.
C
      IDICTL=0
      RETURN
C
      END
