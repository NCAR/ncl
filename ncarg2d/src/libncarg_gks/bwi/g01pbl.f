C
C	$Id: g01pbl.f,v 1.2 1993-01-09 02:06:30 fred Exp $
C
      INTEGER FUNCTION G01PBL (NCHARS, NBYTES)
C
C  Return as the value of the function the parameter list length
C  of an element that contains NCHARS characters of string data
C  and NBYTES bytes of other data.
C
C  The length will be either  NCHARS+NBYTES+1 or NCHARS+NBYTES+3,
C  depending upon whether a short form or long form string is
C  called for.
C
      INTEGER  NCHARS, NBYTES, LEN
C
C  For short form.
C
      LEN = 1 + NCHARS + NBYTES
C
C  Short form can handle at most 254 characters in a string.
C
      IF (NCHARS.GT.254)  LEN = LEN + 2
      G01PBL = LEN
C
      RETURN
      END
