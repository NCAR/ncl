C
C	$Id: g01pbl.f,v 1.1.1.1 1992-04-17 22:33:58 ncargd Exp $
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
C  FOR SHORT FORM.
C
      LEN = 1 + NCHARS + NBYTES
C
C  SHORT FORM CAN HANDLE AT MOST 254 CHARACTERS IN STRING.
C
      IF (NCHARS.GT.254)  LEN = LEN + 2
C
      G01PBL = LEN
C
      RETURN
C
      END
