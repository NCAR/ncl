C
C	$Id: gtflt.f,v 1.1 1993-01-09 02:03:40 fred Exp $
C
        SUBROUTINE GTFLT(FLTNUM,WSIZE,IOS,STATUS)
C
C  Get a floating point number from the metafile.
C
C  This code is version specific.  For versions of the NCAR CGM
C  prior to Version 3.00, the floating point reals were encoded
C  incorrectly as a sign-magnitude whole part, and a fractional part
C  scaled by 32767.  Subsequent versions encode the numbers
C  correctly as a 2's complement whole part and a fractional
C  part scaled by 65536.
C
C  INPUT 
C    WSIZE   -- The size of each part in bits.
C
C  OUTPUT
C    FLTNUM  -- The floating point number.
C    IOS     -- The I/O status, valid only if status flags an 
C               I/O error.
C    STATUS  -- The error status defined in COMMON TREROR.
C
      INTEGER WSIZE, IOS, STATUS
      INTEGER WHOLE, PART
      REAL FLTNUM,DTCNV2
C
      DATA DTCNV2/65536./
C
      STATUS = 0
C
C  Decode the number.
C
C  Get the whole part.
C
      CALL GOPDEC(WHOLE,WSIZE,1,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
C
C  Convert to local negative number.
C
      CALL GENNEG(WHOLE,ITMP)
C
C  Get the fractional part.
C
      CALL GOPDEC(PART,WSIZE,1,IOS,STATUS)
      IF (STATUS .NE. 0) RETURN
C
C  Convert to floating point.
C
      FLTNUM = FLOAT(ITMP) + (FLOAT(PART)/DTCNV2)
C
      RETURN
      END
