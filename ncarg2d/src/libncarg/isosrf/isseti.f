C
C	$Id: isseti.f,v 1.1.1.1 1992-04-17 22:31:24 ncargd Exp $
C
C
C The subroutine ISSETI.
C --- ---------- -------
C
      SUBROUTINE ISSETI (IPN,IVL)
C
      CHARACTER*(*) IPN
C
C This subroutine is called to set the integer value of a specified
C parameter.
C
C IPN is the name of the parameter whose value is to be set.
C
C IVL is the desired new integer value.
C
C Pass the real equivalent of the integer value on to ISSETR.
C
      CALL ISSETR (IPN,REAL(IVL))
C
C Done.
C
      RETURN
C
      END
