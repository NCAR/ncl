C
C	$Id: isgeti.f,v 1.1.1.1 1992-04-17 22:31:24 ncargd Exp $
C
C
C The subroutine ISGETI.
C --- ---------- -------
C
      SUBROUTINE ISGETI (IPN,IVL)
C
      CHARACTER*(*) IPN
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C IPN is the name of the parameter whose value is to be retrieved.
C
C IVL is an integer variable in which the desired value is to be
C returned by ISGETI.
C
C Get the real value of the parameter and return the integer equivalent.
C
      CALL ISGETR (IPN,RVL)
      IVL=INT(RVL)
C
C Done.
C
      RETURN
C
      END
