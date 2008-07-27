C
C $Id: isseti.f,v 1.5 2008-07-27 00:17:16 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
