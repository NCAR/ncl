C
C $Id: isgeti.f,v 1.5 2008-07-27 00:17:16 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
