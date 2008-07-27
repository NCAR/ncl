C
C       $Id: vvgeti.f,v 1.6 2008-07-27 00:17:35 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VVGETI (CNM,IVL)
C
      CHARACTER*(*) CNM
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C CNM is the name of the parameter whose value is to be retrieved.
C
C IVL is an integer variable in which the desired value is to be
C returned by VVGETI.
C
C
C Use VVGETR to retrieve the real value, fix it, and return it to the
C user.
C
      CALL VVGETR (CNM,RVL)
      IVL=INT(RVL)
C
C Done.
C
      RETURN
C
      END
