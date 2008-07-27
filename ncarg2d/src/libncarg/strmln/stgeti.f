C
C       $Id: stgeti.f,v 1.7 2008-07-27 00:17:28 haley Exp $
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
      SUBROUTINE STGETI (CNM,IVL)
C
      CHARACTER*(*) CNM
C
C This subroutine is called to retrieve the integer value of a specified
C parameter.
C
C CNM is the name of the parameter whose value is to be retrieved.
C
C IVL is an integer variable in which the desired value is to be
C returned by STGETI.
C
C ---------------------------------------------------------------------
C
C Use STGETR to retrieve the real value, fix it, and return it to the
C user.
C
      CALL STGETR (CNM,RVL)
      IVL=INT(RVL)
C
C Done.
C
      RETURN
C
      END
