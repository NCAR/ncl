C
C	$Id: frstd.f,v 1.4 2008-07-27 00:23:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE FRSTD (X,Y)
      SAVE
C USER ENTRY PPINT.
      CALL FL2INT (X,Y,IIX,IIY)
      CALL CFVLD (1,IIX,IIY)
      RETURN
      END
