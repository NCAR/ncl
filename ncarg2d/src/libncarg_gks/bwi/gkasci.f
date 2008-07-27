C
C	$Id: gkasci.f,v 1.5 2008-07-27 00:21:06 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      INTEGER FUNCTION GKASCI (INCODE)
C
C  Translate native character code, right-justified in INCODE,
C  to ASCII code, right-justified in function value.
C
C  The codes are as would be returned by the FORTRAN 77 ICHAR function.
C
C  This routine simply copies in to out on ASCII computers.
C
      INTEGER  INCODE
C
      GKASCI = INCODE
C
      RETURN
C
      END
