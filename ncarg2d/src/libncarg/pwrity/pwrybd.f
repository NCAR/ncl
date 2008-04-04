C
C	$Id: pwrybd.f,v 1.4 2008-04-04 21:02:53 kennison Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE PWRYBD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA PWRYBDX
      COMMON /PWRCOM/ USABLE
      LOGICAL         USABLE
      DATA USABLE/.FALSE./
C REVISION HISTORY------
C FEBURARY 1979    CREATED NEW ALGORITHM PWRITY TO REPLACE PWRY
C                  ADDED REVISION HISTORY
C JUNE 1979        CHANGE ARGUMENT THETA IN PWRITY FROM FLOATING TO
C                  INTEGER, USING ITHETA AS THE NEW NAME.  ITS
C                  MEANING IS NOW DEGREES INSTEAD OF RADIANS.
C JULY 1984        CONVERTED TO FORTRAN 77 AND GKS
C-----------------------------------------------------------------------
      END
