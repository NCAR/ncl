C
C $Id: uliber.f,v 1.3 2000-07-12 16:26:23 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE ULIBER (IERR,MESS,LMESS)
C
      CHARACTER*(*) MESS
C
C SUBROUTINE ULIBER (IERR,MESS,LMESS)
C
C PURPOSE                TO PRINT AN ERROR NUMBER AND AN ERROR MESSAGE
C                        OR JUST AN ERROR MESSAGE.
C
C USAGE                  CALL ULIBER (IERR,MESS,LMESS)
C
C ARGUMENTS
C ON INPUT               IERR
C                          THE ERROR NUMBER (PRINTED ONLY IF NON-ZERO).
C
C                        MESS
C                          MESSAGE TO BE PRINTED ( < 130 CHARACTERS)
C
C                        LMESS
C                          NO LONGER USED
C
C ARGUMENTS
C ON OUTPUT              NONE
C
C I/O                    THE MESSAGE IS WRITEN TO UNIT I1MACH(4).
C
C ******************************************************************
C
      IERU=I1MACH(4)
      IF (IERR.NE.0) WRITE (IERU,1001) IERR
      WRITE (IERU,1002) MESS
      RETURN
C
 1001 FORMAT ('0IERR=',I5)
 1002 FORMAT (A)
C
      END
