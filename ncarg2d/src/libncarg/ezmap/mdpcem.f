C
C $Id: mdpcem.f,v 1.3 2005-06-22 21:36:44 kennison Exp $
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
      SUBROUTINE MDPCEM (IEM1,IEM2,IERR,IFLG)
C
        CHARACTER*(*) IEM1,IEM2
        INTEGER       IERR,IFLG
C
C MDPCEM is called to do a call to SETER when the error message to be
C printed is in two parts which need to be concatenated.  FORTRAN-77
C rules make it necessary to concatenate the two parts of the message
C into a local character variable.
C
        CHARACTER*100 IEMC
C
        IEMC=IEM1//IEM2
        CALL SETER (IEMC,IERR,IFLG)
C
        RETURN
C
      END
