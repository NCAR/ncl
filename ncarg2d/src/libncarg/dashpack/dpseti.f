C
C $Id: dpseti.f,v 1.5 2000-08-22 15:03:24 haley Exp $
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
      SUBROUTINE DPSETI (PNAM,IVAL)
C
C This routine, given an integer value, sets the value of an internal
C parameter of type INTEGER or REAL to that value.
C
        CHARACTER*(*) PNAM
C
C Check for an uncleared prior error.
C
        IF (ICFELL('DPSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Pass the buck to DPSETR.
C
        CALL DPSETR (PNAM,REAL(IVAL))
        IF (ICFELL('DPSETI',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
