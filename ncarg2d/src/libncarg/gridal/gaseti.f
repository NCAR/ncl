C
C $Id: gaseti.f,v 1.5 2000-08-22 15:04:36 haley Exp $
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
      SUBROUTINE GASETI (PNAM,IVAL)
C
        CHARACTER*(*) PNAM
C
C The subroutine GASETI may be used to set GRIDAL parameters which have
C values of type INTEGER.
C
C Check for an uncleared prior error.
C
        IF (ICFELL('GASETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Just convert the call into a call to the routine GASETR.
C
        CALL GASETR (PNAM,REAL(IVAL))
        IF (ICFELL('GASETI',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
