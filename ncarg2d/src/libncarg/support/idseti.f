C
C $Id: idseti.f,v 1.2 2000-07-12 16:26:19 haley Exp $
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
      SUBROUTINE IDSETI (PNAM,IVAL)
C
C Set the integer value of the BIVAR parameter named PNAM from IVAL.
C
        CHARACTER*(*) PNAM
C
C Check for an uncleared prior error.
C
        IF (ICFELL('IDSETI (BIVAR) - UNCLEARED PRIOR ERROR',1).NE.0)
     +                                                        RETURN
C
C Pass IDSETR the real equivalent of the integral value and let it do
C the work.
C
        CALL IDSETR (PNAM,REAL(IVAL))
        IF (ICFELL('IDSETI',2).NE.0) RETURN
C
C Done.
C
        RETURN
C
      END
