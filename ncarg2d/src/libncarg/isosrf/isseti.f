C
C	$Id: isseti.f,v 1.2 2000-07-12 16:24:30 haley Exp $
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
C
C The subroutine ISSETI.
C --- ---------- -------
C
      SUBROUTINE ISSETI (IPN,IVL)
C
      CHARACTER*(*) IPN
C
C This subroutine is called to set the integer value of a specified
C parameter.
C
C IPN is the name of the parameter whose value is to be set.
C
C IVL is the desired new integer value.
C
C Pass the real equivalent of the integer value on to ISSETR.
C
      CALL ISSETR (IPN,REAL(IVL))
C
C Done.
C
      RETURN
C
      END
