C
C	$Id: lblank.f,v 1.2 2000-07-11 21:58:06 haley Exp $
C                                                                      
C			     Copyright (C)  2000
C	     University Corporation for Atmospheric Research
C			     All Rights Reserved
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
C License along with this library; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C

c Fortran function lblank
c
c Returns:	Index of last character in string
c
c Side Effects:	None
c
c Purpose:	To provide the ability to concatenate two
c		character strings based on what is in them
c		rather than their length.
c
c Comments:	Used to be a relatively standard function,
c		before the onset (onslaught?) of System V.
c
c Author:	Don Middleton
c		NCAR Graphics Group
c		November 1988
c
      integer function lblank(str)
      character*(*) str

      do 10 i=len(str), 1, -1
	if (str(i:i).ne.' ') then
	  lblank = i
	  return
	endif
10    continue

      lblank = 1
      end
