C
C $Id: sfseti.f,v 1.4 2000-08-22 15:06:00 haley Exp $
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
      SUBROUTINE SFSETI (CNP,IVP)
C
      CHARACTER*(*) CNP
C
C This subroutine is called to give an integer value to a specified
C parameter.
C
C CNP is the name of the parameter whose value is to be set.
C
C IVP is an integer variable containing the desired value.
C
C
C Check for an uncleared prior error.
C
      IF (ICFELL('SFSETI - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Convert the given value to a real and then use SFSETR to set the
C parameter.
C
      CALL SFSETR (CNP,REAL(IVP))
      IF (ICFELL('SFSETI',2).NE.0) RETURN
C
C Done.
C
      RETURN
C
      END
