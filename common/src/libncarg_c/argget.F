C
C	$Id: argget.F,v 1.2 2000-07-11 21:58:04 haley Exp $
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

      SUBROUTINE ARGGET(NUMBER,STRING)
C***********************************************************************
C	
C	Procedure: ARGGET()
C
C	Purpose: To determine the "NUMBER"th argument and
C		copy it into a CHARACTER*1 array "STRING".
C
C	Input Variables:
C		NUMBER:	The index of the desired argument.
C
C	Output Variables:
C		STRING:	A CHARACTER*1 array which is filled
C			with the appropriate argument.
C
C
C***********************************************************************
      INTEGER NUMBER
      CHARACTER*1 STRING(80)
      INTEGER I
      CHARACTER*80 ARG
C
      DO 10 I = 1,80
          ARG(I:I) = ' '
 10   CONTINUE

#ifdef	hpux
      CALL IGETARG(NUMBER,ARG, 80)
#else
      CALL GETARG(NUMBER,ARG)
#endif

      DO 20 I = 1,80
          STRING(I) = ARG(I:I)
 20   CONTINUE
C
      RETURN
      END
