C
C	$Id: q8qst4.f,v 1.2 2000-07-12 16:26:22 haley Exp $
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
      SUBROUTINE Q8QST4(NAME,LBRARY,ENTRY,VRSION)
C
C DIMENSION OF           NAME(1),LBRARY(1),ENTRY(1),VRSION(1)
C ARGUMENTS
C
C LATEST REVISION        MARCH 1984
C
C PURPOSE                MONITORS LIBRARY USE BY WRITING A RECORD WITH
C                        INFORMATION ABOUT THE CIRCUMSTANCES OF A
C                        LIBRARY ROUTINE CALL TO THE SYSTEM ACCOUNTING
C                        TAPE FOR LATER PROCESSING.
C
C NOTE---                THIS VERSION OF Q8QST4 SIMPLY RETURNS TO THE
C                        CALLING ROUTINE.  LOCAL IMPLEMENTORS MAY WISH
C                        TO IMPLEMENT A VERSION OF THIS ROUTINE THAT
C                        MONITORS USE OF NCAR ROUTINES WITH LOCAL
C                        MECHANISMS.  OTHERWISE IT WILL SAVE A SMALL
C                        AMOUNT OF SPACE AND TIME IF CALLS TO Q8QST4 ARE
C                        DELETED FROM ALL NSSL ROUTINES.
C
      CHARACTER*(*) NAME,LBRARY,ENTRY,VRSION
C
      RETURN
      END
