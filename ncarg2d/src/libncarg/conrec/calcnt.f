C
C	$Id: calcnt.f,v 1.2 2000-07-12 16:22:51 haley Exp $
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
      SUBROUTINE CALCNT (Z,M,N,A1,A2,A3,I1,I2,I3)
C
C THIS ENTRY POINT IS FOR USERS WHO ARE TOO LAZY TO SWITCH OLD DECKS
C TO THE NEW CALLING SEQUENCE.
C
      DIMENSION       Z(M,N)
      SAVE
C
C THE FOLLOWING CALL IS FOR GATHERING STATISTICS ON LIBRARY USE AT NCAR
C
      CALL Q8QST4 ('GRAPHX','CONREC','CALCNT','VERSION 01')
C
      CALL CONREC (Z,M,M,N,A1,A2,A3,I1,I2,-IABS(I3))
      RETURN
      END
