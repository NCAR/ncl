C
C	$Id: line3w.f,v 1.2 2000-07-12 16:26:45 haley Exp $
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
      SUBROUTINE LINE3W (XA,YA,XB,YB)
      SAVE
      COMMON /PRM31/  Q          ,L
      GO TO ( 10, 30, 40),L
   10 UA = Q
      UB = Q
      VA = XA
      VB = XB
   20 WA = YA
      WB = YB
      GO TO  50
   30 UA = XA
      UB = XB
      VA = Q
      VB = Q
      GO TO  20
   40 UA = XA
      UB = XB
      VA = YA
      VB = YB
      WA = Q
      WB = Q
   50 CALL LINE3 (UA,VA,WA,UB,VB,WB)
      RETURN
      END
