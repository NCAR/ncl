C
C $Id: bcftol.f,v 1.3 2000-08-22 15:06:50 haley Exp $
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
      REAL FUNCTION BCFTOL(BX,BY)
C
C  Returns the maximum undirected distance that the line connecting 
C  the points  (BX(1),BY(1))  and  (BX(4),BY(4))  is away from the
C  actual Bezier curve whose control points are given in BX and BY.
C
      DIMENSION BX(4),BY(4)
C
      D1 = BCDP2L(BX(2),BY(2),BX(1),BY(1),BX(4),BY(4))
      D2 = BCDP2L(BX(3),BY(3),BX(1),BY(1),BX(4),BY(4))
C 
      BCFTOL = MAX(D1,D2)
C
      RETURN
      END
