C
C $Id: bcsubd.f,v 1.2 2000-07-12 16:26:16 haley Exp $
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
      SUBROUTINE BCSUBD(BX,BY,SX,SY,TX,TY)
C
C  Do a midpoint subdivision of the input Bezier curve whose control 
C  points are in BX and BY to create two Bezier curves whose control
C  points are in SX, SY and TX, TY.
C
      DIMENSION BX(4),BY(4),SX(4),SY(4),TX(4),TY(4)
C
      SX(1) = BX(1)
      SY(1) = BY(1)
      SX(2) = .5*(BX(1)+BX(2))
      SY(2) = .5*(BY(1)+BY(2))
      TMPX  = .5*(BX(2)+BX(3))
      TMPY  = .5*(BY(2)+BY(3))
      SX(3) = .5*(SX(2)+TMPX)
      SY(3) = .5*(SY(2)+TMPY)
      TX(4) = BX(4)
      TY(4) = BY(4)
      TX(3) = .5*(BX(3)+BX(4))
      TY(3) = .5*(BY(3)+BY(4))
      TX(2) = .5*(TMPX+TX(3))
      TY(2) = .5*(TMPY+TY(3))
      SX(4) = .5*(SX(3)+TX(2))
      SY(4) = .5*(SY(3)+TY(2))
      TX(1) = SX(4)
      TY(1) = SY(4)
C
      RETURN
      END
