C
C $Id: line3.f,v 1.4 2000-08-22 15:07:23 haley Exp $
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
      SUBROUTINE LINE3 (UA,VA,WA,UB,VB,WB)
      CALL TRN32T (UA,VA,WA,XA,YA,XDUM,2)
      CALL TRN32T (UB,VB,WB,XB,YB,XDUM,2)
      IIX = 32*IFIX(XB)
      IIY = 32*IFIX(YB)
      CALL PLOTIT (32*IFIX(XA),32*IFIX(YA),0)
      CALL PLOTIT (IIX,IIY,1)
C
C FLUSH PLOTIT BUFFER
C
      CALL PLOTIT (0,0,0)
      RETURN
      END
