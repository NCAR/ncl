C
C $Id: mxmy.f,v 1.4 2000-07-12 16:25:39 haley Exp $
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
      SUBROUTINE MXMY (IX,IY)
C
C Return to the user the coordinates of the current pen position, in the
C plotter coordinate system.
C
C In the common block PLTCM are recorded the coordinates of the last
C pen position, in the metacode coordinate system.
C
      COMMON /PLTCM/ JX,JY
      SAVE /PLTCM/
C
C Declare the common block containing the user state variables LL, MI,
C MX, and MY.
C
      COMMON /IUTLCM/ LL,MI,MX,MY,IU(96)
      SAVE /IUTLCM/
C
C Return to the user the plotter-system equivalents of the values in
C the metacode system.
C
      IX=1+IFIX((2.**MX-1.)*FLOAT(JX)/32767.)
      IY=1+IFIX((2.**MY-1.)*FLOAT(JY)/32767.)
C
C Done.
C
      RETURN
C
      END
