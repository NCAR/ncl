C
C $Id: line.f,v 1.6 2000-07-12 16:25:39 haley Exp $
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
      SUBROUTINE LINE (X1,Y1,X2,Y2)
C
C Draw a line connecting the point (X1,Y1) to the point (X2,Y2), in the
C user coordinate system.
C
      IF (ICFELL('LINE - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
      CALL PLOTIF (CUFX(X1),CUFY(Y1),0)
      IF (ICFELL('LINE',2).NE.0) RETURN
      CALL PLOTIF (CUFX(X2),CUFY(Y2),1)
      IF (ICFELL('LINE',3).NE.0) RETURN
      CALL PLOTIF (0.,0.,2)
      IF (ICFELL('LINE',4).NE.0) RETURN
      RETURN
      END
