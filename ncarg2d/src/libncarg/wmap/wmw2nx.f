C
C	$Id: wmw2nx.f,v 1.2 2000-07-12 16:27:07 haley Exp $
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
      SUBROUTINE WMW2NX(NPT,P,Q)
C
C  This subroutine takes the NPT points in the real array P,
C  which are assumed to be in world coordinates, and, using
C  the current normalization transformation, converts them
C  into NDC coordinates and stores them in Q.  This subroutine
C  operates on X-coordinates.
C
      INTEGER NPT
      REAL P(NPT),Q(NPT)
      REAL WINDOW(4),VIEWPT(4)
C
      CALL GQCNTN(IER,NTR)
      CALL GQNT(NTR,IER,WINDOW,VIEWPT)
      SCALE = (VIEWPT(2)-VIEWPT(1))/(WINDOW(2)-WINDOW(1))
      DO 20 I=1,NPT
        Q(I) = VIEWPT(1)+(P(I)-WINDOW(1))*SCALE
   20 CONTINUE
C
      RETURN
      END
