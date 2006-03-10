C
C $Id: curve3.f,v 1.5 2006-03-10 15:31:40 kennison Exp $
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
      SUBROUTINE CURVE3 (U,V,W,N)
      SAVE
      DIMENSION       U(N)       ,V(N)       ,W(N)
      CALL TRN32T (U(1),V(1),W(1),X,Y,ZDUM,2)
      CALL PLOTIT (32*INT(X),32*INT(Y),0)
      NN = N
      IF (NN .LT. 2) RETURN
      DO  10 I=2,NN
         UU = U(I)
         VV = V(I)
         WW = W(I)
         CALL TRN32T (UU,VV,WW,X,Y,ZDUM,2)
         CALL PLOTIT (32*INT(X),32*INT(Y),1)
   10 CONTINUE
C
C FLUSH PLOTIT BUFFER
C
      CALL PLOTIT(0,0,0)
      RETURN
      END
