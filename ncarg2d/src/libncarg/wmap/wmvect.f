C
C	$Id: wmvect.f,v 1.2 2008-04-04 21:02:58 kennison Exp $
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
      SUBROUTINE WMVECT(X,Y,U,V)
C
C  This subroutine plots wind vectors with the origin of the vector at
C  (X,Y).  U and V are the components of the wind vector.
C
      include 'wmcomn.h'
C
      PARAMETER (R2D=57.2957795131)
C
C  Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL WMBLDA
C
C  Save the current line and fill colors and set them to ICOLOR.
C
      CALL GQPLCI(IER,ILCLRO)
      CALL GQLWSC(IER,RLNWTH)
C
      CALL GSPLCI(VCCOLR)
      CALL GSLWSC(VCLWID)
C
      SPEED  = SQRT(U*U+V*V)
      CALL WMGETR('VRS',VRS)
      CALL WMGETR('VRN',VRN)
C
      UP =  U
      VP =  V
      COSANG = UP/SPEED
      SINANG = VP/SPEED
      IF (VP .GE. 0.) THEN
        ALPHA = ACOS(UP/SPEED)*R2D
      ELSE
        ALPHA = -ACOS(UP/SPEED)*R2D
      ENDIF
      CALL WMSETR('VCD',ALPHA)
      CALL WMSETR('VCS',SPEED)
C
C  Draw the vector.
C
      CALL WMLABS(X,Y,'Vector')
C
C  Restore the original environment.
C
      CALL GSPLCI(ILCLRO)
C     CALL GSELNT(NTRO)
      CALL GSLWSC(RLNWTH)
      CALL PCSETI ('FN',IFNO)
C
      RETURN
      END
