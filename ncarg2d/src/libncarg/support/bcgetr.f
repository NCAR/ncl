C
C $Id: bcgetr.f,v 1.2 2000-07-12 16:26:16 haley Exp $
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
      SUBROUTINE BCGETR(PA,RVAL)
C
C Retrieve real-valued parameters for the Bezier curve package.
C
C Arguments
C     Input
C             PA       Character string indicating the parameter.
C
C     Output
C             RVAL     A floating-point number representing the
C                      current setting for the specified parameter.
C

      CHARACTER*(*) PA
      CHARACTER*3   CTMP
C
      include 'bccom.h'
C
      CTMP = PA(1:3)
C
      IF (CTMP.EQ.'FLT' .OR. CTMP.EQ.'flt') THEN
C
C  Get the flatness tolerance limit.
C
        RVAL = FRATIO
      ELSE
        WRITE(I1MACH(4),500) CTMP
      ENDIF
C
      RETURN
C
  500 FORMAT(' BCGETR -- Invalid keyword = ',A3,', no action taken')
C
      END
