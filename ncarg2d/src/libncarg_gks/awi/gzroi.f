C
C	$Id: gzroi.f,v 1.3 2000-08-22 15:08:34 haley Exp $
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
      SUBROUTINE GZROI(IZ)
C
C  Zero out the interface variables depending on IZ:
C
C     IZ = 0  Zero our all variables.
C        = 1  Zero out all variables except IC1 and IC2.
C
      include 'gkscom.h'
C
      IF (IZ .EQ. 0) THEN
        IL1 = 0
        IL2 = 0
        IC1 = 0
        IC2 = 0
        RL1 = 0
        RL2 = 0
        STRL1 = 0
        STRL2 = 0
      ELSE IF (IZ .EQ. 1) THEN
        IL1 = 0
        IL2 = 0
        RL1 = 0
        RL2 = 0
        STRL1 = 0
        STRL2 = 0
      ENDIF
C
      RETURN
      END
