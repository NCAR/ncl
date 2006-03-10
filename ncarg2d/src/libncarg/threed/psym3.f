C
C $Id: psym3.f,v 1.5 2006-03-10 15:31:41 kennison Exp $
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
      SUBROUTINE PSYM3 (U,V,W,ICHAR,SIZE,IDIR,ITOP,IUP)
      IF (IUP .EQ. 2) CALL VECT3 (U,V,W)
      CALL PWRZ (U,V,W,ICHAR,1,SIZE,IDIR,ITOP,0)
      RETURN
      END
