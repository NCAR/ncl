C
C $Id: e2fndp.f,v 1.4 2001-08-16 23:12:42 kennison Exp $
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
      DOUBLE PRECISION FUNCTION E2FNDP (ECCNTS)
C
C This function computes the constant E2.
C
      IMPLICIT DOUBLE PRECISION (A-Z)
      DATA CON1,CON2 /0.05859375D0,0.75D0/
      DATA ONE /1.0D0/
C
      E2FNDP = CON1 * ECCNTS * ECCNTS * (ONE + CON2 * ECCNTS)
C
      RETURN
      END
