C
C $Id: ctfran.f,v 1.1 2003-05-28 15:44:29 kennison Exp $
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
      FUNCTION CTFRAN ()
C
C Pseudo-random-number generator.
C
      DOUBLE PRECISION X
      SAVE X
C
      DATA X / 2.718281828459045 /
C
      X=MOD(9821.D0*X+.211327D0,1.D0)
      CTFRAN=REAL(X)
C
      RETURN
C
      END
