C
C $Id: idiot.f,v 1.7 2006-03-09 22:56:09 kennison Exp $
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
      SUBROUTINE IDIOT (XDRA,YDRA,NPTS,LTYP,LDSH,LABX,LABY,LABG,LFRA)
C
      REAL XDRA(*),YDRA(*)
C
      INTEGER LDSH(*)
C
      CHARACTER*(*) LABX,LABY,LABG
C
      CHARACTER*16 AGBNCH
C
C This is an implementation of the routine from which AUTOGRAPH grew.
C It should work pretty much as the original did (if you can figure out
C what that was).
C
      CALL ANOTAT (LABX,LABY,1,2-SIGN(1,NPTS),1,AGBNCH(LDSH))
C
      CALL DISPLA (2-MAX(-1,MIN(1,LFRA)),1,LTYP)
C
      CALL AGEZSU (5,XDRA,YDRA,IABS(NPTS),1,IABS(NPTS),LABG,IIVX,IIEX,
     +                                                        IIVY,IIEY)
      CALL AGBACK
C
      CALL AGCURV (XDRA,1,YDRA,1,IABS(NPTS),1)
C
      IF (LFRA.GT.0) CALL FRAME
C
      RETURN
C
      END
