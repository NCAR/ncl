C
C	$Id: gupasf.f,v 1.4 2000-07-12 16:51:03 haley Exp $
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
        SUBROUTINE GUPASF
C
C  Update aspect source flag context.
C
      include 'gksin.h'
      include 'g01arq.h'
      include 'g01ast.h'
      include 'g01adc.h'
C
      INTEGER  I
C
C  Compute change flags for each GKS ASF;  compute aggregate.
C  The CGM values for ASFs are reversed from those in GKS, so
C  we perform that complement here.
C
      ANYASF = .FALSE.
      DO 435 I=1,NGKASF
        MRASF(I) = 1-ID(I)
        ASFCHG(I) = MRASF(I).NE.MSASF(I)
        ANYASF = ANYASF .OR. ASFCHG(I)
  435 CONTINUE
C
C  Compute polyline aggregrate variable.
C  (Note that the logic relies on ASF pointers being a 
C   contiguous sequence).       
C
      DO 436 I=IALTYP,IAPLCI
        AGPEND(1) = AGPEND(1) .OR. ASFCHG(I)
  436 CONTINUE
C
C  Compute polymarker aggregrate variables.
C  (Note that the logic relies on ASF pointers being a
C   contiguous sequence).
C
      DO 437 I=IAMTYP,IAPMCI
        AGPEND(2) = AGPEND(2) .OR. ASFCHG(I)
  437 CONTINUE
C
C  Compute text aggregrate variables.
C  (Note that the logic relies on ASF pointers being a
C   contiguous sequence).
C
      DO 438 I=IATXFP,IATXCI
        AGPEND(3) = AGPEND(3) .OR. ASFCHG(I)
  438 CONTINUE
C
C  Compute fill area aggregrate variables.
C  (Note that the logic relies on ASF pointers being a
C   contiguous sequence).
C
      DO 439 I=IAFAIS,IAFACI
        AGPEND(4) = AGPEND(4) .OR. ASFCHG(I)
  439 CONTINUE
C
      RETURN
      END
