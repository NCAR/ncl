C
C	$Id: g01d2r.f,v 1.5 2003-02-13 23:58:20 fred Exp $
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
      SUBROUTINE G01D2R
C
C  Copy "DEFAULT" attribute context to "REQUESTED" cOntext.
C
C  (There is now sufficient information in /G01ADC/ to put a DO loop 
C  structure in to effect the copy, using the equivalencing arrays 
C  and the sign of IP2AEA).
C
      include 'g01prm.h'
      include 'g01rqa.h'
      include 'g01dfa.h'
C
      INTEGER  I
C
C  POLYLINE attributes.
C
      MRPLIX = MDPLIX
      MRLTYP = MDLTYP
      ARLWSC = ADLWSC
      MRPLCI = MDPLCI
C
C  POLYMARKER attributes.
C
      MRPMIX = MDPMIX
      MRMTYP = MDMTYP
      ARMSZS = ADMSZS
      MRPMCI = MDPMCI
C
C  TEXT attributes.
C
      MRTXIX    = MDTXIX
      MRTXP     = MDTXP
      MRTXAL(1) = MDTXAL(1)
      MRTXAL(2) = MDTXAL(2)
      MRCHH     = MDCHH
      DO 10 I=1,4
        MRCHOV(I) = MDCHOV(I)
   10 CONTINUE
      MRTXFO = MDTXFO
      MRTXPR = MDTXPR
      ARCHXP = ADCHXP
      ARCHSP = ADCHSP
      MRTXCI = MDTXCI
C
C  FILL AREA ATTRIBUTES.
C
      MRFAIX    = MDFAIX
      MRPASZ(1) = MDPASZ(1)
      MRPASZ(2) = MDPASZ(2)
      MRPASZ(3) = MDPASZ(3)
      MRPASZ(4) = MDPASZ(4)
      MRPARF(1) = MDPARF(1)
      MRPARF(2) = MDPARF(2)
      MRFAIS    = MDFAIS
      MRFASI    = MDFASI
      MRFACI    = MDFACI
C
C  ASPECT SOURCE FLAGS.
C
      DO 20 I=1,13
        MRASF(I) = MDASF(I)
   20 CONTINUE
C
      RETURN
      END
