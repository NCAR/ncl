C
C	$Id: gwid2s.f,v 1.2 2000-07-12 16:54:41 haley Exp $
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
      SUBROUTINE GWID2S
C
C  Copy "DEFAULT" attribute context to "SET" context.
C
      include 'gwiast.h'
      include 'gwiadf.h'
C
      INTEGER  I
C
      SAVE
C
C  Polyline attributes.
C
      MSPLIX = MDPLIX
      MSLTYP = MDLTYP
      ASLWSC = ADLWSC
      MSPLCI = MDPLCI
C
C  Polymarker attributes.
C
      MSPMIX = MDPMIX
      MSMTYP = MDMTYP
      ASMSZS = ADMSZS
      MSPMCI = MDPMCI
C
C  Text attributes.
C
      MSTXIX    = MDTXIX
      MSTXP     = MDTXP
      MSTXAL(1) = MDTXAL(1)
      MSTXAL(2) = MDTXAL(2)
      MSCHH     = MDCHH
      DO 10 I=1,4
        MSCHOV(I) = MDCHOV(I)
   10 CONTINUE
      MSTXFO = MDTXFO
      MSTXPR = MDTXPR
      ASCHXP = ADCHXP
      ASCHSP = ADCHSP
      MSTXCI = MDTXCI
C
C  Fill area attributes.
C
      MSFAIX    = MDFAIX
      MSPASZ(1) = MDPASZ(1)
      MSPASZ(2) = MDPASZ(2)
      MSPASZ(3) = MDPASZ(3)
      MSPASZ(4) = MDPASZ(4)
      MSPARF(1) = MDPARF(1)
      MSPARF(2) = MDPARF(2)
      MSFAIS    = MDFAIS
      MSFASI    = MDFASI
      MSFACI    = MDFACI
C
C  Aspect source flags.
C
      DO 20 I=1,13
        MSASF(I) = MDASF(I)
   20 CONTINUE
C
      RETURN
      END
