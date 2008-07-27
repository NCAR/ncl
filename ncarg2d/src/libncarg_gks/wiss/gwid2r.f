C
C	$Id: gwid2r.f,v 1.4 2008-07-27 00:21:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWID2R
C
C  Copy "DEFAULT" attribute context to "REQUESTED" context.
C
      include 'gwiarq.h'
      include 'gwiadf.h'
C
      INTEGER  I
C
      SAVE
C
C  Polyline attributes.
C
      MRPLIX = MDPLIX
      MRLTYP = MDLTYP
      ARLWSC = ADLWSC
      MRPLCI = MDPLCI
C
C  Polymarker attributes.
C
      MRPMIX = MDPMIX
      MRMTYP = MDMTYP
      ARMSZS = ADMSZS
      MRPMCI = MDPMCI
C
C  Text attributes.
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
C  Fill area attributes.
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
C  Aspect source flags.
C
      DO 20 I=1,13
        MRASF(I) = MDASF(I)
   20 CONTINUE
C
      RETURN
      END
