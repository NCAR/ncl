C
C	$Id: g01d2s.f,v 1.2 1993-01-09 02:05:52 fred Exp $
C
      SUBROUTINE G01D2S
C
C  Copy "DEFAULT" attribute context to "SET" context.
C
C  (There is now sufficient information in /G01ADC/
C  to put a DO loop structure in to effect the copy,
C  using the equivalencing arrays and the sign of IP2AEA).
C
      include 'g01prm.h'
      include 'g01ast.h'
      include 'g01adf.h'
C
      INTEGER  I
C
C  POLYLINE attributes.
C
      MSPLIX = MDPLIX
      MSLTYP = MDLTYP
      ASLWSC = ADLWSC
      MSPLCI = MDPLCI
C
C  POLYMARKER attributes.
C
      MSPMIX = MDPMIX
      MSMTYP = MDMTYP
      ASMSZS = ADMSZS
      MSPMCI = MDPMCI
C
C  TEXT attributes.
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
C  FILL AREA attributes.
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
C  ASPECT SOURCE FLAGS.
C
      DO 20 I=1,13
        MSASF(I) = MDASF(I)
   20 CONTINUE
C
      RETURN
      END
