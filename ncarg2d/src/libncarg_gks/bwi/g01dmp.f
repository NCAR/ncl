C
C	$Id: g01dmp.f,v 1.7 2008-07-27 00:21:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE G01DMP(IOPT)
C
C  If IOPT = 0 this subroutine dumps the current state of all 
C  attributes.  If IOPT = 1, additionally it dumps out the clipping 
C  indicator, the clipping rectangle, and the color table.
C
C
      include 'g01prm.h'
      include 'g01wdt.h'
      include 'g01wsl.h'
      include 'g01adc.h'
      include 'g01rqa.h'
      include 'g01ast.h'
      include 'g01dfa.h'
      include 'g01io.h'
      include 'g01ins.h'
      include 'g01opc.h'
      include 'gksenu.h'
C
      INTEGER NUMO(2),RERR
      INTEGER ICTMP(3)
C
      IF (IOPT .EQ. 1) THEN
C
C  Clipping indicator, and clip rectangle.
C
        NBYTES = 1+(MEFW-1)/8
        CALL GPUTNI (CLCLIN, IDCLIN, NBYTES, RERR)
        CALL GPUTPR (MRCLIP, MEFW,     1, RERR)
        NBYTES = 1 + (4*MVDCFW-1)/8
        CALL GPUTNI (CLCREC, IDCREC, NBYTES, RERR)
        CALL GPUTPR (MRCREC, MVDCFW,     4, RERR)
C
C  Color table.
C
        DO 10 I=1,MOL
          ICTMP(1) = REAL(MDCCRG)*SRED(I)
          ICTMP(2) = REAL(MDCCRG)*SGREEN(I)
          ICTMP(3) = REAL(MDCCRG)*SBLUE(I)
          NCIX = MCOLI(I)
C
C  Don't copy over background color.
C
          IF (NCIX .EQ. 0) GO TO 10
          NBYTES = 1 + (3*MDCCFW + MCIXFW - 1)/8
          CALL GPUTNI (CLCTBL, IDCTBL, NBYTES, RERR)
          IF (RERR .NE. 0)  RETURN
          CALL GPUTPR (NCIX, MCIXFW, 1, RERR)
          IF (RERR .NE. 0)  RETURN
          CALL GPUTPR (ICTMP, MDCCFW, 3, RERR)
   10   CONTINUE
      ENDIF
C
C  Line bundle index.
C
C     NBYTES = 1+(MIXFW-1)/8
C     CALL GPUTNI (CLLBIX,  IDLBIX,  NBYTES,  RERR)
C     CALL GPUTPR (MRPLIX, MIXFW,  1, RERR)
C     MSPLIX         = MRPLIX
C     VALCHG(IVPLIX) = .FALSE.
C
C  Line type.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GPUTNI (CLLTYP,  IDLTYP,  NBYTES,  RERR)
      CALL GPUTPR (MRLTYP, MIXFW,  1, RERR)
      MSLTYP = MRLTYP
      VALCHG(IVLTYP) = .FALSE.
C
C  Linewidth scale factor.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GPUTNI (CLLWID,  IDLWID,  NBYTES,  RERR)
      CALL GFLCNV (ARLWSC,NUMO)
      CALL GPUTPR (NUMO, MCFPP,  2, RERR)
      ASLWSC         = ARLWSC
      VALCHG(IVLWSC) = .FALSE.
C
C  Line color index.
C
      NBYTES = 1+(MCIXFW-1)/8
      CALL GPUTNI (CLLCLR, IDLCLR, NBYTES,  RERR)
      CALL GPUTPR (MRPLCI, MCIXFW,  1, RERR)
      MSPLCI         = MRPLCI
      VALCHG(IVPLCI) = .FALSE.
C
C  Marker bundle index.
C
C     NBYTES = 1+(MIXFW-1)/8
C     CALL GPUTNI (CLMBIX, IDMBIX, NBYTES,  RERR)
C     CALL GPUTPR (MRPMIX, MIXFW,  1, RERR)
C     MSPMIX = MRPMIX
C     VALCHG(IVPMIX) = .FALSE.
C
C  Marker type.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GPUTNI (CLMTYP, IDMTYP,  NBYTES,  RERR)
      CALL GPUTPR (MRMTYP, MIXFW,  1, RERR)
      MSMTYP = MRMTYP
      VALCHG(IVMTYP) = .FALSE.
C
C  Marker size.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GPUTNI (CLMSIZ, IDMSIZ, NBYTES,  RERR)
      CALL GFLCNV (ARMSZS,NUMO)
      CALL GPUTPR (NUMO, MCFPP,  2, RERR)
      ASMSZS = ARMSZS
      VALCHG(IVMSZS) = .FALSE.
C
C  Marker color.
C
      NBYTES = 1+(MCIXFW-1)/8
      CALL GPUTNI (CLMCLR, IDMCLR, NBYTES,  RERR)
      CALL GPUTPR (MRPMCI, MCIXFW,  1, RERR)
      MSPMCI = MRPMCI
      VALCHG(IVPMCI) = .FALSE.
C
C  Text bundle index.
C
C     NBYTES = 1+(MIXFW-1)/8
C     CALL GPUTNI (CLTBIX, IDTBIX, NBYTES, RERR)
C     CALL GPUTPR (MRTXIX, MIXFW, 1, RERR)
C     MSTXIX         = MRTXIX
C     VALCHG(IVTXIX) = .FALSE.
C
C  Text font.
C
      NBYTES = 1+(MEFW-1)/8
      CALL GPUTNI (CLTFON, IDTFON, NBYTES, RERR)
      CALL GPUTPR (MRTXFO, MEFW, 1, RERR)
      MSTXFO         = MRTXFO
      VALCHG(IVTXFO) = .FALSE.
C
C  Text precision.
C
      NBYTES         = 1+(MIXFW-1)/8
      CALL GPUTNI (CLTPRE, IDTPRE, NBYTES, RERR)
      CALL GPUTPR (MRTXPR, MIXFW, 1, RERR)
      MSTXPR         = MRTXPR
      VALCHG(IVTXPR) = .FALSE.
C
C  Character expansion factor.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GPUTNI (CLCHEX, IDCHEX, NBYTES, RERR)
      CALL GFLCNV (ARCHXP, NUMO)
      CALL GPUTPR (NUMO, MCFPP, 2, RERR)
      ASCHXP         = ARCHXP
      VALCHG(IVCHXP) = .FALSE.
C
C  Character spacing.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GPUTNI (CLCHSP, IDCHSP, NBYTES, RERR)
      CALL GFLCNV (ARCHSP, NUMO)
      CALL GPUTPR (NUMO, MCFPP, 2, RERR)
      ASCHSP         = ARCHSP
      VALCHG(IVCHSP) = .FALSE.
C
C  Text color index.
C
      NBYTES = 1+(MCIXFW-1)/8
      CALL GPUTNI (CLTCLR, IDTCLR, NBYTES, RERR)
      CALL GPUTPR (MRTXCI, MCIXFW, 1, RERR)
      MSTXCI         = MRTXCI
      VALCHG(IVTXCI) = .FALSE.
C
C  Text path.
C
      NBYTES = 1+(MEFW-1)/8
      CALL GPUTNI (CLTXPA, IDTXPA, NBYTES, RERR)
      CALL GPUTPR (MRTXP , MEFW,  1, RERR)
      MSTXP         = MRTXP
      VALCHG(IVTXP) = .FALSE.
C
C  Text alignment.
C
      NBYTES = 1+(2*MEFW + 4*MCFPP - 1)/8
      CALL GPUTNI (CLTXAL, IDTXAL, NBYTES, RERR)
      CALL GPUTPR (MRTXAL, MEFW, 2, RERR)
      NUMO(1) = 0
      NUMO(2) = 0
      CALL GPUTPR (NUMO, MCFPP, 2, RERR)
      CALL GPUTPR (NUMO, MCFPP, 2, RERR)
      MSTXAL(1)      = MRTXAL(1)
      MSTXAL(2)      = MRTXAL(2)
      VALCHG(IVTXAL) = .FALSE.
C
C  Character height.
C
      NBYTES = 1+(MVDCFW-1)/8
      CALL GPUTNI (CLCHHT, IDCHHT, NBYTES, RERR)
      CALL GPUTPR (MRCHH, MVDCFW,  1, RERR)
      MSCHH         = MRCHH
      VALCHG(IVCHH) = .FALSE.
C
C  Character up vector.
C
      NBYTES = 1+(4*MVDCFW-1)/8
      CALL GPUTNI (CLCHOR, IDCHOR, NBYTES, RERR)
      CALL GPUTPR (MRCHOV(1) , MVDCFW,  4, RERR)
      MSCHOV(1)      = MRCHOV(1)
      MSCHOV(2)      = MRCHOV(2)
      MSCHOV(3)      = MRCHOV(3)
      MSCHOV(4)      = MRCHOV(4)
      VALCHG(IVCHOV) = .FALSE.
C
C  Fill bundle index (currently disabled).
C
C      NBYTES = 1+(MIXFW-1)/8
C      CALL GPUTNI (CLFBIX, IDFBIX,  NBYTES,  RERR)
C      CALL GPUTPR (MRFAIX, MIXFW,  1, RERR)
C      MSFAIX = MRFAIX
C      VALCHG(IVFAIX) = .FALSE.
C
C  Fill area interior style.
C
       NBYTES = 1+(MEFW-1)/8
       CALL GPUTNI (CLINTS, IDINTS, NBYTES,  RERR)
       CALL GPUTPR (MRFAIS, MEFW,  1, RERR)
       MSFAIS = MRFAIS
       VALCHG(IVFAIS) = .FALSE.
C
C  Fill area style index--pattern index (currently disabled) 
C                         and hatch index.
C
       NBYTES = 1+(MIXFW-1)/8
       CALL GPUTNI (CLHAIX, IDHAIX, NBYTES,  RERR)
       CALL GPUTPR (MRFASI, MIXFW, 1, RERR)
C      CALL GPUTNI (CLPTIX, IDPTIX, NBYTES,  RERR)
C      CALL GPUTPR (MRFASI, MIXFW, 1, RERR)
       MSFASI = MRFASI
       VALCHG(IVFASI) = .FALSE.
C
C  Fill area color index.
C
       NBYTES = 1+(MCIXFW-1)/8
       CALL GPUTNI (CLFCLR, IDFCLR, NBYTES, RERR)
       CALL GPUTPR (MRFACI, MCIXFW,  1, RERR)
       MSFACI = MRFACI
       VALCHG(IVFACI) = .FALSE.
C
C  Pattern size (currently disabled).
C
C      NBYTES = 1+(4*MVDCFW-1)/8
C      CALL GPUTNI (CLPTSZ, IDPTSZ, NBYTES, RERR)
C      CALL GPUTPR (MRPASZ(1) , MVDCFW,  4, RERR)
C      MSPASZ(1) = MRPASZ(1)
C      MSPASZ(2) = MRPASZ(2)
C      MSPASZ(3) = MRPASZ(3)
C      MSPASZ(4) = MRPASZ(4)
C      VALCHG(IVPASZ) = .FALSE.
C
C  Pattern reference point (currently disabled).
C
C      NBYTES = 1+(2*MVDCFW-1)/8
C      CALL GPUTNI (CLFRPT, IDFRPT, NBYTES, RERR)
C      CALL GPUTPR (MRPARF(1), MVDCFW, 2, RERR)
C      MSPARF(1) = MRPARF(1)
C      MSPARF(2) = MRPARF(2)
C      VALCHG(IVPARF) = .FALSE.
C
C  Aspect source flags.
C
      CALL G01SAS (1, RERR)
      CALL G01SAS (2, RERR)
      CALL G01SAS (3, RERR)
      CALL G01SAS (4, RERR)
C
C  Clear aggregate change variable.
C
      AGPEND(1) = .FALSE.
      AGPEND(2) = .FALSE.
      AGPEND(3) = .FALSE.
      AGPEND(4) = .FALSE.
C
      RETURN
      END
