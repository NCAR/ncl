C
C	$Id: gwidmp.f,v 1.1 1993-01-09 02:09:16 fred Exp $
C
      SUBROUTINE GWIDMP
C
C  This subroutine dumps the current state of all attributes.
C  It also dumps out the clipping indicator and the clip rectangle.
C
C
      include 'gwiwdt.h'
      include 'gwiwsl.h'
      include 'gwiadc.h'
      include 'gwiarq.h'
      include 'gwiast.h'
      include 'gwiadf.h'
      include 'gwiio.h'
      include 'gwiins.h'
      include 'gwiopc.h'
      include 'gksenu.h'
C
      INTEGER NUMO(2),RERR
      LOGICAL ASFTMP(13)
C
      SAVE
C
C  Clipping indicator, and clip rectangle.
C
      NBYTES = 1+(MEFW-1)/8
      CALL GWPTNI (CLCLIN, IDCLIN, NBYTES, RERR)
      CALL GWPTPR (MRCLIP, MEFW,     1, RERR)
      NBYTES = 1 + (4*MVDCFW-1)/8
      CALL GWPTNI (CLCREC, IDCREC, NBYTES, RERR)
      CALL GWPTPR (MRCREC, MVDCFW,     4, RERR)
C
C  Line bundle index.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GWPTNI (CLLBIX,  IDLBIX,  NBYTES,  RERR)
      CALL GWPTPR (MRPLIX, MIXFW,  1, RERR)
      MSPLIX         = MRPLIX
      VALCHG(IVPLIX) = .FALSE.
C
C  Line type.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GWPTNI (CLLTYP,  IDLTYP,  NBYTES,  RERR)
      CALL GWPTPR (MRLTYP, MIXFW,  1, RERR)
      MSLTYP = MRLTYP
      VALCHG(IVLTYP) = .FALSE.
C
C  Linewidth scale factor.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GWPTNI (CLLWID,  IDLWID,  NBYTES,  RERR)
      CALL GFLCNV (ARLWSC,NUMO)
      CALL GWPTPR (NUMO, MCFPP,  2, RERR)
      ASLWSC         = ARLWSC
      VALCHG(IVLWSC) = .FALSE.
C
C  Line color index.
C
      NBYTES = 1+(MCIXFW-1)/8
      CALL GWPTNI (CLLCLR, IDLCLR, NBYTES,  RERR)
      CALL GWPTPR (MRPLCI, MCIXFW,  1, RERR)
      MSPLCI         = MRPLCI
      VALCHG(IVPLCI) = .FALSE.
C
C  Marker bundle index.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GWPTNI (CLMBIX, IDMBIX, NBYTES,  RERR)
      CALL GWPTPR (MRPMIX, MIXFW,  1, RERR)
      MSPMIX = MRPMIX
      VALCHG(IVPMIX) = .FALSE.
C
C  Marker type.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GWPTNI (CLMTYP, IDMTYP,  NBYTES,  RERR)
      CALL GWPTPR (MRMTYP, MIXFW,  1, RERR)
      MSMTYP = MRMTYP
      VALCHG(IVMTYP) = .FALSE.
C
C  Marker size.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GWPTNI (CLMSIZ, IDMSIZ, NBYTES,  RERR)
      CALL GFLCNV (ARMSZS,NUMO)
      CALL GWPTPR (NUMO, MCFPP,  2, RERR)
      ASMSZS = ARMSZS
      VALCHG(IVMSZS) = .FALSE.
C
C  Marker color.
C
      NBYTES = 1+(MCIXFW-1)/8
      CALL GWPTNI (CLMCLR, IDMCLR, NBYTES,  RERR)
      CALL GWPTPR (MRPMCI, MCIXFW,  1, RERR)
      MSPMCI = MRPMCI
      VALCHG(IVPMCI) = .FALSE.
C
C  Text bundle index.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GWPTNI (CLTBIX, IDTBIX, NBYTES, RERR)
      CALL GWPTPR (MRTXIX, MIXFW, 1, RERR)
      MSTXIX         = MRTXIX
      VALCHG(IVTXIX) = .FALSE.
C
C  Text font.
C
      NBYTES = 1+(MEFW-1)/8
      CALL GWPTNI (CLTFON, IDTFON, NBYTES, RERR)
      CALL GWPTPR (MRTXFO, MEFW, 1, RERR)
      MSTXFO         = MRTXFO
      VALCHG(IVTXFO) = .FALSE.
C
C  Text precision.
C
      NBYTES         = 1+(MIXFW-1)/8
      CALL GWPTNI (CLTPRE, IDTPRE, NBYTES, RERR)
      CALL GWPTPR (MRTXPR, MIXFW, 1, RERR)
      MSTXPR         = MRTXPR
      VALCHG(IVTXPR) = .FALSE.
C
C  Character expansion factor.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GWPTNI (CLCHEX, IDCHEX, NBYTES, RERR)
      CALL GFLCNV (ARCHXP, NUMO)
      CALL GWPTPR (NUMO, MCFPP, 2, RERR)
      ASCHXP         = ARCHXP
      VALCHG(IVCHXP) = .FALSE.
C
C  Character spacing.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GWPTNI (CLCHSP, IDCHSP, NBYTES, RERR)
      CALL GFLCNV (ARCHSP, NUMO)
      CALL GWPTPR (NUMO, MCFPP, 2, RERR)
      ASCHSP         = ARCHSP
      VALCHG(IVCHSP) = .FALSE.
C
C  Text color index.
C
      NBYTES = 1+(MCIXFW-1)/8
      CALL GWPTNI (CLTCLR, IDTCLR, NBYTES, RERR)
      CALL GWPTPR (MRTXCI, MCIXFW, 1, RERR)
      MSTXCI         = MRTXCI
      VALCHG(IVTXCI) = .FALSE.
C
C  Text path.
C
      NBYTES = 1+(MEFW-1)/8
      CALL GWPTNI (CLTXPA, IDTXPA, NBYTES, RERR)
      CALL GWPTPR (MRTXP , MEFW,  1, RERR)
      MSTXP         = MRTXP
      VALCHG(IVTXP) = .FALSE.
C
C  Text alignment.
C
      NBYTES = 1+(2*MEFW + 4*MCFPP - 1)/8
      CALL GWPTNI (CLTXAL, IDTXAL, NBYTES, RERR)
      CALL GWPTPR (MRTXAL, MEFW, 2, RERR)
      NUMO(1) = 0
      NUMO(2) = 0
      CALL GWPTPR (NUMO, MCFPP, 2, RERR)
      CALL GWPTPR (NUMO, MCFPP, 2, RERR)
      MSTXAL(1)      = MRTXAL(1)
      MSTXAL(2)      = MRTXAL(2)
      VALCHG(IVTXAL) = .FALSE.
C
C  Character height.
C
      NBYTES = 1+(MVDCFW-1)/8
      CALL GWPTNI (CLCHHT, IDCHHT, NBYTES, RERR)
      CALL GWPTPR (MRCHH, MVDCFW,  1, RERR)
      MSCHH         = MRCHH
      VALCHG(IVCHH) = .FALSE.
C
C  Character up vector.
C
      NBYTES = 1+(4*MVDCFW-1)/8
      CALL GWPTNI (CLCHOR, IDCHOR, NBYTES, RERR)
      CALL GWPTPR (MRCHOV(1) , MVDCFW,  4, RERR)
      MSCHOV(1)      = MRCHOV(1)
      MSCHOV(2)      = MRCHOV(2)
      MSCHOV(3)      = MRCHOV(3)
      MSCHOV(4)      = MRCHOV(4)
      VALCHG(IVCHOV) = .FALSE.
C
C  Fill bundle index.
C
       NBYTES = 1+(MIXFW-1)/8
       CALL GWPTNI (CLFBIX, IDFBIX,  NBYTES,  RERR)
       CALL GWPTPR (MRFAIX, MIXFW,  1, RERR)
       MSFAIX = MRFAIX
       VALCHG(IVFAIX) = .FALSE.
C
C  Fill area interior style.
C
       NBYTES = 1+(MEFW-1)/8
       CALL GWPTNI (CLINTS, IDINTS, NBYTES,  RERR)
       CALL GWPTPR (MRFAIS, MEFW,  1, RERR)
       MSFAIS = MRFAIS
       VALCHG(IVFAIS) = .FALSE.
C
C  Fill area style index--send pattern index and hatch index.
C
       NBYTES = 1+(MIXFW-1)/8
       CALL GWPTNI (CLHAIX, IDHAIX, NBYTES,  RERR)
       CALL GWPTPR (MRFASI, MIXFW, 1, RERR)
       CALL GWPTNI (CLPTIX, IDPTIX, NBYTES,  RERR)
       CALL GWPTPR (MRFASI, MIXFW, 1, RERR)
       MSFASI = MRFASI
       VALCHG(IVFASI) = .FALSE.
C
C  Fill area color index.
C
       NBYTES = 1+(MCIXFW-1)/8
       CALL GWPTNI (CLFCLR, IDFCLR, NBYTES, RERR)
       CALL GWPTPR (MRFACI, MCIXFW,  1, RERR)
       MSFACI = MRFACI
       VALCHG(IVFACI) = .FALSE.
C
C  Pattern size (currently not implemented).
C
C      NBYTES = 1+(4*MVDCFW-1)/8
C      CALL GWPTNI (CLPTSZ, IDPTSZ, NBYTES, RERR)
C      CALL GWPTPR (MRPASZ(1) , MVDCFW,  4, RERR)
C      MSPASZ(1) = MRPASZ(1)
C      MSPASZ(2) = MRPASZ(2)
C      MSPASZ(3) = MRPASZ(3)
C      MSPASZ(4) = MRPASZ(4)
C      VALCHG(IVPASZ) = .FALSE.
C
C  Pattern reference point (currently not implemented).
C
C      NBYTES = 1+(2*MVDCFW-1)/8
C      CALL GWPTNI (CLFRPT, IDFRPT, NBYTES, RERR)
C      CALL GWPTPR (MRPARF(1), MVDCFW, 2, RERR)
C      MSPARF(1) = MRPARF(1)
C      MSPARF(2) = MRPARF(2)
C      VALCHG(IVPARF) = .FALSE.
C
C  Aspect source flags.
C
      DO 20 I=1,NGKASF
        ASFTMP(I) = ASFCHG(I)
        ASFCHG(I) = .TRUE.
   20 CONTINUE
      CALL GWISAS (1, RERR)
      CALL GWISAS (2, RERR)
      CALL GWISAS (3, RERR)
      CALL GWISAS (4, RERR)
      DO 30 I=1,NGKASF
        ASFCHG(I) = ASFTMP(I)
   30 CONTINUE
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
