C
C	$Id: gwidmp.f,v 1.7 2008-07-27 00:21:07 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE GWIDMP(ICNTX,RCNTX)
C
C  This subroutine dumps the current state of all attributes as
C  inherited from GKS at the time of the call.  The attributes
C  are stored in the input arrays ICNTX and RCNTX.  See the awi
C  routine GZSRAT for details on the ordering.
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
      include 'gwienu.h'
C
      INTEGER NUMO(2),RERR,ICNTX(31)
      REAL    RCNTX(19)
      LOGICAL ASFTMP(13)
C
      SAVE
C
C  Clipping indicator, and clip rectangle.
C
      NBYTES = 1+(MEFW-1)/8
      CALL GWPTNI (CLCLIN, IDCLIN, NBYTES, RERR)
      MRCLIP = ICNTX(1)
      CALL GWPTPR (MRCLIP, MEFW,     1, RERR)
C
      NBYTES = 1 + (4*MVDCFW-1)/8
      CALL GWPTNI (CLCREC, IDCREC, NBYTES, RERR)
      MRCREC(1) = INT(REAL(MXSCAL)*RCNTX(12))
      MRCREC(2) = INT(REAL(MYSCAL)*RCNTX(14))
      MRCREC(3) = INT(REAL(MXSCAL)*RCNTX(13))
      MRCREC(4) = INT(REAL(MYSCAL)*RCNTX(15))
      CALL GWPTPR (MRCREC, MVDCFW,     4, RERR)
C
C  Line bundle index.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GWPTNI (CLLBIX,  IDLBIX,  NBYTES,  RERR)
      MRPLIX = ICNTX(2)
      CALL GWPTPR (MRPLIX, MIXFW,  1, RERR)
      MSPLIX         = MRPLIX
      VALCHG(IVPLIX) = .FALSE.
C
C  Line type.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GWPTNI (CLLTYP,  IDLTYP,  NBYTES,  RERR)
      MRLTYP = ICNTX(3)
      CALL GWPTPR (MRLTYP, MIXFW,  1, RERR)
      MSLTYP = MRLTYP
      VALCHG(IVLTYP) = .FALSE.
C
C  Linewidth scale factor.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GWPTNI (CLLWID,  IDLWID,  NBYTES,  RERR)
      ARLWSC = RCNTX(1)
      CALL GFLCNV (ARLWSC,NUMO)
      CALL GWPTPR (NUMO, MCFPP,  2, RERR)
      ASLWSC         = ARLWSC
      VALCHG(IVLWSC) = .FALSE.
C
C  Line color index.
C
      NBYTES = 1+(MCIXFW-1)/8
      CALL GWPTNI (CLLCLR, IDLCLR, NBYTES,  RERR)
      MRPLCI = ICNTX(4)
      CALL GWPTPR (MRPLCI, MCIXFW,  1, RERR)
      MSPLCI         = MRPLCI
      VALCHG(IVPLCI) = .FALSE.
C
C  Marker bundle index.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GWPTNI (CLMBIX, IDMBIX, NBYTES,  RERR)
      MRPMIX = ICNTX(5)
      CALL GWPTPR (MRPMIX, MIXFW,  1, RERR)
      MSPMIX = MRPMIX
      VALCHG(IVPMIX) = .FALSE.
C
C  Marker type.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GWPTNI (CLMTYP, IDMTYP,  NBYTES,  RERR)
      MRMTYP = ICNTX(6)
      CALL GWPTPR (MRMTYP, MIXFW,  1, RERR)
      MSMTYP = MRMTYP
      VALCHG(IVMTYP) = .FALSE.
C
C  Marker size.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GWPTNI (CLMSIZ, IDMSIZ, NBYTES,  RERR)
      ARMSZS = RCNTX(2)
      CALL GFLCNV (ARMSZS,NUMO)
      CALL GWPTPR (NUMO, MCFPP,  2, RERR)
      ASMSZS = ARMSZS
      VALCHG(IVMSZS) = .FALSE.
C
C  Marker color.
C
      NBYTES = 1+(MCIXFW-1)/8
      CALL GWPTNI (CLMCLR, IDMCLR, NBYTES,  RERR)
      MRPMCI = ICNTX(7)
      CALL GWPTPR (MRPMCI, MCIXFW,  1, RERR)
      MSPMCI = MRPMCI
      VALCHG(IVPMCI) = .FALSE.
C
C  Text bundle index.
C
      NBYTES = 1+(MIXFW-1)/8
      CALL GWPTNI (CLTBIX, IDTBIX, NBYTES, RERR)
      MRTXIX = ICNTX(8)
      CALL GWPTPR (MRTXIX, MIXFW, 1, RERR)
      MSTXIX         = MRTXIX
      VALCHG(IVTXIX) = .FALSE.
C
C  Text font.
C
      NBYTES = 1+(MEFW-1)/8
      CALL GWPTNI (CLTFON, IDTFON, NBYTES, RERR)
      MRTXFO = ICNTX(9)
      CALL GWPTPR (MRTXFO, MEFW, 1, RERR)
      MSTXFO         = MRTXFO
      VALCHG(IVTXFO) = .FALSE.
C
C  Text precision.
C
      NBYTES         = 1+(MIXFW-1)/8
      CALL GWPTNI (CLTPRE, IDTPRE, NBYTES, RERR)
      MRTXPR = ICNTX(10)
      CALL GWPTPR (MRTXPR, MIXFW, 1, RERR)
      MSTXPR         = MRTXPR
      VALCHG(IVTXPR) = .FALSE.
C
C  Character expansion factor.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GWPTNI (CLCHEX, IDCHEX, NBYTES, RERR)
      ARCHXP = RCNTX(3)
      CALL GFLCNV (ARCHXP, NUMO)
      CALL GWPTPR (NUMO, MCFPP, 2, RERR)
      ASCHXP         = ARCHXP
      VALCHG(IVCHXP) = .FALSE.
C
C  Character spacing.
C
      NBYTES = 1+(2*MCFPP-1)/8
      CALL GWPTNI (CLCHSP, IDCHSP, NBYTES, RERR)
      ARCHSP = RCNTX(4)
      CALL GFLCNV (ARCHSP, NUMO)
      CALL GWPTPR (NUMO, MCFPP, 2, RERR)
      ASCHSP         = ARCHSP
      VALCHG(IVCHSP) = .FALSE.
C
C  Text color index.
C
      NBYTES = 1+(MCIXFW-1)/8
      CALL GWPTNI (CLTCLR, IDTCLR, NBYTES, RERR)
      MRTXCI = ICNTX(11)
      CALL GWPTPR (MRTXCI, MCIXFW, 1, RERR)
      MSTXCI         = MRTXCI
      VALCHG(IVTXCI) = .FALSE.
C
C  Text path.
C
      NBYTES = 1+(MEFW-1)/8
      CALL GWPTNI (CLTXPA, IDTXPA, NBYTES, RERR)
      MRTXP = ICNTX(12)
      CALL GWPTPR (MRTXP , MEFW,  1, RERR)
      MSTXP         = MRTXP
      VALCHG(IVTXP) = .FALSE.
C
C  Text alignment.
C
      NBYTES = 1+(2*MEFW + 4*MCFPP - 1)/8
      CALL GWPTNI (CLTXAL, IDTXAL, NBYTES, RERR)
      MRTXAL(1) = ICNTX(13)
      MRTXAL(2) = ICNTX(14)
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
      UP   = SQRT(RCNTX(16)**2+RCNTX(17)**2)
      BASE = SQRT(RCNTX(18)**2+RCNTX(19)**2)
      DMAX = MAX(UP,BASE)
      IF (DMAX .GT. 1.0)  THEN
C
C  A vector is longer than 1.0 NDC, scale down both vectors equally.
C
        FACTOR = 1.0/DMAX
        RCNTX(16) = FACTOR*RCNTX(16)
        RCNTX(17) = FACTOR*RCNTX(17)
        RCNTX(18) = FACTOR*RCNTX(18)
        RCNTX(19) = FACTOR*RCNTX(19)
      END IF
C
      RTMP = 0.5+UP*REAL(MYSCAL)
      MRCHH = MAXYVD
      IF (RTMP .LT. REAL(MAXYVD)) MRCHH = INT(RTMP)
      CALL GWPTPR (MRCHH, MVDCFW,  1, RERR)
      MSCHH         = MRCHH
      VALCHG(IVCHH) = .FALSE.
C
C  Character up vector.
C
      NBYTES = 1+(4*MVDCFW-1)/8
      CALL GWPTNI (CLCHOR, IDCHOR, NBYTES, RERR)
      MRCHOV(1) = MIN (MAXYVD, INT (0.5 + MXSCAL*RCNTX(16)))
      MRCHOV(2) = MIN (MAXYVD, INT (0.5 + MYSCAL*RCNTX(17)))
      MRCHOV(3) = MIN (MAXYVD, INT (0.5 + MXSCAL*RCNTX(18)))
      MRCHOV(4) = MIN (MAXYVD, INT (0.5 + MYSCAL*RCNTX(19)))
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
       MRFAIX = ICNTX(15)
       CALL GWPTPR (MRFAIX, MIXFW,  1, RERR)
       MSFAIX = MRFAIX
       VALCHG(IVFAIX) = .FALSE.
C
C  Fill area interior style.
C
       NBYTES = 1+(MEFW-1)/8
       CALL GWPTNI (CLINTS, IDINTS, NBYTES,  RERR)
       MRFAIS = ICNTX(16)
       CALL GWPTPR (MRFAIS, MEFW,  1, RERR)
       MSFAIS = MRFAIS
       VALCHG(IVFAIS) = .FALSE.
C
C  Fill area style index--send pattern index and hatch index.
C
       NBYTES = 1+(MIXFW-1)/8
       CALL GWPTNI (CLHAIX, IDHAIX, NBYTES,  RERR)
       MRFASI = ICNTX(17)
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
       MRFACI = ICNTX(18)
       CALL GWPTPR (MRFACI, MCIXFW,  1, RERR)
       MSFACI = MRFACI
       VALCHG(IVFACI) = .FALSE.
C
C  Pattern size (currently not implemented--be sure to transform
C  the sizes as in the GKS GSPA routine if ever implemented).
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
C  Pattern reference point (currently not implemented--be sure to
C  transform the reference point as in the GKS routine GSPARF if
C  ever implemented).
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
      DO 40 I=1,NGKASF
        ASFCHG(I) = .TRUE.
C 
C  Convert from GKS ASFs to CGM ASFs.
C
        MRASF(I) =  1-ICNTX(I+18)
   40 CONTINUE
      CALL GWISAS (1, RERR)
      CALL GWISAS (2, RERR)
      CALL GWISAS (3, RERR)
      CALL GWISAS (4, RERR)
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
