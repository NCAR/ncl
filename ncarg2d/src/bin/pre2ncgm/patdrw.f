C
C	$Id: patdrw.f,v 1.5 2008-07-27 00:59:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PATDRW (IX, IY, IPEN, IOS, STATUS)
C
C  DRAW THE REQUESTED DASH PATTERN
C
C  INPUT
C       IX, IY - THE COORDINATES FOR THE MOVE OR DRAW IN VDC
C       IPEN - THE MOVE DRAW FLAG
C  OUTPUT
C       IOS - THE I/O STATUS FLAG VALID WHEN STATUS SET TO AN I/O ERROR
C       STATUS - THE STATUS FLAG DEFINED BY COMMON TREROR
C
      COMMON /TRDASH/ PATFLG, DASPAT, PATBIT, DEFPAT, DASHX, DASHY,
     1                PATVDC
      INTEGER DPATSZ, PATSIZ, DFPTSZ
      PARAMETER (DPATSZ=16, PATSIZ=320, DFPTSZ=4)
      INTEGER PATFLG, DASPAT, PATBIT, DEFPAT(DFPTSZ), DASHX,
     1        DASHY, PATVDC
      COMMON/TREROR/ ALLOK, MFRCHK, MTOPER, METRDC, REDERR, TYPCHG
     1             ,INVTYP, MINVLD, TYPERR, FRMEND, ENCINT, IVDCDT
     2             ,GCOERR, GCRERR, GCCERR, FCOERR, FCRERR, FCCERR
     3             ,PLIDXG, PMIDXG, TXIDXG, PGIDXG, INVLMT, CELERR
     4             ,COIERR, COLNRM, UNKNOW, UNKOPC, ENDMTF, VNEROR
     5             ,BADRSZ, DEVOUT, NOVERS, BADFNT, PGMERR, FASERR
     6             ,HINERR, VDWERR, RDWERR, RIXLIM
      INTEGER        ALLOK, MFRCHK, MTOPER, METRDC, REDERR, TYPCHG
     1             ,INVTYP, MINVLD, TYPERR, FRMEND, ENCINT, IVDCDT
     2             ,GCOERR, GCRERR, GCCERR, FCOERR, FCRERR, FCCERR
     3             ,PLIDXG, PMIDXG, TXIDXG, PGIDXG, INVLMT, CELERR
     4             ,COIERR, COLNRM, UNKNOW, UNKOPC, ENDMTF, VNEROR
     5             ,BADRSZ, DEVOUT, NOVERS, BADFNT, PGMERR, FASERR
     6             ,HINERR, VDWERR, RDWERR, RIXLIM
      COMMON /TRTYPE/ METMIN, METEXT, METPRT, METHED, MIOPCL, MXOPCL,
     1                MIOPID, MXOPID, ASCDEC, ASCHEX, ASCOCT, BINARY,
     2                GAHNOR, GALEFT, GACENT, GARITE, GAVNOR, GATOP ,
     3                GACAP , GAHALF, GABASE, GABOTT, GRIGHT, GLEFT ,
     4                GUP   ,
     5                GDOWN , CINDEX, CDIRCT, PENUP , PENDN , FLAGVL,
     6                SEPRVL, ASCTEK, TYPRGB, TYPBGR,
     7                TYPHLS, TYPMON, SOLID , DASHED, INVSBL, SOLPAT,
     8                ASFIND, ASFBND, ASFLNT, ASFLNW, ASFLNC, ASFMRT,
     9                ASFMRS, ASFMRC, ASFFIS, ASFFHI, ASFFPI, ASFFCO,
     A                ASFFPT, ASFFPW, ASFFPC, ASFTFI, ASFTPR, ASFTCX,
     B                ASFTCS, ASFTCO, ASCFLT, PHOLLO, PSOLID, PPATTR,
     C                PHATCH, PEMPTY, HHORIZ, HVERTI, HPOSLP, HNESLP,
     D                HHOAVE, HPOANE
      INTEGER         METMIN, METEXT, METPRT, METHED, MIOPCL, MXOPCL,
     1                MIOPID, MXOPID, ASCDEC, ASCHEX, ASCOCT, BINARY,
     2                GAHNOR, GALEFT, GACENT, GARITE, GAVNOR, GATOP ,
     3                GACAP , GAHALF, GABASE, GABOTT, GRIGHT, GLEFT ,
     4                GUP   ,
     5                GDOWN , CINDEX, CDIRCT, PENUP , PENDN , FLAGVL,
     6                SEPRVL, ASCTEK, TYPRGB, TYPBGR,
     7                TYPHLS, TYPMON, SOLID , DASHED, INVSBL, SOLPAT,
     8                ASFIND, ASFBND, ASFLNT, ASFLNW, ASFLNC, ASFMRT,
     9                ASFMRS, ASFMRC, ASFFIS, ASFFHI, ASFFPI, ASFFCO,
     A                ASFFPT, ASFFPW, ASFFPC, ASFTFI, ASFTPR, ASFTCX,
     B                ASFTCS, ASFTCO, ASCFLT, PHOLLO, PSOLID, PPATTR,
     C                PHATCH, PEMPTY, HHORIZ, HVERTI, HPOSLP, HNESLP,
     D                HHOAVE, HPOANE
      COMMON /PTRPARS/  MOPLEN, MCICPR, MCDCPR, MFLCPR, MIXCPR, MWHCPR,
     1                 MOPDLN, MCIDPR, MCDDPR, MFLDPR, MIXDPR, MWHDPR,
     2                 BYTSIZ, CONFLG, MENDPR, MENCPR
      INTEGER MOPCLL, MOPIDL, MSCLEN, MLGFLG, MCOLEN, MLOLEN
      PARAMETER (MOPCLL=4, MOPIDL=7, MCOLEN=1, MSCLEN=5, MLOLEN=15,
     1           MLGFLG=31)
       INTEGER         MOPLEN, MCICPR, MCDCPR, MFLCPR, MIXCPR, MWHCPR,
     1                 MOPDLN, MCIDPR, MCDDPR, MFLDPR, MIXDPR, MWHDPR,
     2                 BYTSIZ, CONFLG, MENDPR, MENCPR
C
      INTEGER IX, IY, IPEN, IOS, STATUS
C
      INTEGER DIST, IP, NX, NY, XD, YD, PDIST
      REAL XMV, YMV
      REAL FLOAT
C
      SAVE PDIST
C
C  IF PATTERN SOLID THEN PASS ON COORDINATES
C
      IF (PATFLG .EQ. SOLID) THEN
                CALL PLTCLP (IX, IY, IPEN, IOS, STATUS)
      ELSE IF (PATFLG .EQ. DASHED) THEN
C
C       DASHED PATTERN
C
        IF (IPEN .EQ. PENUP) THEN
C
C               RESET THE POINTERS THIS IS A MOVE
C
                PDIST = PATVDC
                PATBIT = 0
                DASHX = IX
                DASHY = IY
                CALL PLTCLP (IX, IY, IPEN,IOS, STATUS)
        ELSE
C
C               PEN DOWN SO COMPUTE DISTANCE
C
                XD = IX - DASHX
                YD = IY - DASHY
                DIST = SQRT(FLOAT((XD*XD) + (YD*YD)))
C
C               IF NO DISTANCE THEN RETURN
C
                IF ( DIST .EQ. 0) RETURN
C
                XMV = FLOAT(XD) / FLOAT(DIST)
                YMV = FLOAT(YD) / FLOAT(DIST)
C
C               LOOP FOR ALL DIST
C
 100            CONTINUE
                CALL GBYTES(DASPAT, IP, PATBIT, 1, 0, 1)
                IF (DIST .LE. PDIST) THEN
                        PDIST = PDIST - DIST
                        DIST = 0
                        NX = IX
                        NY = IY
                ELSE
                        NX = FLOAT(DASHX) + FLOAT(PDIST)*XMV
                        NY = FLOAT(DASHY) + FLOAT(PDIST)*YMV
                        DIST = DIST - PDIST
                        PATBIT = PATBIT + 1
                        PDIST = PATVDC
                        IF (PATBIT .EQ. DPATSZ) PATBIT = 0
                END IF
                CALL PLTCLP (NX, NY, IP, IOS, STATUS)
                IF (STATUS .NE. ALLOK) RETURN
                DASHX = NX
                DASHY = NY
C
C               CHECK IF MORE DRAWING REQUIRED
C
                IF (DIST .GT. 0) GO TO 100
C
        END IF
      END IF
C
      RETURN
      END
