C
C	$Id: pnt.f,v 1.2 2000-07-12 17:04:36 haley Exp $
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
      SUBROUTINE PNT(IOS,STATUS)
C
C  SET UP THE POINTS MODE DATA
C
C  OUTPUT
C       IOS- THE I/O STATUS VALID WHEN STATUS SET TO AN I/O ERROR
C       STATUS-THE ERROR STATUS DEFINED BY COMMON TRERO
C
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
      COMMON /TRINST/ OPCL, OPID, LEN, CONT
      INTEGER OPCL, OPID, LEN
      LOGICAL CONT
      COMMON /TRMTYP/ FOURBT, MULTBT, TWOBT, PTSFLG, CURCOL, IPWRIT,
     1                IFRAME, ISETOP, IRANG, IPTS, ICAS, IINT, IORNT,
     2                ISIZE, IFONT, IPAT, ISSIZ, ICENT, ICOLR,
     3                MCENT, MSIZE, MOR, OLDX, OLDY, MCASE, MINTEN,
     4                MFONT, MDASH, MSPOT, MCOLOR, MLIMIT, PTSYM,
     5                SAVX, SAVY
      INTEGER FOURBT, MULTBT, TWOBT, PTSFLG, CURCOL, IPWRIT,
     1                IFRAME, ISETOP, IRANG, IPTS, ICAS, IINT, IORNT,
     2                ISIZE, IFONT, IPAT, ISSIZ, ICENT, ICOLR,
     3                MCENT, MSIZE, MOR, OLDX, OLDY, MCASE, MINTEN,
     4                MFONT, MDASH, MSPOT, MCOLOR(3), MLIMIT(4), PTSYM,
     5                SAVX, SAVY
C
      INTEGER IOS, STATUS
      INTEGER BYTE8, W16
C
C  DEFINE THE BYTE SIZE FOR COUNT AND OPTION CODE
C
      DATA BYTE8/8/,W16/16/
C
C  INITIALIZE THE STATUS FIELD TO ALL OK
C
      STATUS = ALLOK
C
C  SKIP THE NEXT BYTE
C
      CALL MNINST(BYTE8,IOS,STATUS)
C
C  READ IN THE POINTS FLAG
C
      CALL MNINST(BYTE8,IOS,STATUS)
      PTSFLG = OPCL
      IF (STATUS.NE.ALLOK) RETURN
C
C   READ IN THE POINTS SYMBOL
C
      CALL MNINST(BYTE8,IOS,STATUS)
      PTSYM = OPCL
C
      RETURN
      END
