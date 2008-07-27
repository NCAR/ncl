C
C	$Id: lodctb.f,v 1.4 2008-07-27 00:59:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE LODCTB(IOS,STATUS)
C
C  Send out the color table to the device.
C
C  OUTPUT
C    IOS    -  The I/O status, valid only if STATUS is non-zero.
C    STATUS -  The status of the request, defined by COMMON TREROR.
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
      COMMON /CINFO/COLVLS,INTS,NUMI
      DIMENSION COLVLS(3,256),INTS(256)
C
      INTEGER IOS, STATUS
C
      STATUS = ALLOK
      CALL GSCR(1,0,0.,0.,0.)
      DO 10 I=1,NUMI
      CALL GSCR(1,I,COLVLS(1,I),COLVLS(2,I),COLVLS(3,I))
   10 CONTINUE
C
      RETURN
      END
