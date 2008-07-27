C
C	$Id: alover.f,v 1.4 2008-07-27 00:59:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE ALOVER(IOS,STATUS)
C
C  pre2ncgm is a package for converting NCAR metafiles produced from the
C  pre-GKS NCAR graphics package to NCAR CGM format--the format of the
C  metafiles produced from the GKS-based NCAR Graphics package.
C
C  Implementation:
C
C  On UNIX systems, to get an executable you just compile the Fortran
C  code with "ncargf77".  This will give you an executable which reads
C  its input metafile from standard input and writes its output to
C  the metafile produced by the NCAR GKS package (gmeta by default).
C
C  On non-UNIX systems you will have to load the enclosed Frotran
C  code with all of the necessary support routines from the NCAR
C  graphics package.
C
C  The enclosed code is structured for 32-bit machines.  To run on other
C  hosts, the setting for MNWRDS must be changed everywhere in the code
C  to equal the number of machine words needed to hold 1440 bytes.
C  MNWRDS is set to 360 in this code.
C
C---------------------------------------------------------------------------
C
C  Description of ALOVER:
C
C  THIS ROUTINE PROCESSES FATAL ERRORS
C  IT WILL
C       1. PUT AN ERROR MESSAGE ON THE META TRANSLATOR ERROR UNIT
C       2. CLEAN UP THE DEVICE BEING USED
C       3. TERMINATE EXECUTION OF THE META TRANSLATOR
C
C  INPUT
C       IOS-READ STATUS ONLY HAS MEANING IF STATUS = REDERR
C       STATUS-THE FATAL ERROR NUMBER
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
      COMMON/TRINOT/ IPTR, MBUFOT, MAXBYT, DEVUNT, METUNT,
     1          METIPT, ERRUNT, FNTUNT
      INTEGER MAXCNT
      PARAMETER (MAXCNT=200)
      INTEGER IPTR, MBUFOT(MAXCNT), MAXBYT, DEVUNT, METUNT, ERRUNT,
     1        FNTUNT
      LOGICAL METIPT
C
      INTEGER IOS, STATUS
C
C  CLEAN UP THE GRAPHICS DEVICE
C
      CALL GECLKS
C
C  TERMINATE EXECUTION OF THE META TRANSLATOR
C
      STOP
C
      END
