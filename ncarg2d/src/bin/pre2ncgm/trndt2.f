C
C	$Id: trndt2.f,v 1.5 2008-07-27 00:59:05 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TRNDT2
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA TRNDT2X
C
C  MAKE SURE THAT THESE COMMONS ARE DEFINED EVERYWHERE
C
      COMMON /CAPSCN/ SCSSTR, SCSSIZ, SCTSTR, SCTSIZ, SCNLLX,
     1                SCNLLY, SCNURX, SCNURY, SCNXOF, SCNYOF,
     2                SCNXSC, SCNYSC, SCNFMT, SCNFIN, SCVFMT,
     3                SCVFIN, SCNSIM
      INTEGER         SCSMAX, SCTMAX, SFMMAX, SFNMAX, SCVFMX,
     1                SCVFIX
      PARAMETER  (SCSMAX=50, SCTMAX=50, SFMMAX=10, SFNMAX=8)
      PARAMETER  (SCVFMX=10, SCVFIX=8)
      INTEGER         SCSSTR(SCSMAX), SCSSIZ, SCTSTR(SCTMAX), SCTSIZ,
     2                SCNLLX, SCNLLY, SCNURX, SCNURY, SCNXOF, SCNYOF,
     3                SCNFMT(SFMMAX,4)      , SCNFIN(SFNMAX),
     4                SCVFMT(SCVFMX,4)      , SCVFIN(SCVFIX)
      REAL            SCNXSC, SCNYSC, SCNRIN(SFNMAX), SCVRIN(SCVFIX)
      LOGICAL         SCNSIM
      INTEGER         LENSCN
      PARAMETER  (LENSCN=SCSMAX+1+SCTMAX+1+1+1+1+1+1+1+1+1+
     1                   (SFMMAX*4)+SFNMAX+(SCVFMX*4)+SCVFIX+1)
      EQUIVALENCE (SCNFIN,SCNRIN), (SCVFIN,SCVRIN)
C
C  INITIALIZE RASTER SCAN DATA
C
      DATA SCNXOF,SCNYOF,SCNXSC,SCNYSC,SCNSIM/0,0,1.,1.,.FALSE./
C
      END
