C
C	$Id: trndt2.f,v 1.2 2000-07-12 17:04:37 haley Exp $
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
      BLOCKDATA TRNDT2
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
