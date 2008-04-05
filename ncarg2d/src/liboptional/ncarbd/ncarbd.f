C
C	$Id: ncarbd.f,v 1.12 2008-04-05 01:40:24 kennison Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE NCARBD
C
C Call all of the do-nothing routines that, by virtue of being called,
C force the associated block data routines to be loaded.
C
        CALL AGDFLTX
        CALL ARBLDAX
        CALL BCBKDX
        CALL CONBDX
        CALL CONBDNX
        CALL CPBLDAX
        CALL CTBLDAX
        CALL DASHBDX
        CALL DPBLDAX
        CALL G01BKDX
        CALL GABLDTX
        CALL GFLSBDX
        CALL GKSBDX
        CALL GSEGDTX
        CALL GTPZBDX
        CALL GWIBKDX
        CALL HFINITX
        CALL HSTBKDX
        CALL IDBLDAX
        CALL ISBLDAX
        CALL LBBLDAX
        CALL MAPBDX
        CALL NGBLDAX
        CALL PCBDFFX
        CALL PCBLDAX
        CALL PWRXBDX
        CALL PWRYBDX
        CALL SEBLDAX
        CALL SFBLDAX
        CALL SLBLDAX
        CALL SRFABDX
        CALL STDATAX
        CALL TDBLDAX
        CALL THREBDX
        CALL UTILBDX
        CALL VELDATX
        CALL VTBLDAX
        CALL VVDATAX
        CALL WMBLDAX
C
      END
