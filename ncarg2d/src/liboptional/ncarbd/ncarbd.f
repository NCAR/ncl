C
C	$Id: ncarbd.f,v 1.13 2008-07-27 00:23:04 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
