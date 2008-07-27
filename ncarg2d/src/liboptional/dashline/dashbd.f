C
C	$Id: dashbd.f,v 1.5 2008-07-27 00:23:02 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE DASHBD
C
C Calling this do-nothing subroutine forces "ld" to load the following
C block data routine (but only if they are in the same ".f" file).
C
        RETURN
C
      END
CNOSPLIT
      BLOCKDATA DASHBDX
C
C DASHBDX IS USED TO INITIALIZE VARIABLES IN NAMED COMMON.
C
      COMMON/INTPR/IPAU,FPART,TENSN,NP,SMALL,L1,ADDLR,ADDTB,MLLINE,
     1    ICLOSE
      COMMON /DSHD/   ISL        ,L          ,IP(16)
      COMMON /DSHDA/  IFSTFL
      COMMON /DSHDC/ IXSTOR,IYSTOR
      COMMON /DSHDD/ IFCFLG, MASK
      SAVE
C
C ISL IS A FLAG FOR AN ALL SOLID PATTERN (+1) OR AN ALL GAP PATTERN (-1)
C
      DATA ISL /1/
C
C
C IFSTFL IS A FLAG TO CONTROL THAT FRSTD IS CALLED BEFORE VECTD IS
C CALLED ANY TIME WHEN DASHDB HAS BEEN CALLED.
C
      DATA IFSTFL /1/
C
C  INTERNAL PARAMETERS
C
      DATA IPAU/3/
      DATA FPART/1./
      DATA TENSN/2.5/
      DATA NP/150/
      DATA SMALL/128./
      DATA L1/70/
      DATA ADDLR/2./
      DATA ADDTB/2./
      DATA MLLINE/384/
      DATA ICLOSE/6/
C
C IFCFLG IS THE FIRST CALL FLAG FOR DASHDB.
C
      DATA IFCFLG /1/
C
C IXSTOR AND IYSTOR CONTAIN THE CURRENT PEN POSITION. THEY ARE
C INITIALIZED TO AN IMPOSSIBLE POSITION
C
      DATA IXSTOR,IYSTOR /-9999,-9999/
C
C
C REVISION HISTORY--------
C
C APRIL 1984        CONVERTED TO FORTRAN77 AND GKS.
C
C DECEMBER 1979     ADDED STATISTICS CALL AND REVISION HISTORY
C                   DID NOT CHANGE CARD COUNT
C
C------------------------------------------------------------
      END
