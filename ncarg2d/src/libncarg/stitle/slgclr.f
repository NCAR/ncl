C
C $Id: slgclr.f,v 1.2 2000-07-12 16:25:58 haley Exp $
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
      SUBROUTINE SLGCLR (ICLR,FRED,FGRN,FBLU)
C
C Given a color index ICLR, SLGCLR returns to the caller the RGB
C components of the color that STITLE associates with that index.
C
C The common block SLCOMN holds all of the internal parameters of
C the package STITLE except for color-table parameters.
C
        COMMON /SLCOMN/ GPSZ,IBGC,IBGF,ICOP,IDOT,IFGC,IFGF,IJMP,IMAP,
     +                  INCU,IWLU,IWRK,IWWI,IXND,IXST,OORV,PCSZ,RNFS,
     +                  RVPB,RVPL,RVPR,RVPT,TFIN,TFOU,TGP1,TGP2,TGP3
        SAVE   /SLCOMN/
C
C The common block SLCLRS holds color-table parameters.
C
        COMMON /SLCLRS/ CRED(256),CGRN(256),CBLU(256),LOCI(256),NOCI
        SAVE   /SLCLRS/
C
C If it wasn't done yet, get the ID of the first active workstation.
C
        IF (IWRK.LT.0) THEN
          CALL SLGWID
          IF (ICFELL('SLGCLR',1).NE.0) RETURN
        END IF
C
C Look for the color index in the current list of color indices.  If
C it's there, just return the RGB components associated with it.
C
        DO 101 I=1,NOCI
          IF (LOCI(I).EQ.ICLR) THEN
            FRED=CRED(I)
            FGRN=CGRN(I)
            FBLU=CBLU(I)
            RETURN
          END IF
  101   CONTINUE
C
C The desired color index is not in the current list.  If the list is
C not full, create an entry for the color index and initialize the RGB
C components associated with it, using values returned by GQCR.  If
C GQCR doesn't return good values, use all 0s for color index 0 and
C all 1s for any other color index.  In any case, return the desired
C RGB components to the caller.
C
        IF (NOCI.LT.256) THEN
          NOCI=NOCI+1
          LOCI(NOCI)=ICLR
          CALL GQCR (IWRK,ICLR,0,IERR,FRED,FGRN,FBLU)
          IF (IERR.NE.0) THEN
            FRED=REAL(MAX(0,MIN(1,ICLR)))
            FGRN=REAL(MAX(0,MIN(1,ICLR)))
            FBLU=REAL(MAX(0,MIN(1,ICLR)))
          END IF
          CRED(NOCI)=FRED
          CGRN(NOCI)=FGRN
          CBLU(NOCI)=FBLU
        ELSE
          CALL SETER ('SLGCLR - TOO MANY COLORS DEFINED',2,1)
          RETURN
        END IF
C
C Done.
C
        RETURN
C
      END
