C
C $Id: slsclr.f,v 1.4 2008-07-27 00:17:27 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SLSCLR (ICLR,FRED,FGRN,FBLU)
C
C Given a color index ICLR and the RGB components to be associated with
C it, SLSCLR updates STITLES's information about the color.  If one of
C FRED, FGRN, or FBLU is negative, it just says that the associated
C component is not to be updated in the saved information.
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
          IF (ICFELL('SLSCLR',1).NE.0) RETURN
        END IF
C
C Look for the color index in the current list of color indices.  If
C it's there, update the RGB components associated with it as directed
C by the values of FRED, FGRN, and FBLU.
C
        DO 101 I=1,NOCI
          IF (LOCI(I).EQ.ICLR) THEN
            IF (FRED.GE.0.) CRED(I)=MIN(1.,FRED)
            IF (FGRN.GE.0.) CGRN(I)=MIN(1.,FGRN)
            IF (FBLU.GE.0.) CBLU(I)=MIN(1.,FBLU)
            RETURN
          END IF
  101   CONTINUE
C
C The color index is not in the current list.  If the list isn't full,
C create an entry for the desired color index and initialize the RGB
C components associated with it, using values returned by GQCR.  If
C GQCR doesn't return good values, use all 0s for color index 0 and all
C 1s for any other color index.  In any case, once the entry has been
C created, update the RGB components associated with it as directed
C by the values of FRED, FGRN, and FBLU.
C
        IF (NOCI.LT.256) THEN
          NOCI=NOCI+1
          LOCI(NOCI)=ICLR
          CALL GQCR (IWRK,ICLR,0,IERR,CRED(NOCI),CGRN(NOCI),CBLU(NOCI))
          IF (IERR.NE.0) THEN
            CRED(NOCI)=REAL(MAX(0,MIN(1,ICLR)))
            CGRN(NOCI)=REAL(MAX(0,MIN(1,ICLR)))
            CBLU(NOCI)=REAL(MAX(0,MIN(1,ICLR)))
          END IF
          IF (FRED.GE.0.) CRED(NOCI)=MIN(1.,FRED)
          IF (FGRN.GE.0.) CGRN(NOCI)=MIN(1.,FGRN)
          IF (FBLU.GE.0.) CBLU(NOCI)=MIN(1.,FBLU)
        ELSE
          CALL SETER ('SLSCLR - TOO MANY COLORS DEFINED',2,1)
          RETURN
        END IF
C
C Done.
C
        RETURN
C
      END
