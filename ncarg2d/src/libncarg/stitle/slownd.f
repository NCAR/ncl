C
C $Id: slownd.f,v 1.2 1995-07-28 18:38:08 kennison Exp $
C
      SUBROUTINE SLOWND
C
C Draw the outline of the windowed area.
C
C The common block SLCOMN holds all of the internal parameters of
C the package STITLE except for color-table parameters.
C
        COMMON /SLCOMN/ GPSZ,IBGC,IBGF,ICOP,IDOT,IFGC,IFGF,IJMP,IMAP,
     +                  INCU,IWLU,IWRK,IWWI,IXND,IXST,OORV,PCSZ,RNFS,
     +                  RVPB,RVPL,RVPR,RVPT,TFIN,TFOU,TGP1,TGP2,TGP3
        SAVE   /SLCOMN/
C
        CALL PLOTIF (  0.,  0.,2)
        IF (ICFELL('SLOWND',1).NE.0) RETURN
        CALL PLOTIF (RVPL,RVPB,0)
        IF (ICFELL('SLOWND',2).NE.0) RETURN
        CALL PLOTIF (RVPR,RVPB,1)
        IF (ICFELL('SLOWND',3).NE.0) RETURN
        CALL PLOTIF (RVPR,RVPT,1)
        IF (ICFELL('SLOWND',4).NE.0) RETURN
        CALL PLOTIF (RVPL,RVPT,1)
        IF (ICFELL('SLOWND',5).NE.0) RETURN
        CALL PLOTIF (RVPL,RVPB,1)
        IF (ICFELL('SLOWND',6).NE.0) RETURN
        CALL PLOTIF (  0.,  0.,2)
        IF (ICFELL('SLOWND',7).NE.0) RETURN
        RETURN
      END
