C
C	$Id: iszero.f,v 1.1.1.1 1992-04-17 22:31:26 ncargd Exp $
C
C
C The subroutine ISZERO.
C --- ---------- -------
C
      SUBROUTINE ISZERO
C
C This routine zeroes both screen models.
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
      DO  20 I=1,LX
        DO  10 J=1,NY
          ISCR(I,J) = 0
          ISCA(I,J) = 0
   10   CONTINUE
   20 CONTINUE
      RETURN
      END
