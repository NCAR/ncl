C
C The subroutine TRN32I.
C --- ---------- -------
C
      SUBROUTINE TRN32I (UT,VT,WT,XT,YT,ZT,IENT)
C
C This routine provides a temporary interface for ISTR32 until such
C time as all calls to TRN32I can be found and modified (which may
C be never, as there are likely to be some such calls in user code).
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
C Pass the arguments on to ISTR32.
C
        CALL ISTR32 (UT,VT,WT,XT,YT,ZT,IENT)
C
C If necessary, convert the returned coordinates from the fractional
C system to the metacode system.
C
        IF (IENT.NE.1.AND.ISCALE.NE.0) THEN
          XT=32767.*XT
          YT=32767.*YT
        END IF
C
C Done.
C
        RETURN
C
      END
