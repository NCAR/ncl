C
C	$Id: trn32i.f,v 1.1.1.1 1992-04-17 22:31:26 ncargd Exp $
C
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
C REVISION HISTORY:
C
C January, 1978    Deleted references to "*COSY cards" and
C                  added revision history.
C January, 1979    New shading algorithm.
C March, 1979      Made code machine independent and conforming
C                  to the FORTRAN-66 standard.
C June, 1979       This version placed on "ULIB".
C September, 1979  Fixed problem in EZISOS dealing with
C                  determination of visibility of W plane.
C December, 1979   Fixed problem with pen down on contour
C                  initialization in subroutine FRSTC.
C March, 1980      Changed routine names "TRN32I" and "DRAW" to
C                  "TRN32I" and "DRAWI" to be consistent with the
C                  usage of the new routine "PWRZI".
C June, 1980       Fixed problem with zero index computation in
C                  subroutine FRSTC.  Added input parameter.
C                  DIMENSION statement missing in EZISOS.
C                  Fixed error in computation of arc cosine
C                  in EZISOS and TRN32I.
C December, 1984   Converted to GKS level 0A and standard FORTRAN 77.
C November, 1988   Removed KURV1S and KURV2S.  Implemented calls to
C                  MSKRV1 and MSKRV2 instead.
C July, 1990       Changed the name of the common block TEMPR to
C                  TEMPRX (to avoid conflict with THREED).
C March, 1991      Rewrote the routines FRSTC and FILLIN to implement a
C                  new, more robust, hidden-line algorithm.
C April, 1991      Added a parameter access interface.  Made it possible
C                  to vary the screen model resolution.  Cleaned up.
C                  Made special values work.
C
      END
