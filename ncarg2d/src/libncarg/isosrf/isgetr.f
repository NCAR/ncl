C
C	$Id: isgetr.f,v 1.1.1.1 1992-04-17 22:31:24 ncargd Exp $
C
C
C The subroutine ISGETR.
C --- ---------- -------
C
      SUBROUTINE ISGETR (IPN,RVL)
C
      CHARACTER*(*) IPN
C
C This subroutine is called to retrieve the real value of a specified
C parameter.
C
C IPN is the name of the parameter whose value is to be retrieved.
C
C RVL is a real variable in which the desired value is to be returned
C by ISGETR.
C
C Declare the required common blocks.
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
      COMMON /TEMPRX/ RZERO
      SAVE   /TEMPRX/
C
C Force the BLOCK DATA routine to load.
C
      EXTERNAL ISBLDA
C
C Define a character temporary to hold an error message.
C
      CHARACTER*48 CTM
C
C Check for a parameter name that is too short.
C
      IF (LEN(IPN).LT.2) THEN
        CTM(1:46)='ISGETI OR ISGETR - PARAMETER NAME TOO SHORT - '
        CTM(47:46+LEN(IPN))=IPN
        CALL SETER (CTM(1:36+LEN(IPN)),1,2)
        STOP
      END IF
C
C Get the appropriate parameter value.
C
      IF      (IPN(1:2).EQ.'IU') THEN
        RVL=REAL(NINU)
      ELSE IF (IPN(1:2).EQ.'IV') THEN
        RVL=REAL(NINV)
      ELSE IF (IPN(1:2).EQ.'IW') THEN
        RVL=REAL(NINW)
      ELSE IF (IPN(1:2).EQ.'RF') THEN
        RVL=REAL(IREF)
      ELSE IF (IPN(1:2).EQ.'RS') THEN
        RVL=RZERO
      ELSE IF (IPN(1:2).EQ.'SL') THEN
        RVL=DBPI
      ELSE IF (IPN(1:2).EQ.'SM') THEN
        IF (NX.EQ.128) THEN
          RVL=0.
        ELSE
          RVL=1.
        END IF
      ELSE IF (IPN(1:2).EQ.'ST') THEN
        RVL=TENSN
      ELSE IF (IPN(1:2).EQ.'SV') THEN
        RVL=SVAL
      ELSE IF (IPN(1:2).EQ.'VL') THEN
        RVL=XVPL
      ELSE IF (IPN(1:2).EQ.'VR') THEN
        RVL=XVPR
      ELSE IF (IPN(1:2).EQ.'VB') THEN
        RVL=YVPB
      ELSE IF (IPN(1:2).EQ.'VT') THEN
        RVL=YVPT
      ELSE
        CTM(1:46)='ISGETI OR ISGETR - PARAMETER NAME NOT KNOWN - '
        CTM(47:48)=IPN(1:2)
        CALL SETER (CTM(1:48),2,2)
        STOP
      END IF
C
C Done.
C
      RETURN
C
      END
