C
C	$Id: issetr.f,v 1.1.1.1 1992-04-17 22:31:24 ncargd Exp $
C
C
C The subroutine ISSETR.
C --- ---------- -------
C
      SUBROUTINE ISSETR (IPN,RVL)
C
      CHARACTER*(*) IPN
C
C This subroutine is called to set the real value of a specified
C parameter.
C
C IPN is the name of the parameter whose value is to be set.
C
C RVL is the desired new real value.
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
        CTM(1:46)='ISSETI OR ISSETR - PARAMETER NAME TOO SHORT - '
        CTM(47:46+LEN(IPN))=IPN
        CALL SETER (CTM(1:46+LEN(IPN)),1,2)
        STOP
      END IF
C
C Set the appropriate parameter value.
C
      IF      (IPN(1:2).EQ.'IU') THEN
        NINU=MAX(-10,MIN(10,INT(RVL)))
      ELSE IF (IPN(1:2).EQ.'IV') THEN
        NINV=MAX(-10,MIN(10,INT(RVL)))
      ELSE IF (IPN(1:2).EQ.'IW') THEN
        NINW=MAX(-10,MIN(10,INT(RVL)))
      ELSE IF (IPN(1:2).EQ.'RF') THEN
        IREF=MAX(0,MIN(1,INT(RVL)))
      ELSE IF (IPN(1:2).EQ.'RS') THEN
        RZERO=MAX(0.,RVL)
      ELSE IF (IPN(1:2).EQ.'SL') THEN
        DBPI=MAX(.0001,MIN(.1,RVL))
      ELSE IF (IPN(1:2).EQ.'SM') THEN
        IF (INT(RVL).EQ.0) THEN
          NX=128
          NY=128
        ELSE
          NX=256
          NY=256
        END IF
        IDONE=0
      ELSE IF (IPN(1:2).EQ.'ST') THEN
        TENSN=MAX(0.,RVL)
      ELSE IF (IPN(1:2).EQ.'SV') THEN
        SVAL=RVL
      ELSE IF (IPN(1:2).EQ.'VL') THEN
        XVPL=MAX(0.,MIN(1.,RVL))
      ELSE IF (IPN(1:2).EQ.'VR') THEN
        XVPR=MAX(0.,MIN(1.,RVL))
      ELSE IF (IPN(1:2).EQ.'VB') THEN
        YVPB=MAX(0.,MIN(1.,RVL))
      ELSE IF (IPN(1:2).EQ.'VT') THEN
        YVPT=MAX(0.,MIN(1.,RVL))
      ELSE
        CTM(1:46)='ISSETI OR ISSETR - PARAMETER NAME NOT KNOWN - '
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
