C
C $Id: issetr.f,v 1.8 2008-07-27 00:17:16 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
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
C Define a character temporary to hold an error message.
C
      CHARACTER*48 CTM
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL ISBLDA
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
      IF      (IPN(1:2).EQ.'IU'.OR.IPN(1:2).EQ.'iu') THEN
        NINU=MAX(-10,MIN(10,INT(RVL)))
      ELSE IF (IPN(1:2).EQ.'IV'.OR.IPN(1:2).EQ.'iv') THEN
        NINV=MAX(-10,MIN(10,INT(RVL)))
      ELSE IF (IPN(1:2).EQ.'IW'.OR.IPN(1:2).EQ.'iw') THEN
        NINW=MAX(-10,MIN(10,INT(RVL)))
      ELSE IF (IPN(1:2).EQ.'RF'.OR.IPN(1:2).EQ.'rf') THEN
        IREF=MAX(0,MIN(1,INT(RVL)))
      ELSE IF (IPN(1:2).EQ.'RS'.OR.IPN(1:2).EQ.'rs') THEN
        RZERO=MAX(0.,RVL)
      ELSE IF (IPN(1:2).EQ.'SL'.OR.IPN(1:2).EQ.'sl') THEN
        DBPI=MAX(.0001,MIN(.1,RVL))
      ELSE IF (IPN(1:2).EQ.'SM'.OR.IPN(1:2).EQ.'sm') THEN
        IF (INT(RVL).EQ.0) THEN
          NX=128
          NY=128
        ELSE
          NX=256
          NY=256
        END IF
        IDONE=0
      ELSE IF (IPN(1:2).EQ.'ST'.OR.IPN(1:2).EQ.'st') THEN
        TENSN=MAX(0.,RVL)
      ELSE IF (IPN(1:2).EQ.'SV'.OR.IPN(1:2).EQ.'sv') THEN
        SVAL=RVL
      ELSE IF (IPN(1:2).EQ.'VL'.OR.IPN(1:2).EQ.'vl') THEN
        XVPL=MAX(0.,MIN(1.,RVL))
      ELSE IF (IPN(1:2).EQ.'VR'.OR.IPN(1:2).EQ.'vr') THEN
        XVPR=MAX(0.,MIN(1.,RVL))
      ELSE IF (IPN(1:2).EQ.'VB'.OR.IPN(1:2).EQ.'vb') THEN
        YVPB=MAX(0.,MIN(1.,RVL))
      ELSE IF (IPN(1:2).EQ.'VT'.OR.IPN(1:2).EQ.'vt') THEN
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
