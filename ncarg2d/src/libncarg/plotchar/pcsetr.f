C
C $Id: pcsetr.f,v 1.16 2008-07-27 00:17:20 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCSETR (WHCH,RVAL)
C
      CHARACTER*(*) WHCH
C
C The subroutine PCSETR may be used to set PLCHHQ parameters which have
C values of type REAL.
C
C COMMON block declarations.
C
      COMMON /PCPRMS/ ADDS,CONS,DSTB,DSTL,DSTR,DSTT,HPIC(3),IBNU,
     +                IBXC(3),IBXF,ICEN,IORD,IOUC,IOUF,IPCC,IQUF,
     +                ISHC,ISHF,ITEF,JCOD,LSCI(16),NFCC,NODF,RBXL,
     +                RBXM,RBXX,RBXY,ROLW,RPLW,RSLW,SHDX,SHDY,SIZA,
     +                SSIC,SSPR,SUBS,VPIC(3),WPIC(3),XBEG,XCEN,XEND,
     +                XMUL(3),YBEG,YCEN,YEND,YMUL(3),ZINX,ZINY,ZINZ
      SAVE   /PCPRMS/
C
      COMMON /PCPFLQ/ IMAP,OORV,RHTW
      SAVE   /PCPFLQ/
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
      CALL PCBLDA
C
C Check for an uncleared prior error.
C
      IF (ICFELL('PCSETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Set the selected parameter.
C
      IF      (WHCH(1:2).EQ.'AS'.OR.WHCH(1:2).EQ.'as') THEN
        ADDS=RVAL
      ELSE IF (WHCH(1:2).EQ.'BC'.OR.WHCH(1:2).EQ.'bc') THEN
        CALL PCGPAI (WHCH,3,IPAI)
        IF (IPAI.EQ.0) THEN
          IBXC(1)=INT(RVAL)
        ELSE IF (IPAI.GE.1.AND.IPAI.LE.3) THEN
          IBXC(IPAI)=INT(RVAL)
        ELSE
          CALL SETER ('PCSETR - BOX COLOR ARRAY INDEX IS OUT OF RANGE',
     +                                                             2,1)
          RETURN
        END IF
      ELSE IF (WHCH(1:2).EQ.'BF'.OR.WHCH(1:2).EQ.'bf') THEN
        IBXF=INT(RVAL)
      ELSE IF (WHCH(1:2).EQ.'BL'.OR.WHCH(1:2).EQ.'bl') THEN
        RBXL=RVAL
      ELSE IF (WHCH(1:2).EQ.'BM'.OR.WHCH(1:2).EQ.'bm') THEN
        RBXM=RVAL
      ELSE IF (WHCH(1:2).EQ.'BX'.OR.WHCH(1:2).EQ.'bx') THEN
        RBXX=RVAL
      ELSE IF (WHCH(1:2).EQ.'BY'.OR.WHCH(1:2).EQ.'by') THEN
        RBXY=RVAL
      ELSE IF (WHCH(1:2).EQ.'CC'.OR.WHCH(1:2).EQ.'cc') THEN
        CALL PCGPAI (WHCH,3,IPAI)
        IF (IPAI.EQ.0) THEN
          IPCC=INT(RVAL)
        ELSE IF (IPAI.GE.1.AND.IPAI.LE.16) THEN
          LSCI(IPAI)=INT(RVAL)
        ELSE
          CALL SETER ('PCSETR - COLOR ARRAY INDEX IS OUT OF RANGE',3,1)
          RETURN
        END IF
      ELSE IF (WHCH(1:2).EQ.'CD'.OR.WHCH(1:2).EQ.'cd') THEN
        JCOD=MAX(0,MIN(1,INT(RVAL)))
      ELSE IF (WHCH(1:2).EQ.'CE'.OR.WHCH(1:2).EQ.'ce') THEN
        ICEN=MAX(0,MIN(1,INT(RVAL)))
      ELSE IF (WHCH(1:2).EQ.'CH'.OR.WHCH(1:2).EQ.'ch') THEN
        HPIC(3)=MAX(0.,RVAL)
        YMUL(3)=HPIC(3)/9.
      ELSE IF (WHCH(1:2).EQ.'CL'.OR.WHCH(1:2).EQ.'cl') THEN
        RPLW=MAX(0.,RVAL)
      ELSE IF (WHCH(1:2).EQ.'CS'.OR.WHCH(1:2).EQ.'cs') THEN
        CONS=RVAL/2.
      ELSE IF (WHCH(1:2).EQ.'CV'.OR.WHCH(1:2).EQ.'cv') THEN
        VPIC(3)=MAX(0.,RVAL)
      ELSE IF (WHCH(1:2).EQ.'CW'.OR.WHCH(1:2).EQ.'cw') THEN
        WPIC(3)=MAX(0.,RVAL)
        XMUL(3)=WPIC(3)/8.
      ELSE IF (WHCH(1:2).EQ.'DO'.OR.WHCH(1:2).EQ.'do') THEN
        IORD=MAX(-2,MIN(+2,INT(RVAL)))
        IF (IORD.EQ.0) IORD=1
      ELSE IF (WHCH(1:2).EQ.'FB'.OR.WHCH(1:2).EQ.'fb') THEN
        CALL BCSETR ('FTL',RVAL)
        IF (ICFELL('PCSETR',4).NE.0) RETURN
      ELSE IF (WHCH(1:2).EQ.'FN'.OR.WHCH(1:2).EQ.'fn') THEN
        NODF=ABS(INT(RVAL))
        IF ((NODF.GE. 23.AND.NODF.LE. 24).OR.
     +      (NODF.GE. 27.AND.NODF.LE. 28).OR.
     +      (NODF.GE. 31.AND.NODF.LE. 32).OR.
     +      (NODF.GE. 38.AND.NODF.LE.120).OR.
     +      (NODF.GE.123.AND.NODF.LE.124).OR.
     +      (NODF.GE.127.AND.NODF.LE.128).OR.
     +      (NODF.GE.131.AND.NODF.LE.132).OR.NODF.GE.138) NODF=1
      ELSE IF (WHCH(1:2).EQ.'HW'.OR.WHCH(1:2).EQ.'hw') THEN
        RHTW=RVAL
      ELSE IF (WHCH(1:2).EQ.'IH'.OR.WHCH(1:2).EQ.'ih') THEN
        HPIC(2)=MAX(0.,RVAL)
        YMUL(2)=HPIC(2)/13.
      ELSE IF (WHCH(1:2).EQ.'IS'.OR.WHCH(1:2).EQ.'is') THEN
        SSIC=MAX(0.,RVAL)
      ELSE IF (WHCH(1:2).EQ.'IV'.OR.WHCH(1:2).EQ.'iv') THEN
        VPIC(2)=MAX(0.,RVAL)
      ELSE IF (WHCH(1:2).EQ.'IW'.OR.WHCH(1:2).EQ.'iw') THEN
        WPIC(2)=MAX(0.,RVAL)
        XMUL(2)=WPIC(2)/12.
      ELSE IF (WHCH(1:2).EQ.'MA'.OR.WHCH(1:2).EQ.'ma') THEN
        IMAP=MAX(0,INT(RVAL))
      ELSE IF (WHCH(1:2).EQ.'OC'.OR.WHCH(1:2).EQ.'oc') THEN
        IOUC=INT(RVAL)
      ELSE IF (WHCH(1:2).EQ.'OF'.OR.WHCH(1:2).EQ.'of') THEN
        IOUF=MAX(0,MIN(1,INT(RVAL)))
      ELSE IF (WHCH(1:2).EQ.'OL'.OR.WHCH(1:2).EQ.'ol') THEN
        ROLW=MAX(0.,RVAL)
      ELSE IF (WHCH(1:2).EQ.'OR'.OR.WHCH(1:2).EQ.'or') THEN
        OORV=RVAL
      ELSE IF (WHCH(1:2).EQ.'PH'.OR.WHCH(1:2).EQ.'ph') THEN
        HPIC(1)=MAX(0.,RVAL)
        YMUL(1)=HPIC(1)/21.
      ELSE IF (WHCH(1:2).EQ.'PS'.OR.WHCH(1:2).EQ.'ps') THEN
        SSPR=MAX(0.,RVAL)
      ELSE IF (WHCH(1:2).EQ.'PV'.OR.WHCH(1:2).EQ.'pv') THEN
        VPIC(1)=MAX(0.,RVAL)
      ELSE IF (WHCH(1:2).EQ.'PW'.OR.WHCH(1:2).EQ.'pw') THEN
        WPIC(1)=MAX(0.,RVAL)
        XMUL(1)=WPIC(1)/16.
      ELSE IF (WHCH(1:2).EQ.'QU'.OR.WHCH(1:2).EQ.'qu') THEN
        IQUF=MAX(0,MIN(2,INT(RVAL)))
      ELSE IF (WHCH(1:2).EQ.'SA'.OR.WHCH(1:2).EQ.'sa') THEN
        SIZA=MAX(0.,RVAL)
      ELSE IF (WHCH(1:2).EQ.'SC'.OR.WHCH(1:2).EQ.'sc') THEN
        ISHC=INT(RVAL)
      ELSE IF (WHCH(1:2).EQ.'SF'.OR.WHCH(1:2).EQ.'sf') THEN
        ISHF=MAX(0,MIN(1,INT(RVAL)))
      ELSE IF (WHCH(1:2).EQ.'SL'.OR.WHCH(1:2).EQ.'sl') THEN
        RSLW=MAX(0.,RVAL)
      ELSE IF (WHCH(1:2).EQ.'SS'.OR.WHCH(1:2).EQ.'ss') THEN
        SUBS=RVAL
      ELSE IF (WHCH(1:2).EQ.'SX'.OR.WHCH(1:2).EQ.'sx') THEN
        SHDX=RVAL
      ELSE IF (WHCH(1:2).EQ.'SY'.OR.WHCH(1:2).EQ.'sy') THEN
        SHDY=RVAL
      ELSE IF (WHCH(1:2).EQ.'TE'.OR.WHCH(1:2).EQ.'te') THEN
        ITEF=MAX(0,MIN(1,INT(RVAL)))
      ELSE IF (WHCH(1:2).EQ.'UN'.OR.WHCH(1:2).EQ.'un') THEN
        IBNU=MAX(1,INT(RVAL))
      ELSE IF (WHCH(1:2).EQ.'ZX'.OR.WHCH(1:2).EQ.'zx') THEN
        ZINX=MAX(0.,RVAL)
      ELSE IF (WHCH(1:2).EQ.'ZY'.OR.WHCH(1:2).EQ.'zy') THEN
        ZINY=MAX(0.,RVAL)
      ELSE IF (WHCH(1:2).EQ.'ZZ'.OR.WHCH(1:2).EQ.'zz') THEN
        ZINZ=MAX(0.,RVAL)
      ELSE
        CALL SETER ('PCSETR - UNRECOGNIZED PARAMETER NAME',5,1)
        RETURN
      END IF
C
C Done.
C
      RETURN
C
      END
