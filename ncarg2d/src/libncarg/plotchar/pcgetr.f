C
C $Id: pcgetr.f,v 1.16 2008-07-27 00:17:19 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCGETR (WHCH,RVAL)
C
      CHARACTER*(*) WHCH
C
C The subroutine PCGETR may be used to get PLCHHQ parameters which have
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
      IF (ICFELL('PCGETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Get the selected parameter.
C
      IF      (WHCH(1:2).EQ.'AS'.OR.WHCH(1:2).EQ.'as') THEN
        RVAL=ADDS
      ELSE IF (WHCH(1:2).EQ.'BC'.OR.WHCH(1:2).EQ.'bc') THEN
        CALL PCGPAI (WHCH,3,IPAI)
        IF (IPAI.EQ.0) THEN
          RVAL=REAL(IBXC(1))
        ELSE IF (IPAI.GE.1.AND.IPAI.LE.3) THEN
          RVAL=REAL(IBXC(IPAI))
        ELSE
          CALL SETER ('PCGETR - BOX COLOR ARRAY INDEX IS OUT OF RANGE',
     +                                                             2,1)
          RETURN
        END IF
      ELSE IF (WHCH(1:2).EQ.'BF'.OR.WHCH(1:2).EQ.'bf') THEN
        RVAL=REAL(IBXF)
      ELSE IF (WHCH(1:2).EQ.'BL'.OR.WHCH(1:2).EQ.'bl') THEN
        RVAL=RBXL
      ELSE IF (WHCH(1:2).EQ.'BM'.OR.WHCH(1:2).EQ.'bm') THEN
        RVAL=RBXM
      ELSE IF (WHCH(1:2).EQ.'BX'.OR.WHCH(1:2).EQ.'bx') THEN
        RVAL=RBXX
      ELSE IF (WHCH(1:2).EQ.'BY'.OR.WHCH(1:2).EQ.'by') THEN
        RVAL=RBXY
      ELSE IF (WHCH(1:2).EQ.'CC'.OR.WHCH(1:2).EQ.'cc') THEN
        CALL PCGPAI (WHCH,3,IPAI)
        IF (IPAI.EQ.0) THEN
          RVAL=REAL(IPCC)
        ELSE IF (IPAI.GE.1.AND.IPAI.LE.16) THEN
          RVAL=REAL(LSCI(IPAI))
        ELSE
          CALL SETER ('PCGETR - COLOR ARRAY INDEX IS OUT OF RANGE',3,1)
          RETURN
        END IF
      ELSE IF (WHCH(1:2).EQ.'CD'.OR.WHCH(1:2).EQ.'cd') THEN
        RVAL=REAL(JCOD)
      ELSE IF (WHCH(1:2).EQ.'CE'.OR.WHCH(1:2).EQ.'ce') THEN
        RVAL=REAL(ICEN)
      ELSE IF (WHCH(1:2).EQ.'CH'.OR.WHCH(1:2).EQ.'ch') THEN
        RVAL=HPIC(3)
      ELSE IF (WHCH(1:2).EQ.'CL'.OR.WHCH(1:2).EQ.'cl') THEN
        RVAL=RPLW
      ELSE IF (WHCH(1:2).EQ.'CS'.OR.WHCH(1:2).EQ.'cs') THEN
        RVAL=2.*CONS
      ELSE IF (WHCH(1:2).EQ.'CV'.OR.WHCH(1:2).EQ.'cv') THEN
        RVAL=VPIC(3)
      ELSE IF (WHCH(1:2).EQ.'CW'.OR.WHCH(1:2).EQ.'cw') THEN
        RVAL=WPIC(3)
      ELSE IF (WHCH(1:2).EQ.'DB'.OR.WHCH(1:2).EQ.'db') THEN
        RVAL=DSTB
      ELSE IF (WHCH(1:2).EQ.'DL'.OR.WHCH(1:2).EQ.'dl') THEN
        RVAL=DSTL
      ELSE IF (WHCH(1:2).EQ.'DO'.OR.WHCH(1:2).EQ.'do') THEN
        RVAL=REAL(IORD)
      ELSE IF (WHCH(1:2).EQ.'DR'.OR.WHCH(1:2).EQ.'dr') THEN
        RVAL=DSTR
      ELSE IF (WHCH(1:2).EQ.'DT'.OR.WHCH(1:2).EQ.'dt') THEN
        RVAL=DSTT
      ELSE IF (WHCH(1:2).EQ.'FB'.OR.WHCH(1:2).EQ.'fb') THEN
        CALL BCGETR ('FTL',RVAL)
      ELSE IF (WHCH(1:2).EQ.'FN'.OR.WHCH(1:2).EQ.'fn') THEN
        RVAL=REAL(NODF)
      ELSE IF (WHCH(1:2).EQ.'HW'.OR.WHCH(1:2).EQ.'hw') THEN
        RVAL=RHTW
      ELSE IF (WHCH(1:2).EQ.'IH'.OR.WHCH(1:2).EQ.'ih') THEN
        RVAL=HPIC(2)
      ELSE IF (WHCH(1:2).EQ.'IS'.OR.WHCH(1:2).EQ.'is') THEN
        RVAL=SSIC
      ELSE IF (WHCH(1:2).EQ.'IV'.OR.WHCH(1:2).EQ.'iv') THEN
        RVAL=VPIC(2)
      ELSE IF (WHCH(1:2).EQ.'IW'.OR.WHCH(1:2).EQ.'iw') THEN
        RVAL=WPIC(2)
      ELSE IF (WHCH(1:2).EQ.'MA'.OR.WHCH(1:2).EQ.'ma') THEN
        RVAL=REAL(IMAP)
      ELSE IF (WHCH(1:2).EQ.'OC'.OR.WHCH(1:2).EQ.'oc') THEN
        RVAL=REAL(IOUC)
      ELSE IF (WHCH(1:2).EQ.'OF'.OR.WHCH(1:2).EQ.'of') THEN
        RVAL=REAL(IOUF)
      ELSE IF (WHCH(1:2).EQ.'OL'.OR.WHCH(1:2).EQ.'ol') THEN
        RVAL=ROLW
      ELSE IF (WHCH(1:2).EQ.'OR'.OR.WHCH(1:2).EQ.'or') THEN
        RVAL=OORV
      ELSE IF (WHCH(1:2).EQ.'PH'.OR.WHCH(1:2).EQ.'ph') THEN
        RVAL=HPIC(1)
      ELSE IF (WHCH(1:2).EQ.'PS'.OR.WHCH(1:2).EQ.'ps') THEN
        RVAL=SSPR
      ELSE IF (WHCH(1:2).EQ.'PV'.OR.WHCH(1:2).EQ.'pv') THEN
        RVAL=VPIC(1)
      ELSE IF (WHCH(1:2).EQ.'PW'.OR.WHCH(1:2).EQ.'pw') THEN
        RVAL=WPIC(1)
      ELSE IF (WHCH(1:2).EQ.'QU'.OR.WHCH(1:2).EQ.'qu') THEN
        RVAL=REAL(IQUF)
      ELSE IF (WHCH(1:2).EQ.'SA'.OR.WHCH(1:2).EQ.'sa') THEN
        RVAL=SIZA
      ELSE IF (WHCH(1:2).EQ.'SC'.OR.WHCH(1:2).EQ.'sc') THEN
        RVAL=REAL(ISHC)
      ELSE IF (WHCH(1:2).EQ.'SF'.OR.WHCH(1:2).EQ.'sf') THEN
        RVAL=REAL(ISHF)
      ELSE IF (WHCH(1:2).EQ.'SL'.OR.WHCH(1:2).EQ.'sl') THEN
        RVAL=RSLW
      ELSE IF (WHCH(1:2).EQ.'SS'.OR.WHCH(1:2).EQ.'ss') THEN
        RVAL=SUBS
      ELSE IF (WHCH(1:2).EQ.'SX'.OR.WHCH(1:2).EQ.'sx') THEN
        RVAL=SHDX
      ELSE IF (WHCH(1:2).EQ.'SY'.OR.WHCH(1:2).EQ.'sy') THEN
        RVAL=SHDY
      ELSE IF (WHCH(1:2).EQ.'TE'.OR.WHCH(1:2).EQ.'te') THEN
        RVAL=REAL(ITEF)
      ELSE IF (WHCH(1:2).EQ.'UN'.OR.WHCH(1:2).EQ.'un') THEN
        RVAL=REAL(IBNU)
      ELSE IF (WHCH(1:2).EQ.'XB'.OR.WHCH(1:2).EQ.'xb') THEN
        RVAL=XBEG
      ELSE IF (WHCH(1:2).EQ.'XC'.OR.WHCH(1:2).EQ.'xc') THEN
        RVAL=XCEN
      ELSE IF (WHCH(1:2).EQ.'XE'.OR.WHCH(1:2).EQ.'xe') THEN
        RVAL=XEND
      ELSE IF (WHCH(1:2).EQ.'YB'.OR.WHCH(1:2).EQ.'yb') THEN
        RVAL=YBEG
      ELSE IF (WHCH(1:2).EQ.'YC'.OR.WHCH(1:2).EQ.'yc') THEN
        RVAL=YCEN
      ELSE IF (WHCH(1:2).EQ.'YE'.OR.WHCH(1:2).EQ.'ye') THEN
        RVAL=YEND
      ELSE IF (WHCH(1:2).EQ.'ZX'.OR.WHCH(1:2).EQ.'zx') THEN
        RVAL=ZINX
      ELSE IF (WHCH(1:2).EQ.'ZY'.OR.WHCH(1:2).EQ.'zy') THEN
        RVAL=ZINY
      ELSE IF (WHCH(1:2).EQ.'ZZ'.OR.WHCH(1:2).EQ.'zz') THEN
        RVAL=ZINZ
      ELSE
        CALL SETER ('PCGETR - UNRECOGNIZED PARAMETER NAME',4,1)
        RETURN
      END IF
C
C Done.
C
      RETURN
C
      END
