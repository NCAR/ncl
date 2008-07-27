C
C $Id: slsetr.f,v 1.7 2008-07-27 00:17:27 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SLSETR (PNAM,RVAL)
C
C Set the real value of the STITLE parameter named PNAM from RVAL.
C
        CHARACTER*(*) PNAM
C
C The common block SLCOMN holds all of the internal parameters of
C the package STITLE except for color-table parameters.
C
        COMMON /SLCOMN/ GPSZ,IBGC,IBGF,ICOP,IDOT,IFGC,IFGF,IJMP,IMAP,
     +                  INCU,IWLU,IWRK,IWWI,IXND,IXST,OORV,PCSZ,RNFS,
     +                  RVPB,RVPL,RVPR,RVPT,TFIN,TFOU,TGP1,TGP2,TGP3
        SAVE   /SLCOMN/
C
C Define a temporary variable in which to put the first three characters
C of PNAM.
C
        CHARACTER*3 CTMP
C
C Define a character variable in which messages may be formed.
C
        CHARACTER*29 CMSG
C
C Do a call forcing a BLOCKDATA to be loaded from a binary library.
C
        CALL SLBLDA
C
C Check for an uncleared prior error.
C
        IF (ICFELL('SLSETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Extract the first three characters of the parameter name.
C
        CTMP=PNAM
C
C If the parameter name has less than three characters, log an error.
C
        IF (LEN(PNAM).LT.3) GO TO 901
C
C See what the parameter name is ...
C
C ... the flag controlling the output of the alignment frames, ...
C
        IF      (CTMP.EQ.'ALN'.OR.CTMP.EQ.'aln') THEN
C
          IDOT=MAX(0,MIN(2,INT(RVAL)))
C
C ... the blue component of the background color, ...
C
        ELSE IF (CTMP.EQ.'BGB'.OR.CTMP.EQ.'bgb') THEN
C
          CALL SLSCLR (IBGC,-1.,-1.,RVAL)
          IF (ICFELL('SLSETR',2).NE.0) RETURN
C
C ... the default background color index ...
C
        ELSE IF (CTMP.EQ.'BGC'.OR.CTMP.EQ.'bgc') THEN
C
          IBGC=MAX(0,INT(RVAL))
C
C ... the background color fade flag ...
C
        ELSE IF (CTMP.EQ.'BGF'.OR.CTMP.EQ.'bgf') THEN
C
          IBGF=MAX(-2,MIN(999999,INT(RVAL)))
C
C ... the green component of the background color, ...
C
        ELSE IF (CTMP.EQ.'BGG'.OR.CTMP.EQ.'bgg') THEN
C
          CALL SLSCLR (IBGC,-1.,RVAL,-1.)
          IF (ICFELL('SLSETR',3).NE.0) RETURN
C
C ... the red component of the background color, ...
C
        ELSE IF (CTMP.EQ.'BGR'.OR.CTMP.EQ.'bgr') THEN
C
          CALL SLSCLR (IBGC,RVAL,-1.,-1.)
          IF (ICFELL('SLSETR',4).NE.0) RETURN
C
C ... the blue component of the foreground color, ...
C
        ELSE IF (CTMP.EQ.'FGB'.OR.CTMP.EQ.'fgb') THEN
C
          CALL SLGPAI (PNAM,4,IPAI)
          IF (IPAI.LE.0) IPAI=IFGC
          CALL SLSCLR (IPAI,-1.,-1.,RVAL)
          IF (ICFELL('SLSETR',5).NE.0) RETURN
C
C ... the default foreground color index ...
C
        ELSE IF (CTMP.EQ.'FGC'.OR.CTMP.EQ.'fgc') THEN
C
          IFGC=MAX(1,INT(RVAL))
C
C ... the foreground color fade flag ...
C
        ELSE IF (CTMP.EQ.'FGF'.OR.CTMP.EQ.'fgf') THEN
C
          IFGF=MAX(-2,MIN(999999,INT(RVAL)))
C
C ... the green component of the foreground color, ...
C
        ELSE IF (CTMP.EQ.'FGG'.OR.CTMP.EQ.'fgg') THEN
C
          CALL SLGPAI (PNAM,4,IPAI)
          IF (IPAI.LE.0) IPAI=IFGC
          CALL SLSCLR (IPAI,-1.,RVAL,-1.)
          IF (ICFELL('SLSETR',6).NE.0) RETURN
C
C ... the red component of the foreground color, ...
C
        ELSE IF (CTMP.EQ.'FGR'.OR.CTMP.EQ.'fgr') THEN
C
          CALL SLGPAI (PNAM,4,IPAI)
          IF (IPAI.LE.0) IPAI=IFGC
          CALL SLSCLR (IPAI,RVAL,-1.,-1.)
          IF (ICFELL('SLSETR',7).NE.0) RETURN
C
C ... the fade-in time, ...
C
        ELSE IF (CTMP.EQ.'FIN'.OR.CTMP.EQ.'fin') THEN
C
          TFIN=MAX(0.,RVAL)
C
C ... the fade-out time, ...
C
        ELSE IF (CTMP.EQ.'FOU'.OR.CTMP.EQ.'fou') THEN
C
          TFOU=MAX(0.,RVAL)
C
C ... the interline spacing ("gap size"), ...
C
        ELSE IF (CTMP.EQ.'GSZ'.OR.CTMP.EQ.'gsz') THEN
C
          GPSZ=MAX(0.,RVAL)
C
C ... the centering parameter, ...
C
        ELSE IF (CTMP.EQ.'ICO'.OR.CTMP.EQ.'ico') THEN
C
          ICOP=MAX(0,MIN(2,INT(RVAL)))
C
C ... the FORTRAN logical unit number for "card" input, ...
C
        ELSE IF (CTMP.EQ.'ICU'.OR.CTMP.EQ.'icu') THEN
C
          INCU=MAX(0,INT(RVAL))
C
C ... the interline spacing for practice runs, ...
C
        ELSE IF (CTMP.EQ.'INC'.OR.CTMP.EQ.'inc') THEN
C
          IJMP=MAX(1,INT(RVAL))
C
C ... the FORTRAN logical unit number for WISS, ...
C
        ELSE IF (CTMP.EQ.'LOG'.OR.CTMP.EQ.'log') THEN
C
          IWLU=MAX(0,INT(RVAL))
C
C ... the plotter X coordinate of the left edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'LX1'.OR.CTMP.EQ.'lx1') THEN
C
          RVPL=MAX(0.,MIN(1.,RVAL/32767.))
C
C ... the plotter X coordinate of the right edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'LX2'.OR.CTMP.EQ.'lx2') THEN
C
          RVPR=MAX(0.,MIN(1.,RVAL/32767.))
C
C ... the plotter Y coordinate of the bottom edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'LY1'.OR.CTMP.EQ.'ly1') THEN
C
          RVPB=MAX(0.,MIN(1.,RVAL/32767.))
C
C ... the plotter Y coordinate of the top edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'LY2'.OR.CTMP.EQ.'ly2') THEN
C
          RVPT=MAX(0.,MIN(1.,RVAL/32767.))
C
C ... the PLOTCHAR mapping flag, ...
C
        ELSE IF (CTMP.EQ.'MAP'.OR.CTMP.EQ.'map') THEN
C
          IMAP=MAX(0,INT(RVAL))
C
C ... the number of frames per second, ...
C
        ELSE IF (CTMP.EQ.'NFS'.OR.CTMP.EQ.'nfs') THEN
C
          RNFS=MAX(0.,RVAL)
C
C ... the horizontal scroll end coordinate, ...
C
        ELSE IF (CTMP.EQ.'NXE'.OR.CTMP.EQ.'nxe') THEN
C
          IXND=MAX(0,MIN(1023,INT(RVAL)))
C
C ... the horizontal scroll start coordinate, ...
C
        ELSE IF (CTMP.EQ.'NXS'.OR.CTMP.EQ.'nxs') THEN
C
          IXST=MAX(0,MIN(1023,INT(RVAL)))
C
C ... the PLOTCHAR out-of-range value, ...
C
        ELSE IF (CTMP.EQ.'ORV'.OR.CTMP.EQ.'orv') THEN
C
          OORV=RVAL
C
C ... the character size, ...
C
        ELSE IF (CTMP.EQ.'PSZ'.OR.CTMP.EQ.'psz') THEN
C
          PCSZ=MAX(0.,RVAL)
C
C ... the flag controlling suppression of background color during a
C fade-in or fade-out, ...
C
        ELSE IF (CTMP.EQ.'SBK'.OR.CTMP.EQ.'sbk') THEN
C
          IF (RVAL.EQ.0.) THEN
            IBGF=-2
          ELSE
            IBGF=-1
          END IF
C
C ... the flag controlling suppression of foreground color during a
C fade-in or fade-out, ...
C
        ELSE IF (CTMP.EQ.'SFG'.OR.CTMP.EQ.'sfg') THEN
C
          IF (RVAL.EQ.0.) THEN
            IFGF=-2
          ELSE
            IFGF=-1
          END IF
C
C ... the initial blank-frame time (for FTITLE),
C
        ELSE IF (CTMP.EQ.'TM1'.OR.CTMP.EQ.'tm1') THEN
C
          TGP1=RVAL
C
C ... the intermediate blank-frame time (for FTITLE), ...
C
        ELSE IF (CTMP.EQ.'TM2'.OR.CTMP.EQ.'tm2') THEN
C
          TGP2=RVAL
C
C ... the intermediate blank-frame time (for FTITLE), ...
C
        ELSE IF (CTMP.EQ.'TM3'.OR.CTMP.EQ.'tm3') THEN
C
          TGP3=RVAL
C
C ... the NDC Y coordinate of the bottom edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'VPB'.OR.CTMP.EQ.'vpb') THEN
C
          RVPB=MAX(0.,MIN(1.,RVAL))
C
C ... the NDC X coordinate of the left edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'VPL'.OR.CTMP.EQ.'vpl') THEN
C
          RVPL=MAX(0.,MIN(1.,RVAL))
C
C ... the NDC X coordinate of the right edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'VPR'.OR.CTMP.EQ.'vpr') THEN
C
          RVPR=MAX(0.,MIN(1.,RVAL))
C
C ... the NDC Y coordinate of the top edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'VPT'.OR.CTMP.EQ.'vpt') THEN
C
          RVPT=MAX(0.,MIN(1.,RVAL))
C
C ... or the workstation identifier for WISS.
C
        ELSE IF (CTMP.EQ.'WID'.OR.CTMP.EQ.'wid') THEN
C
          IWWI=MAX(0,INT(RVAL))
C
        ELSE
C
C Otherwise, the parameter name is not recognized.
C
          GO TO 901
C
        END IF
C
C Normal exit.
C
        RETURN
C
C Error exit.
C
  901   CMSG(1:29)='SLSETR - INVALID KEYWORD: '//CTMP
        CALL SETER (CMSG(1:29),8,1)
        RETURN
C
      END
