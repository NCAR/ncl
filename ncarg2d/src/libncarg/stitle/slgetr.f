C
C $Id: slgetr.f,v 1.7 2008-07-27 00:17:26 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE SLGETR (PNAM,RVAL)
C
C Get in RVAL the real value of the STITLE parameter named PNAM.
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
        IF (ICFELL('SLGETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
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
          RVAL=REAL(IDOT)
C
C ... the blue component of the background color, ...
C
        ELSE IF (CTMP.EQ.'BGB'.OR.CTMP.EQ.'bgb') THEN
C
          CALL SLGCLR (IBGC,DUMI,DUMI,RVAL)
          IF (ICFELL('SLGETR',2).NE.0) RETURN
C
C ... the default background color index ...
C
        ELSE IF (CTMP.EQ.'BGC'.OR.CTMP.EQ.'bgc') THEN
C
          RVAL=REAL(IBGC)
C
C ... the background color fade flag ...
C
        ELSE IF (CTMP.EQ.'BGF'.OR.CTMP.EQ.'bgf') THEN
C
          RVAL=REAL(IBGF)
C
C ... the green component of the background color, ...
C
        ELSE IF (CTMP.EQ.'BGG'.OR.CTMP.EQ.'bgg') THEN
C
          CALL SLGCLR (IBGC,DUMI,RVAL,DUMI)
          IF (ICFELL('SLGETR',3).NE.0) RETURN
C
C ... the red component of the background color, ...
C
        ELSE IF (CTMP.EQ.'BGR'.OR.CTMP.EQ.'bgr') THEN
C
          CALL SLGCLR (IBGC,RVAL,DUMI,DUMI)
          IF (ICFELL('SLGETR',4).NE.0) RETURN
C
C ... the blue component of the foreground color, ...
C
        ELSE IF (CTMP.EQ.'FGB'.OR.CTMP.EQ.'fgb') THEN
C
          CALL SLGPAI (PNAM,4,IPAI)
          IF (IPAI.LE.0) IPAI=IFGC
          CALL SLGCLR (IPAI,DUMI,DUMI,RVAL)
          IF (ICFELL('SLGETR',5).NE.0) RETURN
C
C ... the default foreground color index ...
C
        ELSE IF (CTMP.EQ.'FGC'.OR.CTMP.EQ.'fgc') THEN
C
          RVAL=REAL(IFGC)
C
C ... the foreground color fade flag ...
C
        ELSE IF (CTMP.EQ.'FGF'.OR.CTMP.EQ.'fgf') THEN
C
          RVAL=REAL(IFGF)
C
C ... the green component of the foreground color, ...
C
        ELSE IF (CTMP.EQ.'FGG'.OR.CTMP.EQ.'fgg') THEN
C
          CALL SLGPAI (PNAM,4,IPAI)
          IF (IPAI.LE.0) IPAI=IFGC
          CALL SLGCLR (IPAI,DUMI,RVAL,DUMI)
          IF (ICFELL('SLGETR',6).NE.0) RETURN
C
C ... the red component of the foreground color, ...
C
        ELSE IF (CTMP.EQ.'FGR'.OR.CTMP.EQ.'fgr') THEN
C
          CALL SLGPAI (PNAM,4,IPAI)
          IF (IPAI.LE.0) IPAI=IFGC
          CALL SLGCLR (IPAI,RVAL,DUMI,DUMI)
          IF (ICFELL('SLGETR',7).NE.0) RETURN
C
C ... the fade-in time, ...
C
        ELSE IF (CTMP.EQ.'FIN'.OR.CTMP.EQ.'fin') THEN
C
          RVAL=TFIN
C
C ... the fade-out time, ...
C
        ELSE IF (CTMP.EQ.'FOU'.OR.CTMP.EQ.'fou') THEN
C
          RVAL=TFOU
C
C ... the interline spacing ("gap size"), ...
C
        ELSE IF (CTMP.EQ.'GSZ'.OR.CTMP.EQ.'gsz') THEN
C
          RVAL=GPSZ
C
C ... the centering parameter, ...
C
        ELSE IF (CTMP.EQ.'ICO'.OR.CTMP.EQ.'ico') THEN
C
          RVAL=REAL(ICOP)
C
C ... the FORTRAN logical unit number for "card" input, ...
C
        ELSE IF (CTMP.EQ.'ICU'.OR.CTMP.EQ.'icu') THEN
C
          RVAL=REAL(INCU)
C
C ... the interline spacing for practice runs, ...
C
        ELSE IF (CTMP.EQ.'INC'.OR.CTMP.EQ.'inc') THEN
C
          RVAL=REAL(IJMP)
C
C ... the FORTRAN logical unit number for WISS, ...
C
        ELSE IF (CTMP.EQ.'LOG'.OR.CTMP.EQ.'log') THEN
C
          RVAL=REAL(IWLU)
C
C ... the plotter X coordinate of the left edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'LX1'.OR.CTMP.EQ.'lx1') THEN
C
          RVAL=32767.*RVPL
C
C ... the plotter X coordinate of the right edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'LX2'.OR.CTMP.EQ.'lx2') THEN
C
          RVAL=32767.*RVPR
C
C ... the plotter Y coordinate of the bottom edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'LY1'.OR.CTMP.EQ.'ly1') THEN
C
          RVAL=32767.*RVPB
C
C ... the plotter Y coordinate of the top edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'LY2'.OR.CTMP.EQ.'ly2') THEN
C
          RVAL=32767.*RVPT
C
C ... the PLOTCHAR mapping flag, ...
C
        ELSE IF (CTMP.EQ.'MAP'.OR.CTMP.EQ.'map') THEN
C
          RVAL=REAL(IMAP)
C
C ... the number of frames per second, ...
C
        ELSE IF (CTMP.EQ.'NFS'.OR.CTMP.EQ.'nfs') THEN
C
          RVAL=RNFS
C
C ... the horizontal scroll end coordinate, ...
C
        ELSE IF (CTMP.EQ.'NXE'.OR.CTMP.EQ.'nxe') THEN
C
          RVAL=REAL(IXND)
C
C ... the horizontal scroll start coordinate, ...
C
        ELSE IF (CTMP.EQ.'NXS'.OR.CTMP.EQ.'nxs') THEN
C
          RVAL=REAL(IXST)
C
C ... the PLOTCHAR out-of-range value, ...
C
        ELSE IF (CTMP.EQ.'ORV'.OR.CTMP.EQ.'orv') THEN
C
          RVAL=OORV
C
C ... the character size, ...
C
        ELSE IF (CTMP.EQ.'PSZ'.OR.CTMP.EQ.'psz') THEN
C
          RVAL=PCSZ
C
C ... the flag controlling suppression of background color during a
C fade-in or fade-out, ...
C
        ELSE IF (CTMP.EQ.'SBK'.OR.CTMP.EQ.'sbk') THEN
C
          IF (IBGF.NE.-1) THEN
            RVAL=0.
          ELSE
            RVAL=1.
          END IF
C
C ... the flag controlling suppression of foreground color during a
C fade-in or fade-out, ...
C
        ELSE IF (CTMP.EQ.'SFG'.OR.CTMP.EQ.'sfg') THEN
C
          IF (IFGF.NE.-1) THEN
            RVAL=0.
          ELSE
            RVAL=1.
          END IF
C
C ... the initial blank-frame count (for FTITLE), ...
C
        ELSE IF (CTMP.EQ.'TM1'.OR.CTMP.EQ.'tm1') THEN
C
          RVAL=TGP1
C
C ... the intermediate blank-frame count (for FTITLE), ...
C
        ELSE IF (CTMP.EQ.'TM2'.OR.CTMP.EQ.'tm2') THEN
C
          RVAL=TGP2
C
C ... the terminal extra blank-frame count (for FTITLE), ...
C
        ELSE IF (CTMP.EQ.'TM3'.OR.CTMP.EQ.'tm3') THEN
C
          RVAL=TGP2
C
C ... the NDC Y coordinate of the bottom edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'VPB'.OR.CTMP.EQ.'vpb') THEN
C
          RVAL=RVPB
C
C ... the NDC X coordinate of the left edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'VPL'.OR.CTMP.EQ.'vpl') THEN
C
          RVAL=RVPL
C
C ... the NDC X coordinate of the right edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'VPR'.OR.CTMP.EQ.'vpr') THEN
C
          RVAL=RVPR
C
C ... the NDC Y coordinate of the top edge of the viewport, ...
C
        ELSE IF (CTMP.EQ.'VPT'.OR.CTMP.EQ.'vpt') THEN
C
          RVAL=RVPT
C
C ... or the workstation identifier for WISS.
C
        ELSE IF (CTMP.EQ.'WID'.OR.CTMP.EQ.'wid') THEN
C
          RVAL=REAL(IWWI)
C
        ELSE
C
C Otherwise, the parameter name is not recognized.
C
          GO TO 901
C
        END IF
C
        RETURN
C
C Error exit.
C
  901   CMSG(1:29)='SLGETR - INVALID KEYWORD: '//CTMP
        CALL SETER (CMSG(1:29),8,1)
        RETURN
C
      END
