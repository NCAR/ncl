C
C $Id: issmth.f,v 1.5 2008-07-27 00:17:16 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE ISSMTH (XSVE,YSVE,NSVE,IFSF)
C
      DIMENSION XSVE(*),YSVE(*)
C
C This routine, given the coordinates of points defining a segment of
C a contour line, smooths the line and then passes points defining the
C smoothed line on to the routine ISPLTF for drawing and/or marking of
C the screen models.  (The end points are omitted, because they are
C passed to ISPLTF separately.)
C
      COMMON /ISCOMN/ BIG,BIGD,DBPI,IDONE,IFILL,IFLIP,IONES,IHSS(1000),
     +                IREF,ISCA(16,256),ISCALE,ISCR(16,256),ISLBT,ISDR,
     +                LHSS,LX,NFPW,NINU,NINV,NINW,NX,NY,R0,RNX,RNY,
     +                SMALL,SVAL,TENSN,U,V,W,XMAX,XMIN,YMAX,YMIN,XVPL,
     +                XVPR,YVPB,YVPT
      SAVE   /ISCOMN/
C
C Define arrays needed by the smoothing routines; they must be
C dimensioned the same as XSVE and YSVE, in ISTRCL.
C
        DIMENSION TMP1(100),TMP2(100),TMP3(100),SLEN(100)
C
C Certain quantities must be saved between calls.
C
        SAVE XBFS,YBFS,SBFS,XELS,YELS,SELS
C
C Supply some default values just to keep the code from blowing up the
C first time through.
C
        DATA XBFS,YBFS,XELS,YELS / 0.,0.,0.,0. /
C
C If the line to be smoothed is a closed loop drawn using one segment
C and appropriate conditions are met, let the smoother estimate the
C slope at both ends.
C
        IF (IFSF.NE.0.AND.NSVE.GE.3.AND.
     +      ABS(XSVE(NSVE)-XSVE(1)).LT.SMALL.AND.
     +      ABS(YSVE(NSVE)-YSVE(1)).LT.SMALL) THEN
          ISLP=4
C
C If the line to be smoothed is a continuation of the last one, make the
C slope at its beginning match the slope at the end of the last one ...
C
        ELSE IF (IFSF.EQ.0.AND.
     +           ABS(XSVE(1)-XELS).LT.SMALL.AND.
     +           ABS(YSVE(1)-YELS).LT.SMALL) THEN
          ISLP=1
          SLP1=SELS
C
C ... and if, in addition, this segment closes the loop, supply it
C with a final slope matching the slope at the beginning of the first
C segment.
C
          IF (ABS(XSVE(NSVE)-XBFS).LT.SMALL.AND.
     +        ABS(YSVE(NSVE)-YBFS).LT.SMALL) THEN
            ISLP=0
            SLPN=SBFS
          END IF
C
C If none of the above, let the smoother estimate slopes.
C
        ELSE
          ISLP=3
        END IF
C
C Initialize the smoother.
C
        CALL MSKRV1 (NSVE,XSVE,YSVE,SLP1,SLPN,TMP1,TMP2,TMP3,SLEN,
     +                                                 TENSN,ISLP)
C
C Generate points on the smoothed line and deliver them to ISPLTF.
C
        NINT=MAX(3,INT(SLEN(NSVE)/DBPI)-1)
C
        DO 101 IINT=1,NINT
          CALL MSKRV2 (REAL(IINT)/REAL(NINT+1),XTMP,YTMP,NSVE,
     +                 XSVE,YSVE,TMP1,TMP2,SLEN,TENSN,0,DUMI)
          CALL ISPLTF (XTMP,YTMP,2)
  101   CONTINUE
C
C If this is the first segment, save the position at its beginning
C and the slope there.
C
        IF (IFSF.NE.0) THEN
          XBFS=XSVE(1)
          YBFS=YSVE(1)
          CALL MSKRV2 (0.,XTMP,YTMP,NSVE,XSVE,YSVE,TMP1,TMP2,SLEN,
     +                                               TENSN,1,SBFS)
        END IF
C
C In any case, save the slope at the end of the segment.
C
        XELS=XSVE(NSVE)
        YELS=YSVE(NSVE)
        CALL MSKRV2 (1.,XTMP,YTMP,NSVE,XSVE,YSVE,TMP1,TMP2,SLEN,
     +                                             TENSN,1,SELS)
C
C Done.
C
        RETURN
C
      END
