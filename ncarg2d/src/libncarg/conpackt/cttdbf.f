C
C $Id: cttdbf.f,v 1.1 2004-03-26 21:00:09 kennison Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU General Public License as published
C by the Free Software Foundation; either version 2 of the License, or
C (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C General Public License for more details.
C
C You should have received a copy of the GNU General Public License
C along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE CTTDBF (RPNT,IEDG,ITRI,RWRK,IWRK,IFLG,ATOL)
C
      DIMENSION RPNT(*),IEDG(*),ITRI(*),RWRK(*),IWRK(*)
C
C This routine assumes that TDPACK routines are being used to map the
C triangular mesh from 3-space into 2-space.  It sets blocking flags
C for the triangles in the triangle list so as to block those that are
C seen from the wrong side or too nearly edge on and/or those that are
C invisible because they are behind other triangles of the mesh.  The
C partially blocked mesh can then be used to draw contour lines; if the
C mesh is a fine one, this can do a fair job of solving the hidden-line
C problem.
C
C RPNT is an array of nodes defining vertices of triangles.
C
C IEDG is an array of nodes defining edges of triangles.
C
C ITRI is an array of nodes defining triangles.
C
C RWRK is the user's real workspace array.
C
C IWRK is the user's integer workspace array.
C
C IFLG is a flag set to 1 to block triangles that are seen from the
C wrong side or too nearly edge on, to 2 to block triangles that are
C invisible because they are hidden by other triangles, or to 3 to do
C both of the above.  One can also set IFLG to zero to simply clear
C the blocking flags that this routine is capable of setting.
C
C ATOL is a tolerance, in degrees, to be used in determining whether
C or not a triangle is nearly edge-on to the line of sight.  Use a
C value near zero.
C
C Declare all of the CONPACKT common blocks.
C
C
C CTCOM1 contains integer and real variables.
C
      COMMON /CTCOM1/ ANCF,ANHL,ANIL,ANLL,CDMX,CHWM,CINS,CINT(10)
      COMMON /CTCOM1/ CINU,CLDB(256),CLDL(256),CLDR(256)
      COMMON /CTCOM1/ CLDT(256),CLEV(256),CLWA(258),CXCF
      COMMON /CTCOM1/ CXIL,CYCF,CYIL,DBLF,DBLM,DBLN,DBLV,DFLD,DMAX
      COMMON /CTCOM1/ DMIN,DOPT,DVAL,EPSI,FNCM,GRAV,GRSD,GSDM,HCHL
      COMMON /CTCOM1/ HCHS,HLSR,IAIA(258),IAIB(256),IBCF,IBHL
      COMMON /CTCOM1/ IBIL,IBLL,ICAF,ICCF,ICCL(258),ICFF,ICHI
      COMMON /CTCOM1/ ICHL,ICIL,ICLL(256),ICLO,ICLP(256),ICLS
      COMMON /CTCOM1/ ICLU(258),ICLV,ICLW,IDUF,IGCL,IGLB,IGRM
      COMMON /CTCOM1/ IGRN,IGVS,IHCF,IHLE,IIWS(2),IIWU,ILBC
      COMMON /CTCOM1/ IMPF,INCX(8),INCY(8),INHL,INIL,INIT,INLL
      COMMON /CTCOM1/ IOCF,IOHL,IOLL,IPAI,IPCF,IPIC,IPIE,IPIL,IPLL
      COMMON /CTCOM1/ IRWS(4),IRWU,ISET,ITBM,IWSO,JODP,JOMA
      COMMON /CTCOM1/ JOTZ,LCTM,LEA1,LEA2,LEA3,LEE1,LEE2,LEE3,LINS
      COMMON /CTCOM1/ LINT(10),LINU,LIWB,LIWK,LIWM,LIWS(2),LNLG
      COMMON /CTCOM1/ LOEN,LOPN,LOTN,LRWC,LRWG,LRWK,LRWM,LRWS(4)
      COMMON /CTCOM1/ LSDD,LSDL,LSDM,LTCF,LTHI,LTIL,LTLO,MIRO
      COMMON /CTCOM1/ NCLB(256),NCLV,NDGL,NEDG,NEXL,NEXT,NEXU
      COMMON /CTCOM1/ NLBS,NLSD,NLZF,NOMF,NOVS,NPNT,NR04,NSDL
      COMMON /CTCOM1/ NSDR,NTRI,OORV,PITH,SCFS,SCFU,SEGL,T2DS
      COMMON /CTCOM1/ UCMN,UCMX,UVPB,UVPL,UVPR,UVPS,UVPT,UWDB,UWDL
      COMMON /CTCOM1/ UWDR,UWDT,WCCF,WCHL,WCIL,WCLL,WLCF,WLHL,WLIL
      COMMON /CTCOM1/ WLLL,WOCH,WODA,WTCD,WTGR,WTNC,WTOD,WWCF,WWHL
      COMMON /CTCOM1/ WWIL,WWLL,XLBC,XMAX,XMIN,XVPL,XVPR,XWDL,XWDR
      COMMON /CTCOM1/ YLBC,YMAX,YMIN,YVPB,YVPT,YWDB,YWDT,ZMAX,ZMIN
C
      EQUIVALENCE (IIWS(1),II01),(LIWS(1),LI01)
      EQUIVALENCE (IIWS(2),II02),(LIWS(2),LI02)
      EQUIVALENCE (IRWS(1),IR01),(LRWS(1),LR01)
      EQUIVALENCE (IRWS(2),IR02),(LRWS(2),LR02)
      EQUIVALENCE (IRWS(3),IR03),(LRWS(3),LR03)
      EQUIVALENCE (IRWS(4),IR04),(LRWS(4),LR04)
      SAVE   /CTCOM1/
C
C CTCOM2 holds character parameters.
C
      COMMON /CTCOM2/ CHEX,CLBL(256),CLDP(258),CTMA,CTMB,FRMT
      COMMON /CTCOM2/ TXCF,TXHI,TXIL,TXLO
      CHARACTER*13 CHEX
      CHARACTER*64 CLBL
      CHARACTER*128 CLDP
      CHARACTER*500 CTMA,CTMB
      CHARACTER*8 FRMT
      CHARACTER*64 TXCF
      CHARACTER*32 TXHI
      CHARACTER*128 TXIL
      CHARACTER*32 TXLO
      SAVE   /CTCOM2/
C
C
C The variables in the following common block define TDPACK's mapping
C from 3-space to 2-space.
C
      COMMON /TDCOM1/ IH,IT,XM,YM,ZM,XO,YO,ZO,XT,YT,ZT,OE,XE,YE,ZE
      COMMON /TDCOM1/ A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3
      COMMON /TDCOM1/ IS,FV,VL,VR,VB,VT,WL,WR,WB,WT
      SAVE   /TDCOM1/
C
C Declare a radians-to-degrees conversion constant.
C
      DATA RTOD / 57.2957795130823 /
C
C SIDE(X1,Y1,X2,Y2,X3,Y3) is negative if the three vertices of a given
C triangle in the plane are in clockwise order, positive if they are in
C counterclockwise order.  A zero value means that the three points are
C collinear.
C
      SIDE(X1,Y1,X2,Y2,X3,Y3)=(X1-X3)*(Y2-Y3)-(Y1-Y3)*(X2-X3)
C
C IXOR(IONE,ITWO) is the exclusive OR of the 12-bit masks IONE and ITWO.
C
      IXOR(IONE,ITWO)=IAND(IOR(IONE,ITWO),4095-IAND(IONE,ITWO))
C
C ITBF(IARG) is non-zero if and only if a triangle is blocked.
C
      ITBF(IARG)=IAND(IAND(IXOR(IARG,ITBX),ITBA),1)
C
C Check for an uncleared prior error.
C
      IF (ICFELL('CTTDBF - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C If initialization has not been done, log an error and quit.
C
      IF (INIT.EQ.0) THEN
        CALL SETER ('CTTDBF - INITIALIZATION CALL NOT DONE',2,1)
        RETURN
      END IF
C
C Extract the values of ITBX and ITBA.
C
      ITBX=IAND(ISHIFT(ITBM,-12),4095)
      ITBA=IAND(       ITBM     ,4095)
C
C Determine which bits of the blocking flags are to be used and set
C IBIT accordingly.
C
      IF (OE.GE.0.) THEN
        IBIT=2
      ELSE
        IBIT=16
      END IF
C
C Set a mask used to clear the bits that might be set.
C
      ICLR=127-7*IBIT
C
C Make a pass over the triangle list.  At the very least, this pass will
C clear the blocking bits for all triangles.  If IFLG is 1 or 3, it will
C also block triangles that are seen from the wrong side or are too
C nearly edge-on.  If IFLG is 2 or 3, it will also compute the bounding
C box for all triangles in the mesh that are not user-blocked and a
C largest extent in X and Y for any single such triangle.
C
      IF (IFLG.EQ.2.OR.IFLG.EQ.3) THEN
C
        XMNM=XWDR
        XMXM=XWDL
        YMNM=YWDT
        YMXM=YWDB
C
        XEXT=0.
        YEXT=0.
C
      END IF
C
      DO 101 IPTT=0,NTRI-LOTN,LOTN
C
C Use only triangles not blocked by the user.
C
      IF (ITBF(ITRI(IPTT+4)).NE.0) GO TO 101
C
C Clear the bits that might be set by this call.
C
      ITRI(IPTT+4)=IAND(ITRI(IPTT+4),ICLR)
C
C Find the base index of point 1 (that edges 1 and 2 have in common).
C
      IF (IEDG(ITRI(IPTT+1)+1).EQ.IEDG(ITRI(IPTT+2)+1).OR.IEDG(ITRI(IPTT
     ++1)+1).EQ.IEDG(ITRI(IPTT+2)+2)) THEN
        IPT1=IEDG(ITRI(IPTT+1)+1)
      ELSE
        IPT1=IEDG(ITRI(IPTT+1)+2)
      END IF
C
C Find the base index of point 2 (that edges 2 and 3 have in common).
C
      IF (IEDG(ITRI(IPTT+2)+1).EQ.IEDG(ITRI(IPTT+3)+1).OR.IEDG(ITRI(IPTT
     ++2)+1).EQ.IEDG(ITRI(IPTT+3)+2)) THEN
        IPT2=IEDG(ITRI(IPTT+2)+1)
      ELSE
        IPT2=IEDG(ITRI(IPTT+2)+2)
      END IF
C
C Find the base index of point 3 (that edges 3 and 1 have in common).
C
      IF (IEDG(ITRI(IPTT+3)+1).EQ.IEDG(ITRI(IPTT+1)+1).OR.IEDG(ITRI(IPTT
     ++3)+1).EQ.IEDG(ITRI(IPTT+1)+2)) THEN
        IPT3=IEDG(ITRI(IPTT+3)+1)
      ELSE
        IPT3=IEDG(ITRI(IPTT+3)+2)
      END IF
C
C Project all three points.
C
      CALL TDPRPT (RPNT(IPT1+1),RPNT(IPT1+2),RPNT(IPT1+3),XPT1,YPT1)
      CALL TDPRPT (RPNT(IPT2+1),RPNT(IPT2+2),RPNT(IPT2+3),XPT2,YPT2)
      CALL TDPRPT (RPNT(IPT3+1),RPNT(IPT3+2),RPNT(IPT3+3),XPT3,YPT3)
C
C If requested, check for triangles that are wrong-side on or edge-on.
C
      IF (IFLG.EQ.1.OR.IFLG.EQ.3) THEN
C
C If the wrong side of the triangle faces the eye, set a bit.
C
        IF (SIDE(XPT1,YPT1,XPT2,YPT2,XPT3,YPT3).LE.0.) THEN
C
          ITRI(IPTT+4)=IOR(ITRI(IPTT+4),IBIT)
C
        END IF
C
C If the triangle is nearly edge-on to the eye, set a different bit.
C
        IF (ATOL.GT.0.) THEN
C
          XDN1=RPNT(IPT1+2)*(RPNT(IPT3+3)-RPNT(IPT2+3))+
     +         RPNT(IPT2+2)*(RPNT(IPT1+3)-RPNT(IPT3+3))+
     +         RPNT(IPT3+2)*(RPNT(IPT2+3)-RPNT(IPT1+3))
          YDN1=RPNT(IPT1+1)*(RPNT(IPT2+3)-RPNT(IPT3+3))+
     +         RPNT(IPT2+1)*(RPNT(IPT3+3)-RPNT(IPT1+3))+
     +         RPNT(IPT3+1)*(RPNT(IPT1+3)-RPNT(IPT2+3))
          ZDN1=RPNT(IPT1+1)*(RPNT(IPT3+2)-RPNT(IPT2+2))+
     +         RPNT(IPT2+1)*(RPNT(IPT1+2)-RPNT(IPT3+2))+
     +         RPNT(IPT3+1)*(RPNT(IPT2+2)-RPNT(IPT1+2))
C
          XDN2=XE-(RPNT(IPT1+1)+RPNT(IPT2+1)+RPNT(IPT3+1))/3.
          YDN2=YE-(RPNT(IPT1+2)+RPNT(IPT2+2)+RPNT(IPT3+2))/3.
          ZDN2=ZE-(RPNT(IPT1+3)+RPNT(IPT2+3)+RPNT(IPT3+3))/3.
C
          IF ((XDN1.NE.0..OR.YDN1.NE.0..OR.ZDN1.NE.0.).AND.(XDN2.NE.0..O
     +R.YDN2.NE.0..OR.ZDN2.NE.0.)) THEN
            ANGD=RTOD*ABS(ACOS((XDN1*XDN2+YDN1*YDN2+ZDN1*ZDN2)/
     +                    SQRT((XDN1*XDN1+YDN1*YDN1+ZDN1*ZDN1)*
     +                         (XDN2*XDN2+YDN2*YDN2+ZDN2*ZDN2))))
          ELSE
            ANGD=90.
          END IF
C
          IF (ANGD.GT.90.-ATOL.AND.ANGD.LT.90.+ATOL) THEN
            ITRI(IPTT+4)=IOR(ITRI(IPTT+4),2*IBIT)
          END IF
C
        END IF
C
      END IF
C
C Update info required for the second pass (if that pass is to be done).
C
      IF (IFLG.EQ.2.OR.IFLG.EQ.3) THEN
C
        XMNM=MIN(XMNM,XPT1,XPT2,XPT3)
        XMXM=MAX(XMXM,XPT1,XPT2,XPT3)
        YMNM=MIN(YMNM,YPT1,YPT2,YPT3)
        YMXM=MAX(YMXM,YPT1,YPT2,YPT3)
C
        XEXT=MAX(XEXT,MAX(XPT1,XPT2,XPT3)-MIN(XPT1,XPT2,XPT3))
        YEXT=MAX(YEXT,MAX(YPT1,YPT2,YPT3)-MIN(YPT1,YPT2,YPT3))
C
      END IF
C
  101 CONTINUE
C
C We are done if the second pass is not to be done or if the bounding
C box was improperly computed for some reason.
C
      IF (IFLG.NE.2.AND.IFLG.NE.3) RETURN
C
      IF (XMNM.GE.XMXM.OR.YMNM.GE.YMXM) RETURN
C
C Grab a chunk of integer workspace to use.
C
      RWTH=(XMXM-XMNM)/(YMXM-YMNM)
      IBLM=MAX(10,INT(SQRT(RWTH*REAL(LIWB))))
      IBLN=MAX(10,LIWB/IBLM)
      CALL CTGIWS (IWRK,1,IBLM*IBLN,IWSE)
      IF (IWSE.NE.0.OR.ICFELL('CTTDBF',3).NE.0) GO TO 105
C
C For pass two, we use the next higher bit in the blocking flag.
C
      IBIT=IBIT*4
C
C Sort the triangles into an IBLMxIBLN array of bins.  This should help
C to speed up our search for those that overlap each other.  First,
C initialize all the bin pointers to nulls.
C
      DO 10001 I=1,IBLM*IBLN
        IWRK(II01+I)=0
10001 CONTINUE
C
C Put each triangle that isn't blocked by the user into one of the bins,
C based on the position of its center point in user space.
C
      DO 102 IPTT=0,NTRI-LOTN,LOTN
C
      IF (ITBF(ITRI(IPTT+4)).NE.0) GO TO 102
C
      IF (IEDG(ITRI(IPTT+1)+1).EQ.IEDG(ITRI(IPTT+2)+1).OR.IEDG(ITRI(IPTT
     ++1)+1).EQ.IEDG(ITRI(IPTT+2)+2)) THEN
        IPT1=IEDG(ITRI(IPTT+1)+1)
      ELSE
        IPT1=IEDG(ITRI(IPTT+1)+2)
      END IF
C
      IF (IEDG(ITRI(IPTT+2)+1).EQ.IEDG(ITRI(IPTT+3)+1).OR.IEDG(ITRI(IPTT
     ++2)+1).EQ.IEDG(ITRI(IPTT+3)+2)) THEN
        IPT2=IEDG(ITRI(IPTT+2)+1)
      ELSE
        IPT2=IEDG(ITRI(IPTT+2)+2)
      END IF
C
      IF (IEDG(ITRI(IPTT+3)+1).EQ.IEDG(ITRI(IPTT+1)+1).OR.IEDG(ITRI(IPTT
     ++3)+1).EQ.IEDG(ITRI(IPTT+1)+2)) THEN
        IPT3=IEDG(ITRI(IPTT+3)+1)
      ELSE
        IPT3=IEDG(ITRI(IPTT+3)+2)
      END IF
C
      CALL TDPRPT (RPNT(IPT1+1),RPNT(IPT1+2),RPNT(IPT1+3),XPT1,YPT1)
      CALL TDPRPT (RPNT(IPT2+1),RPNT(IPT2+2),RPNT(IPT2+3),XPT2,YPT2)
      CALL TDPRPT (RPNT(IPT3+1),RPNT(IPT3+2),RPNT(IPT3+3),XPT3,YPT3)
C
      XMDT=(XPT1+XPT2+XPT3)/3.
      YMDT=(YPT1+YPT2+YPT3)/3.
C
      I=MAX(1,MIN(IBLM,1+INT(REAL(IBLM)*((XMDT-XMNM)/(XMXM-XMNM)))))
      J=MAX(1,MIN(IBLN,1+INT(REAL(IBLN)*((YMDT-YMNM)/(YMXM-YMNM)))))
C
      ITRI(IPTT+4)=IWRK(II01+(I-1)*IBLN+J)+IAND(ITRI(IPTT+4),127)
      IWRK(II01+(I-1)*IBLN+J)=128*(IPTT/LOTN+1)
C
  102 CONTINUE
C
C Set the blocking flag for each triangle not already blocked by the
C user as implied by what's between the triangle and the eye.
C
      DO 104 IPTA=0,NTRI-LOTN,LOTN
C
C Use only triangles not blocked by the user.
C
      IF (ITBF(ITRI(IPTA+4)).NE.0) GO TO 104
C
C Find the base index of point 1 (that edges 1 and 2 have in common).
C
      IF (IEDG(ITRI(IPTA+1)+1).EQ.IEDG(ITRI(IPTA+2)+1).OR.IEDG(ITRI(IPTA
     ++1)+1).EQ.IEDG(ITRI(IPTA+2)+2)) THEN
        IPA1=IEDG(ITRI(IPTA+1)+1)
      ELSE
        IPA1=IEDG(ITRI(IPTA+1)+2)
      END IF
C
C Find the base index of point 2 (that edges 2 and 3 have in common).
C
      IF (IEDG(ITRI(IPTA+2)+1).EQ.IEDG(ITRI(IPTA+3)+1).OR.IEDG(ITRI(IPTA
     ++2)+1).EQ.IEDG(ITRI(IPTA+3)+2)) THEN
        IPA2=IEDG(ITRI(IPTA+2)+1)
      ELSE
        IPA2=IEDG(ITRI(IPTA+2)+2)
      END IF
C
C Find the base index of point 3 (that edges 3 and 1 have in common).
C
      IF (IEDG(ITRI(IPTA+3)+1).EQ.IEDG(ITRI(IPTA+1)+1).OR.IEDG(ITRI(IPTA
     ++3)+1).EQ.IEDG(ITRI(IPTA+1)+2)) THEN
        IPA3=IEDG(ITRI(IPTA+3)+1)
      ELSE
        IPA3=IEDG(ITRI(IPTA+3)+2)
      END IF
C
C Project all three points.
C
      CALL TDPRPT (RPNT(IPA1+1),RPNT(IPA1+2),RPNT(IPA1+3),XPA1,YPA1)
      CALL TDPRPT (RPNT(IPA2+1),RPNT(IPA2+2),RPNT(IPA2+3),XPA2,YPA2)
      CALL TDPRPT (RPNT(IPA3+1),RPNT(IPA3+2),RPNT(IPA3+3),XPA3,YPA3)
C
C Compute coefficients defining the plane of triangle A.
C
      ACTA=(RPNT(IPA1+2)-RPNT(IPA2+2))*
     +     (RPNT(IPA2+3)-RPNT(IPA3+3))-
     +     (RPNT(IPA2+2)-RPNT(IPA3+2))*
     +     (RPNT(IPA1+3)-RPNT(IPA2+3))
      BCTA=(RPNT(IPA1+3)-RPNT(IPA2+3))*
     +     (RPNT(IPA2+1)-RPNT(IPA3+1))-
     +     (RPNT(IPA2+3)-RPNT(IPA3+3))*
     +     (RPNT(IPA1+1)-RPNT(IPA2+1))
      CCTA=(RPNT(IPA1+1)-RPNT(IPA2+1))*
     +     (RPNT(IPA2+2)-RPNT(IPA3+2))-
     +     (RPNT(IPA2+1)-RPNT(IPA3+1))*
     +     (RPNT(IPA1+2)-RPNT(IPA2+2))
C
      DNOM=SQRT(ACTA*ACTA+BCTA*BCTA+CCTA*CCTA)
C
      ACTA=ACTA/DNOM
      BCTA=BCTA/DNOM
      CCTA=CCTA/DNOM
C
      DCTA=-ACTA*RPNT(IPA1+1)-BCTA*RPNT(IPA1+2)
     +                       -CCTA*RPNT(IPA1+3)
C
C Compute the minimum and maximum X and Y for a box around triangle A
C in which the center of an overlapping triangle might lie.
C
      XMNT=MIN(XPA1,XPA2,XPA3)-.5*XEXT
      XMXT=MAX(XPA1,XPA2,XPA3)+.5*XEXT
      YMNT=MIN(YPA1,YPA2,YPA3)-.5*YEXT
      YMXT=MAX(YPA1,YPA2,YPA3)+.5*YEXT
C
C See which bins we need to look at to be sure of finding any triangle
C that could overlap triangle A.
C
      IMIN=MAX(1,MIN(IBLM,1+INT(REAL(IBLM)*
     +                                  ((XMNT-XMNM)/(XMXM-XMNM)))))
      IMAX=MAX(1,MIN(IBLM,1+INT(REAL(IBLM)*
     +                                  ((XMXT-XMNM)/(XMXM-XMNM)))))
      JMIN=MAX(1,MIN(IBLN,1+INT(REAL(IBLN)*
     +                                  ((YMNT-YMNM)/(YMXM-YMNM)))))
      JMAX=MAX(1,MIN(IBLN,1+INT(REAL(IBLN)*
     +                                  ((YMXT-YMNM)/(YMXM-YMNM)))))
C
C Loop through all the bins.
C
      DO 10002 I=IMIN,IMAX
C
        DO 10003 J=JMIN,JMAX
C
C Test each triangle in the bin to see if it is between triangle A and
C the eye.
C
          IPTB=(IWRK(II01+(I-1)*IBLN+J)/128-1)*LOTN
C
10004     CONTINUE
          IF (.NOT.(IPTB.GE.0)) GO TO 10005
C
C Don't compare the triangle with itself.
C
            IF (IPTB.EQ.IPTA) GO TO 103
C
C Find the base index of point 1 (that edges 1 and 2 have in common).
C
            IF (IEDG(ITRI(IPTB+1)+1).EQ.IEDG(ITRI(IPTB+2)+1).OR.IEDG(ITR
     +I(IPTB+1)+1).EQ.IEDG(ITRI(IPTB+2)+2)) THEN
              IPB1=IEDG(ITRI(IPTB+1)+1)
            ELSE
              IPB1=IEDG(ITRI(IPTB+1)+2)
            END IF
C
C Find the base index of point 2 (that edges 2 and 3 have in common).
C
            IF (IEDG(ITRI(IPTB+2)+1).EQ.IEDG(ITRI(IPTB+3)+1).OR.IEDG(ITR
     +I(IPTB+2)+1).EQ.IEDG(ITRI(IPTB+3)+2)) THEN
              IPB2=IEDG(ITRI(IPTB+2)+1)
            ELSE
              IPB2=IEDG(ITRI(IPTB+2)+2)
            END IF
C
C Find the base index of point 3 (that edges 3 and 1 have in common).
C
            IF (IEDG(ITRI(IPTB+3)+1).EQ.IEDG(ITRI(IPTB+1)+1).OR.IEDG(ITR
     +I(IPTB+3)+1).EQ.IEDG(ITRI(IPTB+1)+2)) THEN
              IPB3=IEDG(ITRI(IPTB+3)+1)
            ELSE
              IPB3=IEDG(ITRI(IPTB+3)+2)
            END IF
C
C Project all three points.
C
            CALL TDPRPT (RPNT(IPB1+1),RPNT(IPB1+2),RPNT(IPB1+3),
     +                                                XPB1,YPB1)
            CALL TDPRPT (RPNT(IPB2+1),RPNT(IPB2+2),RPNT(IPB2+3),
     +                                                XPB2,YPB2)
            CALL TDPRPT (RPNT(IPB3+1),RPNT(IPB3+2),RPNT(IPB3+3),
     +                                                XPB3,YPB3)
C
C See if projected triangles overlap and, if so, get coordinates of a
C point they have in common.
C
            CALL CTPITT (XPA1,YPA1,XPA2,YPA2,XPA3,YPA3,
     +                   XPB1,YPB1,XPB2,YPB2,XPB3,YPB3,
     +                   XPI2,YPI2,INTF)
C
C If they do have a point in common ...
C
            IF (INTF.NE.0) THEN
C
C ... compute 3-space coordinates of that point, ...
C
              XPI3=XO+XPI2*A2+YPI2*A3
              YPI3=YO+XPI2*B2+YPI2*B3
              ZPI3=ZO+XPI2*C2+YPI2*C3
C
C ... compute coefficients defining the plane of the 2nd triangle, ...
C
              ACTB=(RPNT(IPB1+2)-RPNT(IPB2+2))*
     +             (RPNT(IPB2+3)-RPNT(IPB3+3))-
     +             (RPNT(IPB2+2)-RPNT(IPB3+2))*
     +             (RPNT(IPB1+3)-RPNT(IPB2+3))
              BCTB=(RPNT(IPB1+3)-RPNT(IPB2+3))*
     +             (RPNT(IPB2+1)-RPNT(IPB3+1))-
     +             (RPNT(IPB2+3)-RPNT(IPB3+3))*
     +             (RPNT(IPB1+1)-RPNT(IPB2+1))
              CCTB=(RPNT(IPB1+1)-RPNT(IPB2+1))*
     +             (RPNT(IPB2+2)-RPNT(IPB3+2))-
     +             (RPNT(IPB2+1)-RPNT(IPB3+1))*
     +             (RPNT(IPB1+2)-RPNT(IPB2+2))
C
              DNOM=SQRT(ACTB*ACTB+BCTB*BCTB+CCTB*CCTB)
C
              ACTB=ACTB/DNOM
              BCTB=BCTB/DNOM
              CCTB=CCTB/DNOM
C
              DCTB=-ACTB*RPNT(IPB1+1)-BCTB*RPNT(IPB1+2)
     +                               -CCTB*RPNT(IPB1+3)
C
C ... find out for what values of S the line from the eye to the point
C intersects the triangles, ...
C
              SFTA=-(ACTA*XE+BCTA*YE+CCTA*ZE+DCTA)/
     +              (ACTA*(XPI3-XE)+BCTA*(YPI3-YE)+CCTA*(ZPI3-ZE))
C
              SFTB=-(ACTB*XE+BCTB*YE+CCTB*ZE+DCTB)/
     +              (ACTB*(XPI3-XE)+BCTB*(YPI3-YE)+CCTB*(ZPI3-ZE))
C
C ... and, if the first triangle is further away from the eye than the
C second one, block it.
C
              IF (SFTA.GT.1.0001*SFTB) THEN
                ITRI(IPTA+4)=IOR(ITRI(IPTA+4),IBIT)
                GO TO 104
              END IF
C
            END IF
C
  103       IPTB=(ITRI(IPTB+4)/128-1)*LOTN
C
          GO TO 10004
10005     CONTINUE
C
10003   CONTINUE
C
10002 CONTINUE
C
  104 CONTINUE
C
C Clear the upper bits in the blocking flags.
C
      DO 10006 IPTT=0,NTRI-LOTN,LOTN
        ITRI(IPTT+4)=IAND(ITRI(IPTT+4),127)
10006 CONTINUE
C
C Release the integer workspace.
C
  105 LI01=0
C
C Done.
C
      RETURN
C
      END
