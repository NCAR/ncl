C
C $Id: tdgrds.f,v 1.4 2008-07-27 00:17:32 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDGRDS (UMIN,VMIN,WMIN,UMAX,VMAX,WMAX,USTP,VSTP,WSTP,
     +                                                      IGRT,IHID)
C
C This routine may be called to draw the edges of the box containing
C a surface.  One may draw just the perimeter, the perimeter with
C inward-pointing tick marks, or the perimeter with a grid of lines.
C
C   UMIN, VMIN, WMIN, UMAX, VMAX, and WMAX are real input values, each
C   of which specifies one of the coordinate values defining the box in
C   3-space.  The names of these should make it clear what they are.
C
C   USTP, VSTP, and WSTP are real input values specifying step sizes
C   between ticks or grid lines in the U direction, the V direction,
C   and the W direction, respectively.
C
C   IGRT is an integer input value of the form 10*IGRN+IGRF, where IGRN
C   is a value specifying what to draw on the near sides of the box and
C   IGRF is a value specifying what to draw on the far sides of the box,
C   where "near" and "far" are defined by the current line of sight.
C   Each of IGRN and IGRF can have one of the values 0 (draw nothing),
C   1 (draw just a perimeter), 2 (draw a perimeter with inward-pointing
C   ticks), or 3 (draw a perimeter with a grid).
C
C   IHID is an integer input value set to 0 to draw only those sides of
C   the box that cannot be hidden by something inside the box or to 1 to
C   draw only those sides of the box that can be hidden by something
C   inside the box.  Standard operating procedure is to call TDGRDS
C   before drawing surfaces inside the box, with IHID set to 1, and
C   then call it again after drawing surfaces inside the box, this time
C   with IHID set to 0.
C
C Declare a required TDPACK common block.
C
        COMMON /TDCOM1/ IH,IT,XM,YM,ZM,XO,YO,ZO,XT,YT,ZT,OE,XE,YE,ZE
        COMMON /TDCOM1/ A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3
        COMMON /TDCOM1/ IS,FV,VL,VR,VB,VT,WL,WR,WB,WT
        SAVE   /TDCOM1/
C
C Figure out for what values of U, V, and W ticks and grid lines should
C be drawn.
C
        IF (USTP.GT.0.) THEN
          UEPS=1.E-3*(UMAX-UMIN)
          IST1=INT((UMIN+UEPS)/USTP+.5+SIGN(.5,UMIN+UEPS))
          IST2=INT((UMAX-UEPS)/USTP-.5+SIGN(.5,UMAX-UEPS))
        ELSE
          IST1=1
          IST2=0
        END IF
C
        IF (VSTP.GT.0.) THEN
          VEPS=1.E-3*(VMAX-VMIN)
          JST1=INT((VMIN+VEPS)/VSTP+.5+SIGN(.5,VMIN+VEPS))
          JST2=INT((VMAX-VEPS)/VSTP-.5+SIGN(.5,VMAX-VEPS))
        ELSE
          JST1=1
          JST2=0
        END IF
C
        IF (WSTP.GT.0.) THEN
          WEPS=1.E-3*(WMAX-WMIN)
          KST1=INT((WMIN+WEPS)/WSTP+.5+SIGN(.5,WMIN+WEPS))
          KST2=INT((WMAX-WEPS)/WSTP-.5+SIGN(.5,WMAX-WEPS))
        ELSE
          KST1=1
          KST2=0
        END IF
C
C Sort out the various cases.  In each case, we call TDPARA to define
C the parallelogram in which things will be drawn and then pass the
C buck to the underlying routine TDGRID.
C
        IF ((IHID.EQ.0.AND.XE.LE.UMIN).OR.
     +      (IHID.NE.0.AND.XE.GT.UMIN)) THEN
          IF (XM.LT..5*(UMIN+UMAX)) THEN
            IGRD=MOD(IGRT/10,10)
          ELSE
            IGRD=MOD(IGRT,10)
          END IF
          CALL TDPARA (UMIN,VMIN,WMIN,0.,VMAX-VMIN,0.,0.,0.,WMAX-WMIN)
          CALL TDGRID ((REAL(JST1)*VSTP-VMIN)/(VMAX-VMIN),
     +                 VSTP/(VMAX-VMIN),JST2-JST1+1,
     +                 (REAL(KST1)*WSTP-WMIN)/(WMAX-WMIN),
     +                 WSTP/(WMAX-WMIN),KST2-KST1+1,IGRD)
        END IF
C
        IF ((IHID.NE.0.AND.XE.LE.UMAX).OR.
     +      (IHID.EQ.0.AND.XE.GT.UMAX)) THEN
          IF (XM.GE..5*(UMIN+UMAX)) THEN
            IGRD=MOD(IGRT/10,10)
          ELSE
            IGRD=MOD(IGRT,10)
          END IF
          CALL TDPARA (UMAX,VMIN,WMIN,0.,VMAX-VMIN,0.,0.,0.,WMAX-WMIN)
          CALL TDGRID ((REAL(JST1)*VSTP-VMIN)/(VMAX-VMIN),
     +                 VSTP/(VMAX-VMIN),JST2-JST1+1,
     +                 (REAL(KST1)*WSTP-WMIN)/(WMAX-WMIN),
     +                 WSTP/(WMAX-WMIN),KST2-KST1+1,IGRD)
        END IF
C
        IF ((IHID.EQ.0.AND.YE.LE.VMIN).OR.
     +      (IHID.NE.0.AND.YE.GT.VMIN)) THEN
          IF (YM.LT..5*(VMIN+VMAX)) THEN
            IGRD=MOD(IGRT/10,10)
          ELSE
            IGRD=MOD(IGRT,10)
          END IF
          CALL TDPARA (UMIN,VMIN,WMIN,UMAX-UMIN,0.,0.,0.,0.,WMAX-WMIN)
          CALL TDGRID ((REAL(IST1)*USTP-UMIN)/(UMAX-UMIN),
     +                 USTP/(UMAX-UMIN),IST2-IST1+1,
     +                 (REAL(KST1)*WSTP-WMIN)/(WMAX-WMIN),
     +                 WSTP/(WMAX-WMIN),KST2-KST1+1,IGRD)
        END IF
C
        IF ((IHID.NE.0.AND.YE.LE.VMAX).OR.
     +      (IHID.EQ.0.AND.YE.GT.VMAX)) THEN
          IF (YM.GE..5*(VMIN+VMAX)) THEN
            IGRD=MOD(IGRT/10,10)
          ELSE
            IGRD=MOD(IGRT,10)
          END IF
          CALL TDPARA (UMIN,VMAX,WMIN,UMAX-UMIN,0.,0.,0.,0.,WMAX-WMIN)
          CALL TDGRID ((REAL(IST1)*USTP-UMIN)/(UMAX-UMIN),
     +                 USTP/(UMAX-UMIN),IST2-IST1+1,
     +                 (REAL(KST1)*WSTP-WMIN)/(WMAX-WMIN),
     +                 WSTP/(WMAX-WMIN),KST2-KST1+1,IGRD)
        END IF
C
        IF ((IHID.EQ.0.AND.ZE.LE.WMIN).OR.
     +      (IHID.NE.0.AND.ZE.GT.WMIN)) THEN
          IF (ZM.LT..5*(WMIN+WMAX)) THEN
            IGRD=MOD(IGRT/10,10)
          ELSE
            IGRD=MOD(IGRT,10)
          END IF
          CALL TDPARA (UMIN,VMIN,WMIN,UMAX-UMIN,0.,0.,0.,VMAX-VMIN,0.)
          CALL TDGRID ((REAL(IST1)*USTP-UMIN)/(UMAX-UMIN),
     +                 USTP/(UMAX-UMIN),IST2-IST1+1,
     +                 (REAL(JST1)*VSTP-VMIN)/(VMAX-VMIN),
     +                 VSTP/(VMAX-VMIN),JST2-JST1+1,IGRD)
        END IF
C
        IF ((IHID.NE.0.AND.ZE.LE.WMAX).OR.
     +      (IHID.EQ.0.AND.ZE.GT.WMAX)) THEN
          IF (ZM.GE..5*(WMIN+WMAX)) THEN
            IGRD=MOD(IGRT/10,10)
          ELSE
            IGRD=MOD(IGRT,10)
          END IF
          CALL TDPARA (UMIN,VMIN,WMAX,UMAX-UMIN,0.,0.,0.,VMAX-VMIN,0.)
          CALL TDGRID ((REAL(IST1)*USTP-UMIN)/(UMAX-UMIN),
     +                 USTP/(UMAX-UMIN),IST2-IST1+1,
     +                 (REAL(JST1)*VSTP-VMIN)/(VMAX-VMIN),
     +                 VSTP/(VMAX-VMIN),JST2-JST1+1,IGRD)
        END IF
C
C Force a buffer flush.
C
        CALL PLOTIF (0.,0.,2)
C
C Done.
C
        RETURN
C
      END
