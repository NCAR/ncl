C
C $Id: tdlbls.f,v 1.4 2008-07-27 00:17:32 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE TDLBLS (UMIN,VMIN,WMIN,UMAX,VMAX,WMAX,
     +                   UNLB,VNLB,WNLB,UILB,VILB,WILB,IPCK)
C
        CHARACTER*(*) UILB,VILB,WILB,UNLB,VNLB,WNLB
C
C This routine may be called to label the axes of the box in 3-space
C within which a surface is being drawn.  Its arguments are as follows:
C
C   UMIN, VMIN, WMIN, UMAX, VMAX, and WMAX are real input values, each
C   of which specifies one of the coordinate values defining the box in
C   3-space.  The names of these should make it clear what they are.
C
C   UNLB, VNLB, and WNLB are character strings containing numeric labels
C   to be placed on a U axis, a V axis, or a W axis, respectively.  The
C   labels need not be in any particular order, but they have to be
C   separated by blanks and each has to be readable using a FORTRAN
C   format of the form "En.0", where "n" is the length of the label.
C
C   UILB, VILB, and WILB are character strings containing informational
C   labels for a U axis, a V axis, or a W axis, respectively.
C
C   IPCK is an input integer determining which edges of the box are
C   labelled.  If IPCK is zero, all six outer edges are labelled, but
C   if IPCK is non-zero, only three edges are labelled, one set of
C   three if IPCK is negative, a different set if IPCK is positive.
C
C Declare required TDPACK common blocks.
C
        COMMON /TDCOM1/ IH,IT,XM,YM,ZM,XO,YO,ZO,XT,YT,ZT,OE,XE,YE,ZE
        COMMON /TDCOM1/ A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3
        COMMON /TDCOM1/ IS,FV,VL,VR,VB,VT,WL,WR,WB,WT
        SAVE   /TDCOM1/
C
        COMMON /TDCOM4/ CSM1,CSM2
        SAVE   /TDCOM4/
C
C Declare two variables in which to put logical values indicating which
C axes are to be labelled.
C
        LOGICAL IPGZ,IPLZ
C
C Compute logical values indicating which axes are to be labelled.
C
        IPGZ=IPCK.GE.0
        IPLZ=IPCK.LE.0
C
C Compute the coordinates of the center point of the box.
C
        UMID=.5*(UMIN+UMAX)
        VMID=.5*(VMIN+VMAX)
        WMID=.5*(WMIN+WMAX)
C
C Set the combined character size multiplier.  Note that all character
C widths are stated as fractions of the smallest dimension of the box.
C
        CSM2=CSM1*MIN(UMAX-UMIN,VMAX-VMIN,WMAX-WMIN)
C
C Left-handed and right-handed systems need to be treated differently.
C In each case, what we do depends on where the viewpoint (the point
C midway between the eyes in the case of a stereo view) is relative to
C the center of the box.
C
        IF (IH.EQ.0) THEN
          IF      (XM.GT.UMID.AND.YM.GT.VMID.AND.ZM.GT.WMID) THEN
            CALL TDPARA (UMAX,VMIN,WMIN,0.,+1.,0.,-1.,0.,0.)
            IF (IPGZ) CALL TDLBLA (3,VILB,VNLB,VMIN,VMAX,UMAX,UMIN,  0.)
            IF (IPLZ) CALL TDLBLA (2,UILB,UNLB,VMIN,VMAX,UMAX,UMIN, 90.)
            CALL TDPARA (UMIN,VMIN,WMIN,0.,+1.,0.,0.,0.,+1.)
            IF (IPGZ) CALL TDLBLA (2,WILB,WNLB,VMIN,VMAX,WMIN,WMAX, 90.)
            IF (IPLZ) CALL TDLBLA (4,VILB,VNLB,VMIN,VMAX,WMIN,WMAX,  0.)
            CALL TDPARA (UMAX,VMIN,WMIN,-1.,0.,0.,0.,0.,+1.)
            IF (IPGZ) CALL TDLBLA (4,UILB,UNLB,UMAX,UMIN,WMIN,WMAX,  0.)
            IF (IPLZ) CALL TDLBLA (1,WILB,WNLB,UMAX,UMIN,WMIN,WMAX,-90.)
          ELSE IF (XM.LE.UMID.AND.YM.GT.VMID.AND.ZM.GT.WMID) THEN
            CALL TDPARA (UMAX,VMAX,WMIN,-1.,0.,0.,0.,-1.,0.)
            IF (IPGZ) CALL TDLBLA (3,UILB,UNLB,UMAX,UMIN,VMAX,VMIN,  0.)
            IF (IPLZ) CALL TDLBLA (2,VILB,VNLB,UMAX,UMIN,VMAX,VMIN, 90.)
            CALL TDPARA (UMAX,VMIN,WMIN,-1.,0.,0.,0.,0.,+1.)
            IF (IPGZ) CALL TDLBLA (2,WILB,WNLB,UMAX,UMIN,WMIN,WMAX, 90.)
            IF (IPLZ) CALL TDLBLA (4,UILB,UNLB,UMAX,UMIN,WMIN,WMAX,  0.)
            CALL TDPARA (UMAX,VMAX,WMIN,0.,-1.,0.,0.,0.,+1.)
            IF (IPGZ) CALL TDLBLA (4,VILB,VNLB,VMAX,VMIN,WMIN,WMAX,  0.)
            IF (IPLZ) CALL TDLBLA (1,WILB,WNLB,VMAX,VMIN,WMIN,WMAX,-90.)
          ELSE IF (XM.GT.UMID.AND.YM.LE.VMID.AND.ZM.GT.WMID) THEN
            CALL TDPARA (UMIN,VMIN,WMIN,+1.,0.,0.,0.,+1.,0.)
            IF (IPGZ) CALL TDLBLA (3,UILB,UNLB,UMIN,UMAX,VMIN,VMAX,  0.)
            IF (IPLZ) CALL TDLBLA (2,VILB,VNLB,UMIN,UMAX,VMIN,VMAX, 90.)
            CALL TDPARA (UMIN,VMAX,WMIN,+1.,0.,0.,0.,0.,+1.)
            IF (IPGZ) CALL TDLBLA (2,WILB,WNLB,UMIN,UMAX,WMIN,WMAX, 90.)
            IF (IPLZ) CALL TDLBLA (4,UILB,UNLB,UMIN,UMAX,WMIN,WMAX,  0.)
            CALL TDPARA (UMIN,VMIN,WMIN,0.,+1.,0.,0.,0.,+1.)
            IF (IPGZ) CALL TDLBLA (4,VILB,VNLB,VMIN,VMAX,WMIN,WMAX,  0.)
            IF (IPLZ) CALL TDLBLA (1,WILB,WNLB,VMIN,VMAX,WMIN,WMAX,-90.)
          ELSE IF (XM.LE.UMID.AND.YM.LE.VMID.AND.ZM.GT.WMID) THEN
            CALL TDPARA (UMIN,VMAX,WMIN,0.,-1.,0.,+1.,0.,0.)
            IF (IPGZ) CALL TDLBLA (3,VILB,VNLB,VMAX,VMIN,UMIN,UMAX,  0.)
            IF (IPLZ) CALL TDLBLA (2,UILB,UNLB,VMAX,VMIN,UMIN,UMAX, 90.)
            CALL TDPARA (UMAX,VMAX,WMIN,0.,-1.,0.,0.,0.,+1.)
            IF (IPGZ) CALL TDLBLA (2,WILB,WNLB,VMAX,VMIN,WMIN,WMAX, 90.)
            IF (IPLZ) CALL TDLBLA (4,VILB,VNLB,VMAX,VMIN,WMIN,WMAX,  0.)
            CALL TDPARA (UMIN,VMAX,WMIN,+1.,0.,0.,0.,0.,+1.)
            IF (IPGZ) CALL TDLBLA (4,UILB,UNLB,UMIN,UMAX,WMIN,WMAX,  0.)
            IF (IPLZ) CALL TDLBLA (1,WILB,WNLB,UMIN,UMAX,WMIN,WMAX,-90.)
          ELSE IF (XM.GT.UMID.AND.YM.GT.VMID.AND.ZM.LE.WMID) THEN
            CALL TDPARA (UMIN,VMIN,WMAX,0.,+1.,0.,+1.,0.,0.)
            IF (IPLZ) CALL TDLBLA (2,UILB,UNLB,VMIN,VMAX,UMIN,UMAX,-90.)
            IF (IPGZ) CALL TDLBLA (4,VILB,VNLB,VMIN,VMAX,UMIN,UMAX,  0.)
            CALL TDPARA (UMAX,VMIN,WMIN,-1.,0.,0.,0.,0.,+1.)
            IF (IPLZ) CALL TDLBLA (1,WILB,WNLB,UMAX,UMIN,WMIN,WMAX, 90.)
            IF (IPGZ) CALL TDLBLA (3,UILB,UNLB,UMAX,UMIN,WMIN,WMAX,  0.)
            CALL TDPARA (UMIN,VMIN,WMIN,0.,+1.,0.,0.,0.,+1.)
            IF (IPLZ) CALL TDLBLA (3,VILB,VNLB,VMIN,VMAX,WMIN,WMAX,  0.)
            IF (IPGZ) CALL TDLBLA (2,WILB,WNLB,VMIN,VMAX,WMIN,WMAX,-90.)
          ELSE IF (XM.LE.UMID.AND.YM.GT.VMID.AND.ZM.LE.WMID) THEN
            CALL TDPARA (UMAX,VMIN,WMAX,-1.,0.,0.,0.,+1.,0.)
            IF (IPLZ) CALL TDLBLA (2,VILB,VNLB,UMAX,UMIN,VMIN,VMAX,-90.)
            IF (IPGZ) CALL TDLBLA (4,UILB,UNLB,UMAX,UMIN,VMIN,VMAX,  0.)
            CALL TDPARA (UMAX,VMAX,WMIN,0.,-1.,0.,0.,0.,+1.)
            IF (IPLZ) CALL TDLBLA (1,WILB,WNLB,VMAX,VMIN,WMIN,WMAX, 90.)
            IF (IPGZ) CALL TDLBLA (3,VILB,VNLB,VMAX,VMIN,WMIN,WMAX,  0.)
            CALL TDPARA (UMAX,VMIN,WMIN,-1.,0.,0.,0.,0.,+1.)
            IF (IPLZ) CALL TDLBLA (3,UILB,UNLB,UMAX,UMIN,WMIN,WMAX,  0.)
            IF (IPGZ) CALL TDLBLA (2,WILB,WNLB,UMAX,UMIN,WMIN,WMAX,-90.)
          ELSE IF (XM.GT.UMID.AND.YM.LE.VMID.AND.ZM.LE.WMID) THEN
            CALL TDPARA (UMIN,VMAX,WMAX,+1.,0.,0.,0.,-1.,0.)
            IF (IPLZ) CALL TDLBLA (2,VILB,VNLB,UMIN,UMAX,VMAX,VMIN,-90.)
            IF (IPGZ) CALL TDLBLA (4,UILB,UNLB,UMIN,UMAX,VMAX,VMIN,  0.)
            CALL TDPARA (UMIN,VMIN,WMIN,0.,+1.,0.,0.,0.,+1.)
            IF (IPLZ) CALL TDLBLA (1,WILB,WNLB,VMIN,VMAX,WMIN,WMAX, 90.)
            IF (IPGZ) CALL TDLBLA (3,VILB,VNLB,VMIN,VMAX,WMIN,WMAX,  0.)
            CALL TDPARA (UMIN,VMAX,WMIN,+1.,0.,0.,0.,0.,+1.)
            IF (IPLZ) CALL TDLBLA (3,UILB,UNLB,UMIN,UMAX,WMIN,WMAX,  0.)
            IF (IPGZ) CALL TDLBLA (2,WILB,WNLB,UMIN,UMAX,WMIN,WMAX,-90.)
          ELSE IF (XM.LE.UMID.AND.YM.LE.VMID.AND.ZM.LE.WMID) THEN
            CALL TDPARA (UMAX,VMAX,WMAX,0.,-1.,0.,-1.,0.,0.)
            IF (IPLZ) CALL TDLBLA (2,UILB,UNLB,VMAX,VMIN,UMAX,UMIN,-90.)
            IF (IPGZ) CALL TDLBLA (4,VILB,VNLB,VMAX,VMIN,UMAX,UMIN,  0.)
            CALL TDPARA (UMIN,VMAX,WMIN,+1.,0.,0.,0.,0.,+1.)
            IF (IPLZ) CALL TDLBLA (1,WILB,WNLB,UMIN,UMAX,WMIN,WMAX, 90.)
            IF (IPGZ) CALL TDLBLA (3,UILB,UNLB,UMIN,UMAX,WMIN,WMAX,  0.)
            CALL TDPARA (UMAX,VMAX,WMIN,0.,-1.,0.,0.,0.,+1.)
            IF (IPLZ) CALL TDLBLA (3,VILB,VNLB,VMAX,VMIN,WMIN,WMAX,  0.)
            IF (IPGZ) CALL TDLBLA (2,WILB,WNLB,VMAX,VMIN,WMIN,WMAX,-90.)
          END IF
        ELSE
          IF      (XM.GT.UMID.AND.YM.GT.VMID.AND.ZM.GT.WMID) THEN
            CALL TDPARA (UMAX,VMIN,WMIN,-1.,0.,0.,0.,+1.,0.)
            IF (IPLZ) CALL TDLBLA (1,VILB,VNLB,UMAX,UMIN,VMIN,VMAX,-90.)
            IF (IPGZ) CALL TDLBLA (4,UILB,UNLB,UMAX,UMIN,VMIN,VMAX,180.)
            CALL TDPARA (UMIN,VMIN,WMIN,0.,0.,+1.,0.,+1.,0.)
            IF (IPLZ) CALL TDLBLA (4,WILB,WNLB,WMIN,WMAX,VMIN,VMAX,180.)
            IF (IPGZ) CALL TDLBLA (2,VILB,VNLB,WMIN,WMAX,VMIN,VMAX,-90.)
            CALL TDPARA (UMAX,VMIN,WMIN,0.,0.,+1.,-1.,0.,0.)
            IF (IPLZ) CALL TDLBLA (2,UILB,UNLB,WMIN,WMAX,UMAX,UMIN,-90.)
            IF (IPGZ) CALL TDLBLA (3,WILB,WNLB,WMIN,WMAX,UMAX,UMIN,  0.)
          ELSE IF (XM.LE.UMID.AND.YM.GT.VMID.AND.ZM.GT.WMID) THEN
            CALL TDPARA (UMAX,VMAX,WMIN,0.,-1.,0.,-1.,0.,0.)
            IF (IPLZ) CALL TDLBLA (1,UILB,UNLB,VMAX,VMIN,UMAX,UMIN,-90.)
            IF (IPGZ) CALL TDLBLA (4,VILB,VNLB,VMAX,VMIN,UMAX,UMIN,180.)
            CALL TDPARA (UMAX,VMIN,WMIN,0.,0.,+1.,-1.,0.,0.)
            IF (IPLZ) CALL TDLBLA (4,WILB,WNLB,WMIN,WMAX,UMAX,UMIN,180.)
            IF (IPGZ) CALL TDLBLA (2,UILB,UNLB,WMIN,WMAX,UMAX,UMIN,-90.)
            CALL TDPARA (UMAX,VMAX,WMIN,0.,0.,+1.,0.,-1.,0.)
            IF (IPLZ) CALL TDLBLA (2,VILB,VNLB,WMIN,WMAX,VMAX,VMIN,-90.)
            IF (IPGZ) CALL TDLBLA (3,WILB,WNLB,WMIN,WMAX,VMAX,VMIN,  0.)
          ELSE IF (XM.GT.UMID.AND.YM.LE.VMID.AND.ZM.GT.WMID) THEN
            CALL TDPARA (UMIN,VMIN,WMIN,0.,+1.,0.,+1.,0.,0.)
            IF (IPLZ) CALL TDLBLA (1,UILB,UNLB,VMIN,VMAX,UMIN,UMAX,-90.)
            IF (IPGZ) CALL TDLBLA (4,VILB,VNLB,VMIN,VMAX,UMIN,UMAX,180.)
            CALL TDPARA (UMIN,VMAX,WMIN,0.,0.,+1.,+1.,0.,0.)
            IF (IPLZ) CALL TDLBLA (4,WILB,WNLB,WMIN,WMAX,UMIN,UMAX,180.)
            IF (IPGZ) CALL TDLBLA (2,UILB,UNLB,WMIN,WMAX,UMIN,UMAX,-90.)
            CALL TDPARA (UMIN,VMIN,WMIN,0.,0.,+1.,0.,+1.,0.)
            IF (IPLZ) CALL TDLBLA (2,VILB,VNLB,WMIN,WMAX,VMIN,VMAX,-90.)
            IF (IPGZ) CALL TDLBLA (3,WILB,WNLB,WMIN,WMAX,VMIN,VMAX,  0.)
          ELSE IF (XM.LE.UMID.AND.YM.LE.VMID.AND.ZM.GT.WMID) THEN
            CALL TDPARA (UMIN,VMAX,WMIN,+1.,0.,0.,0.,-1.,0.)
            IF (IPLZ) CALL TDLBLA (1,VILB,VNLB,UMIN,UMAX,VMAX,VMIN,-90.)
            IF (IPGZ) CALL TDLBLA (4,UILB,UNLB,UMIN,UMAX,VMAX,VMIN,180.)
            CALL TDPARA (UMAX,VMAX,WMIN,0.,0.,+1.,0.,-1.,0.)
            IF (IPLZ) CALL TDLBLA (4,WILB,WNLB,WMIN,WMAX,VMAX,VMIN,180.)
            IF (IPGZ) CALL TDLBLA (2,VILB,VNLB,WMIN,WMAX,VMAX,VMIN,-90.)
            CALL TDPARA (UMIN,VMAX,WMIN,0.,0.,+1.,+1.,0.,0.)
            IF (IPLZ) CALL TDLBLA (2,UILB,UNLB,WMIN,WMAX,UMIN,UMAX,-90.)
            IF (IPGZ) CALL TDLBLA (3,WILB,WNLB,WMIN,WMAX,UMIN,UMAX,  0.)
          ELSE IF (XM.GT.UMID.AND.YM.GT.VMID.AND.ZM.LE.WMID) THEN
            CALL TDPARA (UMIN,VMIN,WMAX,+1.,0.,0.,0.,+1.,0.)
            IF (IPGZ) CALL TDLBLA (4,UILB,UNLB,UMIN,UMAX,VMIN,VMAX,  0.)
            IF (IPLZ) CALL TDLBLA (2,VILB,VNLB,UMIN,UMAX,VMIN,VMAX,-90.)
            CALL TDPARA (UMAX,VMIN,WMIN,0.,0.,+1.,-1.,0.,0.)
            IF (IPGZ) CALL TDLBLA (3,WILB,WNLB,WMIN,WMAX,UMAX,UMIN,180.)
            IF (IPLZ) CALL TDLBLA (1,UILB,UNLB,WMIN,WMAX,UMAX,UMIN,-90.)
            CALL TDPARA (UMIN,VMIN,WMIN,0.,0.,+1.,0.,+1.,0.)
            IF (IPGZ) CALL TDLBLA (1,VILB,VNLB,WMIN,WMAX,VMIN,VMAX,-90.)
            IF (IPLZ) CALL TDLBLA (4,WILB,WNLB,WMIN,WMAX,VMIN,VMAX,  0.)
          ELSE IF (XM.LE.UMID.AND.YM.GT.VMID.AND.ZM.LE.WMID) THEN
            CALL TDPARA (UMAX,VMIN,WMAX,0.,+1.,0.,-1.,0.,0.)
            IF (IPGZ) CALL TDLBLA (4,VILB,VNLB,VMIN,VMAX,UMAX,UMIN,  0.)
            IF (IPLZ) CALL TDLBLA (2,UILB,UNLB,VMIN,VMAX,UMAX,UMIN,-90.)
            CALL TDPARA (UMAX,VMAX,WMIN,0.,0.,+1.,0.,-1.,0.)
            IF (IPGZ) CALL TDLBLA (3,WILB,WNLB,WMIN,WMAX,VMAX,VMIN,180.)
            IF (IPLZ) CALL TDLBLA (1,VILB,VNLB,WMIN,WMAX,VMAX,VMIN,-90.)
            CALL TDPARA (UMAX,VMIN,WMIN,0.,0.,+1.,-1.,0.,0.)
            IF (IPGZ) CALL TDLBLA (1,UILB,UNLB,WMIN,WMAX,UMAX,UMIN,-90.)
            IF (IPLZ) CALL TDLBLA (4,WILB,WNLB,WMIN,WMAX,UMAX,UMIN,  0.)
          ELSE IF (XM.GT.UMID.AND.YM.LE.VMID.AND.ZM.LE.WMID) THEN
            CALL TDPARA (UMIN,VMAX,WMAX,0.,-1.,0.,+1.,0.,0.)
            IF (IPGZ) CALL TDLBLA (4,VILB,VNLB,VMAX,VMIN,UMIN,UMAX,  0.)
            IF (IPLZ) CALL TDLBLA (2,UILB,UNLB,VMAX,VMIN,UMIN,UMAX,-90.)
            CALL TDPARA (UMIN,VMIN,WMIN,0.,0.,+1.,0.,+1.,0.)
            IF (IPGZ) CALL TDLBLA (3,WILB,WNLB,WMIN,WMAX,VMIN,VMAX,180.)
            IF (IPLZ) CALL TDLBLA (1,VILB,VNLB,WMIN,WMAX,VMIN,VMAX,-90.)
            CALL TDPARA (UMIN,VMAX,WMIN,0.,0.,+1.,+1.,0.,0.)
            IF (IPGZ) CALL TDLBLA (1,UILB,UNLB,WMIN,WMAX,UMIN,UMAX,-90.)
            IF (IPLZ) CALL TDLBLA (4,WILB,WNLB,WMIN,WMAX,UMIN,UMAX,  0.)
          ELSE IF (XM.LE.UMID.AND.YM.LE.VMID.AND.ZM.LE.WMID) THEN
            CALL TDPARA (UMAX,VMAX,WMAX,-1.,0.,0.,0.,-1.,0.)
            IF (IPGZ) CALL TDLBLA (4,UILB,UNLB,UMAX,UMIN,VMAX,VMIN,  0.)
            IF (IPLZ) CALL TDLBLA (2,VILB,VNLB,UMAX,UMIN,VMAX,VMIN,-90.)
            CALL TDPARA (UMIN,VMAX,WMIN,0.,0.,+1.,+1.,0.,0.)
            IF (IPGZ) CALL TDLBLA (3,WILB,WNLB,WMIN,WMAX,UMIN,UMAX,180.)
            IF (IPLZ) CALL TDLBLA (1,UILB,UNLB,WMIN,WMAX,UMIN,UMAX,-90.)
            CALL TDPARA (UMAX,VMAX,WMIN,0.,0.,+1.,0.,-1.,0.)
            IF (IPGZ) CALL TDLBLA (1,VILB,VNLB,WMIN,WMAX,VMAX,VMIN,-90.)
            IF (IPLZ) CALL TDLBLA (4,WILB,WNLB,WMIN,WMAX,VMAX,VMIN,  0.)
          END IF
        END IF
C
C Done.
C
        RETURN
C
      END
