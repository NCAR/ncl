C
C $Id: tdblda.f,v 1.3 1994-03-17 21:37:31 kennison Exp $
C
      BLOCK DATA TDBLDA
C
C This "routine" defines the default values of various quantities in
C common.
C
C The variables in the following common block define the mapping from
C 3-space to 2-space.
C
        COMMON /TDCOM1/ IH,IT,XM,YM,ZM,XO,YO,ZO,XT,YT,ZT,OE,XE,YE,ZE
        COMMON /TDCOM1/ A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3
        COMMON /TDCOM1/ IS,FV,VL,VR,VB,VT,WL,WR,WB,WT
        SAVE   /TDCOM1/
C
C A1, B1, C1, D1, and E1 are quantities computed by TDINIT, describing
C plane 1.
C
        DATA A1,B1,C1,D1,E1 /-.2797514,-.4662524,-.8392543,0.,-10.72381/
C
C A2, B2, C2, D2, and E2 are quantities computed by TDINIT, describing
C plane 2.
C
        DATA A2,B2,C2,D2,E2 /.8574929,-.5144957,0.,0.,0./
C
C A3, B3, C3, D3, and E3 are quantities computed by TDINIT, describing
C plane 3.
C
        DATA A3,B3,C3,D3,E3 /-0.4317928,-.7196546,.5437390,0.,0./
C
C FV is the desired field of view, in degrees, to be used when IS is
C non-zero.
C
        DATA FV / 20. /
C
C IH is the internal parameter 'HND', which says whether the 3D
C coordinate system is right-handed (0) or left-handed (1).
C
        DATA IH / 0 /
C
C IS is the internal parameter 'SET', which says whether a SET call is
C to be done by TDINIT or not.  Use the value 0 if no SET call is to be
C done, the value 1 otherwise
C
        DATA IS / 1 /
C
C IT is the internal parameter 'STY', which says whether stereo views
C are to be done using a single image plane which is perpendicular to
C the line connecting (XO,YO,ZO) to (XM,YM,ZM) or to (XE,YE,ZE).  For
C the former case, use 'STY' = 0; for the latter, use 'STY' = 1.
C
        DATA IT / 0 /
C
C OE is the offset to the current eye position.  Use a zero if only one
C eye is in use, a negative value for the left eye, and a positive value
C for the right eye.
C
        DATA OE / 0. /
C
C VL, VR, VB, and VT define the viewport to be used in a call to SET
C done by TDINIT.
C
        DATA VL,VR,VB,VT / .05,.95,.05,.95 /
C
C WL, WR, WB, WT define the window to be used in a call to SET done by
C TDINIT.
C
        DATA WL,WR,WB,WT / -1.890896,1.890896,-1.890896,1.890896/
C
C (XE,YE,ZE) is the position of the eye from which a view is currently
C being drawn.
C
        DATA XE,YE,ZE / 3.,5.,9. /
C
C (XM,YM,ZM) is the midpoint of the line joining the left eye to the
C right eye.
C
        DATA XM,YM,ZM / 3.,5.,9. /
C
C (XO,YO,ZO) is the point at the origin of the image plane.
C
        DATA XO,YO,ZO / 0.,0.,0. /
C
C (XT,YT,ZT) is the third point required to define the plane of
C bilateral symmetry of the observer, a plane perpendicular to the
C line from the left eye to the right eye.
C
        DATA XT,YT,ZT / 0.,0.,10. /
C
      END
