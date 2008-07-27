C
C $Id: pcmpxy.f,v 1.12 2008-07-27 00:17:20 haley Exp $
C
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C The use of this Software is governed by a License Agreement.
C
      SUBROUTINE PCMPXY (IMAP,XINP,YINP,XOTP,YOTP)
C
C By default, PLCHHQ and PLCHMQ draw characters using X/Y coordinates
C in the fractional system and position the whole character string
C relative to the point (CUFX(XPOS),CUFY(YPOS)).  Setting the flag
C 'MAP' non-zero causes them instead to draw characters using X/Y
C coordinates in an arbitrary X/Y coordinate system and to position
C the whole character string relative to the point (XPOS,YPOS); just
C before drawing, PCMPXY is called to map these coordinates into the
C current user system.
C
C The user of PLOTCHAR may replace this routine as desired to transform
C the characters being drawn.  By default, 'MAP' = 1 selects the EZMAP
C transformations, 'MAP' = 2 selects a polar coordinate transformation,
C 'MAP' = 3 selects a perspective transformation, and 'MAP' = 4 selects
C a scheme appropriate for labelling points on the globe, as described
C in the next paragraph.
C
C When 'MAP' = 4, in all user calls to PLCHHQ and PLCHMQ the values
C of the arguments XPOS, YPOS, and ANGD should be zero.  The argument
C SIZE specifies the desired character width, as always, but it is
C interpreted as an angle, in degrees.  The characters of the string
C are initially thought of as being written along the equator of the
C globe and are positioned relative to the point with latitude 0 and
C longitude 0 as determined by the value of the argument CNTR.  The
C variables PANG, PLAT, and PLON, in the common block PCMP04, must have
C been set by the user to determine how the string is to be transformed
C on the surface of the globe.  Three rotations and a call to the EZMAP
C routine MAPTRA are used: the net effect is to move the string to the
C position (PLAT,PLON) in such a way that a vector in the writing
C direction of the string makes an angle of PANG with the local east
C vector.  The reason for doing all this is that it produces little
C distortion in the character string, as viewed on the surface of the
C globe.
C
C In this version of PCMPXY, 'MAP' = 100 selects the identity mapping,
C but a check is made to see whether the point (XINP,YINP) is outside
C the rectangle specified by the values of the variables XVPL, XVPR,
C YVPB, and YVPT, in the common block PCSTCM; if so, the value 1.E12
C is returned for both XOTP and YOTP.  This mapping is used by the
C package STITLE to clip scrolled titles at the edges of the viewport.
C In future versions of PCMPXY, this situation may change; users who
C grab a copy of the routine and modify it should beware of this.
C
C When IMAP is positive, XINP and YINP are the coordinates of a point
C to be mapped to the user coordinate system; the coordinates in the
C user system are to be returned in the variables XOTP and YOTP.  When
C IMAP is negated, the inverse mapping is requested:  (XINP,YINP) is a
C point in the current user coordinate system; (XOTP,YOTP) is returned
C and is the point which would be carried into (XINP,YINP) by the
C mapping numbered ABS(IMAP).
C
C A call with IMAP = 0 should have the form
C
C       CALL PCMPXY (0,REAL(IMAP),RFLG,DUM1,DUM2)
C
C and will return information in RFLG about the mapping numbered IMAP,
C as follows:
C
C   RFLG       forward mapping defined?     inverse mapping defined?
C   ----       ------------------------     ------------------------
C    0.                 no                            no
C    1.                yes                            no
C    2.                 no                           yes
C    3.                yes                           yes
C
C Declare the common block in which TDPACK has put the current window
C limits.
C
        COMMON /TDCOM1/ IH,IT,XM,YM,ZM,XO,YO,ZO,XT,YT,ZT,OE,XE,YE,ZE
        COMMON /TDCOM1/ A1,B1,C1,D1,E1,A2,B2,C2,D2,E2,A3,B3,C3,D3,E3
        COMMON /TDCOM1/ IS,FV,VL,VR,VB,VT,WL,WR,WB,WT
        SAVE   /TDCOM1/
C
C Declare the common block in which STITLE communicates to PLOTCHAR the
C coordinates of the viewport outside which scrolled titles are to be
C clipped.
C
        COMMON /PCSTCM/ XVPL,XVPR,YVPB,YVPT
        SAVE   /PCSTCM/
C
C Declare the common block in which, when IMAP =4, we may find the
C angle at which a point label is to be written on the globe and the
C latitude and longitude of the point being labelled.
C
        COMMON /PCMP04/ PANG,PLAT,PLON
        SAVE   /PCMP04/
C
C Define multiplicative constants to convert from degrees to radians
C and from radians to degrees.
C
        DATA DTOR / .017453292519943 /
        DATA RTOD / 57.2957795130823 /
C
C Sort out the different cases.  When IMAP = 0, PCMPXY is being asked
C what capabilities it has.  The IF tests distinguishing various values
C of XINP from one another make no difference here, since inverses are
C available for all mappings done by this version; it is included as a
C model for those who pick up the routine and modify it (possibly by
C the addition of a mapping for which the inverse is not available).
C
        IF (IMAP.EQ.0) THEN
C
          IF (INT(XINP).EQ.1) THEN
            YINP=3.
          ELSE IF (INT(XINP).EQ.2) THEN
            YINP=3.
          ELSE IF (INT(XINP).EQ.3) THEN
            YINP=3.
          ELSE IF (INT(XINP).EQ.4) THEN
            YINP=1.
          ELSE IF (INT(XINP).EQ.100) THEN
            YINP=3.
          ELSE
            YINP=3.
          END IF
C
C Handle the EZMAP case ...
C
        ELSE IF (ABS(IMAP).EQ.1) THEN
C
          IF (IMAP.GT.0) THEN
            CALL MAPTRA (YINP,XINP,XOTP,YOTP)
            IF (ICFELL('PCMPXY',1).NE.0) RETURN
          ELSE
            CALL MAPTRI (XINP,YINP,YOTP,XOTP)
            IF (ICFELL('PCMPXY',2).NE.0) RETURN
          END IF
C
C ... the polar coordinate case ...
C
        ELSE IF (ABS(IMAP).EQ.2) THEN
C
          IF (IMAP.GT.0) THEN
            XOTP=XINP*COS(DTOR*YINP)
            YOTP=XINP*SIN(DTOR*YINP)
          ELSE
            XOTP=SQRT(XINP*XINP+YINP*YINP)
            YOTP=RTOD*ATAN2(YINP,XINP)
          END IF
C
C ... the perspective case ...
C
        ELSE IF (ABS(IMAP).EQ.3) THEN
C
          IF (IMAP.GT.0) THEN
            CALL TDPRPA (XINP,YINP,XOTP,YOTP)
            IF (XOTP.LT.WL.OR.XOTP.GT.WR.OR.
     +          YOTP.LT.WB.OR.YOTP.GT.WT) THEN
              XOTP=1.E12
              YOTP=1.E12
            END IF
          ELSE
            IF (XINP.LT.WL.OR.XINP.GT.WR.OR.
     +          YINP.LT.WB.OR.YINP.LT.WT) THEN
              XOTP=1.E12
              YOTP=1.E12
            ELSE
              CALL TDPRPI (XINP,YINP,XOTP,YOTP)
            END IF
          END IF
C
C ... a special use of EZMAP which avoids distorting the characters ...
C
        ELSE IF (ABS(IMAP).EQ.4) THEN
C
          UCRD=COS(DTOR*YINP)*COS(DTOR*XINP)
          VCRD=COS(DTOR*YINP)*SIN(DTOR*XINP)
          WCRD=SIN(DTOR*YINP)
          CALL NGRITD (1, PANG,UCRD,VCRD,WCRD)
          CALL NGRITD (2,-PLAT,UCRD,VCRD,WCRD)
          CALL NGRITD (3, PLON,UCRD,VCRD,WCRD)
          RLAT=RTOD*ASIN(WCRD)
          RLON=RTOD*ATAN2(VCRD,UCRD)
          CALL MAPTRA (RLAT,RLON,XOTP,YOTP)
          IF (ICFELL('PCMPXY',3).NE.0) RETURN
C
C ... the special STITLE case ...
C
        ELSE IF (ABS(IMAP).EQ.100) THEN
C
          IF (XINP.LT.XVPL.OR.XINP.GT.XVPR.OR.
     +        YINP.LT.YVPB.OR.YINP.GT.YVPT) THEN
            XOTP=1.E12
            YOTP=1.E12
          ELSE
            XOTP=XINP
            YOTP=YINP
          END IF
C
C ... and the identity case.
C
        ELSE
C
          XOTP=XINP
          YOTP=YINP
C
        END IF
C
C Done.
C
        RETURN
C
      END
