C
C $Id: pcmpxy.f,v 1.4 1993-01-13 23:07:41 kennison Exp $
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
C By default, 'MAP' = 1 selects the EZMAP transformations, 'MAP' = 2
C selects the polar coordinate transformations, and 'MAP' = 3 selects
C a perspective transformation.  The user of PLOTCHAR may replace this
C routine as desired to transform the characters being drawn.
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
C A call of the form
C
C       CALL PCMPXY (0,REAL(IMAP),RFLG,DUM1,DUM2)
C
C will return information in RFLG about the mapping numbered IMAP, as
C follows:
C
C   RFLG       forward mapping defined?     inverse mapping defined?
C   ----       ------------------------     ------------------------
C    0.                 no                            no
C    1.                yes                            no
C    2.                 no                           yes
C    3.                yes                           yes
C
C
C Declare the common block in which STITLE communicates to PLOTCHAR the
C coordinates of the viewport outside which scrolled titles are to be
C clipped.
C
        COMMON /PCSTCM/ XVPL,XVPR,YVPB,YVPT
        SAVE   /PCSTCM/
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
          ELSE
            CALL MAPTRI (XINP,YINP,YOTP,XOTP)
          END IF
C
C ... the polar coordinate case ...
C
        ELSE IF (ABS(IMAP).EQ.2) THEN
C
          IF (IMAP.GT.0) THEN
            XOTP=XINP*COS(.017453292519943*YINP)
            YOTP=XINP*SIN(.017453292519943*YINP)
          ELSE
            XOTP=SQRT(XINP*XINP+YINP*YINP)
            YOTP=57.2957795130823*ATAN2(YINP,XINP)
          END IF
C
C ... the layered case ...
C
        ELSE IF (ABS(IMAP).EQ.3) THEN
C
          IF (IMAP.GT.0) THEN
            CALL TDPRPA (XINP,YINP,XOTP,YOTP)
          ELSE
            CALL TDPRPI (XINP,YINP,XOTP,YOTP)
          END IF
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
