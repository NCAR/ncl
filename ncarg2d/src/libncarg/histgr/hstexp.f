C
C $Id: hstexp.f,v 1.6 2000-08-22 15:04:48 haley Exp $
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
C *************************************************************
C
        SUBROUTINE HSTEXP(HWIND,MAXW)
C
C *************************************************************
C
C  THE WINDOW IS EXPANDED AND THE NEW WORLD COORDINATES ARE
C  CALCULATED TO CORRESPOND TO THE NEW GIVEN VIEWPORT
C  THE ORIGINAL AXPECT RATIO (WORLD COORDS : VIEWPORT COORDS) REMAINS
C
        REAL MAXW(4), VIEW(4), WIND(4), HWIND(4), LEFT
C
C  INQUIRE CURRENT WINDOW AND VIEWPORT LIMITS
C
        CALL GQCNTN(IERR,ICNT)
        CALL GQNT(ICNT,IERR,WIND,VIEW)
C
C  CALCULATE RATIO OF Y WORLD / VIEWPORT COORDINATES
C
        YRATIO = (WIND(4) - WIND(3))/(VIEW(4) - VIEW(3))
C
C  CALCULATE RATIO OF X WORLD / VIEWPORT COORDINATES
C
        XRATIO = (WIND(2) - WIND(1))/(VIEW(2) - VIEW(1))
C
C  GET EXPANDED LOWER LIMIT Y COORDINATE
C
        VBOTTM = VIEW(3) - HWIND(3)
        BOTTOM = YRATIO * VBOTTM
        MAXW(3) = WIND(3) - BOTTOM
C
C  GET EXPANDED UPPER LIMIT Y COORDINATE
C
        VTOP = HWIND(4) - VIEW(4)
        TOP = YRATIO * VTOP
        MAXW(4) = WIND(4) + TOP
C
C  GET EXPANDED LEFT LIMIT X COORDINATE
C
        VLEFT = VIEW(1) - HWIND(1)
        LEFT = XRATIO * VLEFT
        MAXW(1) = WIND(1) - LEFT
C
C  GET EXPANDED RIGHT LIMIT X COORDINATE
C
        VRIGHT = HWIND(2) - VIEW(2)
        RIGHT = XRATIO * VRIGHT
        MAXW(2) = WIND(2) + RIGHT
C
C  SET NEW (EXPANDED) NORMALIZATION TRANSFORMATION
C
        CALL GSWN(1,MAXW(1),MAXW(2),MAXW(3),MAXW(4))
	CALL GSVP(1, HWIND(1), HWIND(2), HWIND(3), HWIND(4))
        CALL GSELNT(1)
C
C
        RETURN
        END
