C
C	$Id: setr.f,v 1.2 2000-07-12 16:25:52 haley Exp $
C                                                                      
C                Copyright (C)  2000
C        University Corporation for Atmospheric Research
C                All Rights Reserved
C
C This file is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as
C published by the Free Software Foundation; either version 2.1 of the
C License, or (at your option) any later version.
C
C This software is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of
C MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
C Lesser General Public License for more details.
C
C You should have received a copy of the GNU Lesser General Public
C License along with this software; if not, write to the Free Software
C Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
C USA.
C
      SUBROUTINE SETR (XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,R0)
C
C THIS ROUTINE ESTABLISHES CERTAIN CONSTANTS SO THAT SRFACE
C PRODUCES A PICTURE WHOSE SIZE CHANGES WITH RESPECT TO THE
C VIEWERS DISTANCE FROM THE OBJECT.  IT CAN ALSO BE USED
C WHEN MAKING A MOVIE OF AN OBJECT EVOLVING IN TIME TO KEEP
C IT POSITIONED PROPERLY ON THE SCREEN, SAVING COMPUTER TIME
C IN THE BARGIN.  CALL IT WITH R0 NEGATIVE TO TURN OFF THIS
C FEATURE.
C PARAMETERS
C XMIN,XMAX - RANGE OF X ARRAY THAT WILL BE PASSED TO SRFACE.
C YMIN,YMAX - SAME IDEA, BUT FOR Y.
C ZMIN,ZMAX - SAME IDEA, BUT FOR Z.  IF A MOVIE IS BEING
C             MADE OF AN EVOLVING Z ARRAY, ZMIN AND ZMAX
C             SHOULD CONTAIN RANGE OF THE UNION OF ALL THE Z
C             ARRAYS.  THEY NEED NOT BE EXACT.
C R0        - DISTANCE BETWEEN OBSERVER AND POINT LOOKED AT
C             WHEN THE PICTURE IS TO FILL THE SCREEN WHEN
C             VIEWED FROM THE DIRECTION WHICH MAKES THE PIC-
C             TURE BIGGEST.  IF R0 IS NOT POSITIVE, THEN THE
C             RELATIVE SIZE FEATURE IS TURNED OFF, AND SUB-
C             SEQUENT PICTURES WILL FILL THE SCREEN.
C
      COMMON /SRFBLK/ LIMU(1024) ,LIML(1024) ,CL(41)     ,NCL        ,
     1                LL         ,FACT       ,IROT       ,NDRZ       ,
     2                NUPPER     ,NRSWT      ,BIGD       ,UMIN       ,
     3                UMAX       ,VMIN       ,VMAX       ,RZERO      ,
     4                IOFFP      ,NSPVAL     ,SPVAL      ,BIGEST
      COMMON /PWRZ1S/ XXMIN      ,XXMAX      ,YYMIN      ,YYMAX      ,
     1                ZZMIN      ,ZZMAX      ,DELCRT     ,EYEX       ,
     2                EYEY       ,EYEZ
C
C
      CALL Q8QST4 ('GRAPHX','SRFACE','SETR','VERSION 01')
      IF (R0)  10, 10, 20
   10 NRSWT = 0
      RETURN
   20 NRSWT = 1
      XXMIN = XMIN
      XXMAX = XMAX
      YYMIN = YMIN
      YYMAX = YMAX
      ZZMIN = ZMIN
      ZZMAX = ZMAX
      RZERO = R0
      LL = 0
      XAT = (XXMAX+XXMIN)*.5
      YAT = (YYMAX+YYMIN)*.5
      ZAT = (ZZMAX+ZZMIN)*.5
      ALPHA = -(YYMIN-YAT)/(XXMIN-XAT)
      YEYE = -RZERO/SQRT(1.+ALPHA*ALPHA)
      XEYE = YEYE*ALPHA
      YEYE = YEYE+YAT
      XEYE = XEYE+XAT
      ZEYE = ZAT
      CALL TRN32S (XAT,YAT,ZAT,XEYE,YEYE,ZEYE,0)
      XMN = XXMIN
      XMX = XXMAX
      YMN = YYMIN
      YMX = YYMAX
      ZMN = ZZMIN
      ZMX = ZZMAX
      CALL TRN32S (XMN,YMN,ZAT,UMN,DUMMY,DUMMIE,1)
      CALL TRN32S (XMX,YMN,ZMN,DUMMY,VMN,DUMMIE,1)
      CALL TRN32S (XMX,YMX,ZAT,UMX,DUMMY,DUMMIE,1)
      CALL TRN32S (XMX,YMN,ZMX,DUMMY,VMX,DUMMIE,1)
      UMIN = UMN
      UMAX = UMX
      VMIN = VMN
      VMAX = VMX
      BIGD = SQRT((XXMAX-XXMIN)**2+(YYMAX-YYMIN)**2+(ZZMAX-ZZMIN)**2)*.5
      RETURN
      END
