C
C $Id: gasetr.f,v 1.4 2000-07-12 16:24:12 haley Exp $
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
      SUBROUTINE GASETR (PNAM,RVAL)
C
        CHARACTER*(*) PNAM
C
C The subroutine GASETR may be used to set GRIDAL parameters which have
C values of type REAL.
C
C Declare the common block containing real and integer parameters.
C
        COMMON /GAREIN/ ICAX,ICLB,ICMJ,ICMN,ILTY,IORX,NCFX,NCFY,RCWX,
     +                  RCWY,RDCX,RDCY,RMJX,RMJY,RMNX,RMNY,RWAX,RWLB,
     +                  RWMJ,RWMN
        SAVE   /GAREIN/
C
C Declare the block data "routine" external.  This should force it to
C be loaded.
C
        EXTERNAL GABLDT
C
C Check for an uncleared prior error.
C
        IF (ICFELL('GASETR - UNCLEARED PRIOR ERROR',1).NE.0) RETURN
C
C Set the selected parameter.
C
        IF      (PNAM(1:3).EQ.'CAX'.OR.PNAM(1:3).EQ.'cax') THEN
          ICAX=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'CLB'.OR.PNAM(1:3).EQ.'clb') THEN
          ICLB=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'CMJ'.OR.PNAM(1:3).EQ.'cmj') THEN
          ICMJ=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'CMN'.OR.PNAM(1:3).EQ.'cmn') THEN
          ICMN=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'LTY'.OR.PNAM(1:3).EQ.'lty') THEN
          ILTY=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'WAX'.OR.PNAM(1:3).EQ.'wax') THEN
          RWAX=RVAL
        ELSE IF (PNAM(1:3).EQ.'WLB'.OR.PNAM(1:3).EQ.'wlb') THEN
          RWLB=RVAL
        ELSE IF (PNAM(1:3).EQ.'WMJ'.OR.PNAM(1:3).EQ.'wmj') THEN
          RWMJ=RVAL
        ELSE IF (PNAM(1:3).EQ.'WMN'.OR.PNAM(1:3).EQ.'wmn') THEN
          RWMN=RVAL
        ELSE IF (PNAM(1:3).EQ.'XLL'.OR.PNAM(1:3).EQ.'xll') THEN
          NCFX=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'XLO'.OR.PNAM(1:3).EQ.'xlo') THEN
          RDCY=RVAL
        ELSE IF (PNAM(1:3).EQ.'XLS'.OR.PNAM(1:3).EQ.'xls') THEN
          RCWX=RVAL
        ELSE IF (PNAM(1:3).EQ.'XMJ'.OR.PNAM(1:3).EQ.'xmj') THEN
          RMJX=RVAL
        ELSE IF (PNAM(1:3).EQ.'XMN'.OR.PNAM(1:3).EQ.'xmn') THEN
          RMNX=RVAL
        ELSE IF (PNAM(1:3).EQ.'XOR'.OR.PNAM(1:3).EQ.'xor') THEN
          IORX=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'YLL'.OR.PNAM(1:3).EQ.'yll') THEN
          NCFY=INT(RVAL)
        ELSE IF (PNAM(1:3).EQ.'YLO'.OR.PNAM(1:3).EQ.'ylo') THEN
          RDCX=RVAL
        ELSE IF (PNAM(1:3).EQ.'YLS'.OR.PNAM(1:3).EQ.'yls') THEN
          RCWY=RVAL
        ELSE IF (PNAM(1:3).EQ.'YMJ'.OR.PNAM(1:3).EQ.'ymj') THEN
          RMJY=RVAL
        ELSE IF (PNAM(1:3).EQ.'YMN'.OR.PNAM(1:3).EQ.'ymn') THEN
          RMNY=RVAL
        ELSE
          CALL SETER ('GASETR - UNRECOGNIZED PARAMETER NAME',2,1)
        END IF
C
C Done.
C
        RETURN
C
      END
