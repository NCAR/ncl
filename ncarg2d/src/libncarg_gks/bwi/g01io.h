C
C $Id: g01io.h,v 1.4 2000-08-22 03:45:40 haley Exp $
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

      COMMON  /G01IO/   MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY(256)     ,
     +                  MOBFSZ  ,MOUTBF(720)    ,MBFPOS ,MFGLUN ,
     +                  MXBITS  ,MDTYPE ,MNFFLG ,MBMFLG ,MEMFLG
      INTEGER           MIOFLG  ,MRECNM ,MPXYSZ ,MPXPY  ,MOBFSZ ,
     +                  MBFPOS  ,MFGLUN ,MOUTBF ,MXBITS ,MDTYPE ,
     +                  MNFFLG  ,MBMFLG ,MEMFLG
      COMMON  /G01CHA/  MPNAME
      CHARACTER*80      MPNAME
