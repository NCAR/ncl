C
C $Id: g01wsl.h,v 1.2 2000-07-12 16:50:48 haley Exp $
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

      COMMON  /G01WSL/  MWKID   ,MCONID ,MWTYPE ,MSTATE ,MOPEN  ,
     +                  MDEFMO  ,MREGMO ,MDEMPT ,MNFRAM ,MTUS   ,
     +                  RWINDO(4)       ,CWINDO(4)      ,
     +                  RWKVP (4)       ,CWKVP (4)      ,
     +                  MOLMAX  ,MOL    ,MCOVFL ,MCSORT ,MCOLI(256),
     +                  SRED(256)       ,SGREEN(256)    ,SBLUE(256),
     +                  MRCREC(4)       ,MRCLIP
        INTEGER         MWKID   ,MCONID ,MWTYPE ,MSTATE ,MOPEN
        INTEGER         MDEFMO  ,MREGMO ,MDEMPT ,MNFRAM ,MTUS
        INTEGER         MOLMAX  ,MOL    ,MCOVFL ,MCSORT ,MCOLI
        REAL            RWINDO          ,CWINDO
        REAL            RWKVP           ,CWKVP
        REAL            SRED            ,SGREEN         ,SBLUE
        INTEGER         MRCREC  ,MRCLIP
