C
C $Id: gksin.h,v 1.4 2003-10-08 22:39:52 fred Exp $
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
      COMMON/GKSIN1/ FCODE , CONT  ,
     +               IL1   , IL2   , ID(128)       ,
     +               IC1   , IC2   , IC(128)       ,
     +               RL1   , RL2   , RX(128)       , RY(128)       ,
     +               STRL1 , STRL2 , RERR
      COMMON/GKSIN2/ STR
      INTEGER        FCODE , CONT  , IL1   , IL2   , ID    , IC1   ,
     +               IC2   , IC    , RL1   , RL2   , STRL1 , STRL2 ,
     +               RERR
      CHARACTER*160  STR
      REAL           RX    , RY
