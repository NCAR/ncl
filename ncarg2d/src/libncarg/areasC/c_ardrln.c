/*
 *	$Id: c_ardrln.c,v 1.2 2000-07-12 16:21:51 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

void c_ardrln
#ifdef NeedFuncProto
(
    int *iam,
    float *xcd,
    float *ycd,
    int ncd,
    float *xcs,
    float *ycs,
    int mcs,
    int *iai,
    int *iag,
    int mai,
    int (*colrln_)(
        float *xcs,
        float *ycs,
        int *ncs,
        int *iai,
        int *iag,
        int *nai
               )
)
#else
(iam,xcd,ycd,ncd,xcs,ycs,mcs,iai,iag,mai,colrln_)
    int *iam;
    float *xcd;
    float *ycd;
    int ncd;
    float *xcs;
    float *ycs;
    int mcs;
    int *iai;
    int *iag;
    int mai;
    int (*colrln_)();
#endif
{
    NGCALLF(ardrln,ARDRLN)(iam,xcd,ycd,&ncd,xcs,ycs,&mcs,iai,iag,&mai,colrln_);
}
