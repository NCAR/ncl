/*
 *	$Id: c_ardbpa.c,v 1.3 2000-07-31 20:10:38 haley Exp $
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

extern void NGCALLF(ardbpa,ARDBPA)(int*,int*,NGstring,int);

void c_ardbpa
#ifdef NeedFuncProto
(
    int *iam,
    int igi,
    char *lab
)
#else
(iam,igi,lab)
    int *iam;
    int igi;
    char *lab;
#endif
{
	NGstring lab2;
    int len;

    len = NGSTRLEN(lab);
	lab2 = NGCstrToFstr(lab,len);
    NGCALLF(ardbpa,ARDBPA)(iam,&igi,lab2,len);
}
