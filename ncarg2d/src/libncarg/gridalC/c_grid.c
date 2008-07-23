/*
 *	$Id: c_grid.c,v 1.5 2008-07-23 16:16:55 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#include <ncarg/ncargC.h>

extern void NGCALLF(grid,GRID)(int*,int*,int*,int*);

void c_grid
#ifdef NeedFuncProto
(
    int mjrx,
    int mnrx,
    int mjry,
    int mnry
)
#else
(mjrx,mnrx,mjry,mnry)
    int mjrx;
    int mnrx;
    int mjry;
    int mnry;
#endif
{
    NGCALLF(grid,GRID)(&mjrx,&mnrx,&mjry,&mnry);
}
