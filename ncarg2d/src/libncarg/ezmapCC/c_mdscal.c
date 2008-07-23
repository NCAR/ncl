/*
 *      $Id: c_mdscal.c,v 1.2 2008-07-23 16:16:52 haley Exp $
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

extern double NGCALLF(mdscal,MDSCAL)(float*,float*,float*,float*);

double c_mdscal
#ifdef NeedFuncProto
(
    float xcop,
    float ycop,
    float xcoq,
    float ycoq
)
#else
(xcop,ycop,xcoq,ycoq)
    float xcop;
    float ycop;
    float xcoq;
    float ycoq
#endif
{
    return(NGCALLF(mdscal,MDSCAL)(&xcop,&ycop,&xcoq,&ycoq));
}
