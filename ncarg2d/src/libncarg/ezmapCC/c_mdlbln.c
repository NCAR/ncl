/*
 *      $Id: c_mdlbln.c,v 1.2 2008-07-23 16:16:49 haley Exp $
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

extern void NGCALLF(mdlbln,MDLBLN)(float*,float*,float*,float*,float*,float*,
				   float*,float*,float*);

void c_mdlbln
#ifdef NeedFuncProto
(
    float xcop,
    float ycop,
    float xcoq,
    float ycoq,
    float offx,
    float offy,
    float size,
    float angl,
    float cent
)
#else
(xcop,ycop,xcoq,ycoq,offx,offy,size,angl,cent)
    float xcop;
    float ycop;
    float xcoq;
    float ycoq;
    float offx;
    float offy;
    float size;
    float angl;
    float cent;
#endif
{
      NGCALLF(mdlbln,MDLBLN)(&xcop,&ycop,&xcoq,&ycoq,&offx,&offy,
			     &size,&angl,&cent);
}
