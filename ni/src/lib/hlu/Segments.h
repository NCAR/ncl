
/*
 *      $Id: Segments.h,v 1.2 1998-02-18 01:23:50 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Segments.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 1 10:02:49 MDT 1992
 *
 *	Description:
 */

#ifndef _NSegments_h
#define _NSegments_h


typedef struct _NhlTransDat{ 
	int id;
        int indx[3]; 
        float a[3][3]; 
        int d;
        float xmin,xmax,ymin,ymax;
} NhlTransDat;



#endif /*_NSegments_h*/ 


