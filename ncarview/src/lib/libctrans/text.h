/*
 *	$Id: text.h,v 1.5 2008-07-27 03:22:40 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/

#define STR_ALC		256	/* intitial amount of mem alocated for strings*/

/* magnitude of a vector*/
#define	MAG(X1,Y1)	((float) (sqrt((double) (((X1) * (X1)) + ((Y1) * (Y1))))))

/* macro for computing scaling factors in x and y directions	*/
#define Y_SCALE(HEIGHT)	(((float) CHAR_HEIGHT / (float) HEIGHT))
#define X_SCALE(HEIGHT) ((float) (((float) CHAR_HEIGHT / (float) HEIGHT)) * CHAR_EXPAN * (float) (MAG(CHAR_X_BASE,CHAR_Y_BASE) / MAG(CHAR_X_UP,CHAR_Y_UP)))


