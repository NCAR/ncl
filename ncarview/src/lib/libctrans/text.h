/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/

/*enumerated type values for the CGM element text alignment	*/
#define	A_NORM_H	0	/*normal horizontal	*/
#define	A_LEFT		1
#define	A_CENTER	2
#define	A_RIGHT		3
#define	A_CO_HOR	4	/*continous horizontal	*/

#define	A_NORM_V	0	/*normal vertical	*/
#define	A_TOP		1
#define	A_CAP		2
#define	A_HALF		3
#define	A_BASE		4
#define	A_BOTTOM	5
#define	A_CO_VER	6	/*continous vertical	*/


#define STR_ALC		256	/* intitial amount of mem alocated for strings*/

/* magnitude of a vector*/
#define	MAG(X1,Y1)	((float) (sqrt((double) (((X1) * (X1)) + ((Y1) * (Y1))))))

/*  text path	*/
#define	PATH_RIGHT	0	/* text path is right	*/
#define	PATH_LEFT	1	/* text path is left	*/
#define	PATH_UP		2	/* text path is up	*/
#define	PATH_DOWN	3	/* text path is down	*/

/*
 *	text precisions
 */
#define	STRING_P	0
#define	CHAR_P		1
#define	STROKE_P	2

/* macro for computing scaling factors in x and y directions	*/
#define Y_SCALE(HEIGHT)	(((float) CHAR_HEIGHT / (float) HEIGHT))
#define X_SCALE(HEIGHT) ((float) (((float) CHAR_HEIGHT / (float) HEIGHT)) * CHAR_EXPAN * (float) (MAG(CHAR_X_BASE,CHAR_Y_BASE) / MAG(CHAR_X_UP,CHAR_Y_UP)))


