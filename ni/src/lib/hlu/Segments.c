/*
 *      $Id: Segments.c,v 1.9 2003-04-04 18:33:48 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Segments.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 1 08:39:05 MDT 1992
 *
 *	Description:	This file is used to create, compute, reset and 
 *		destroy transformation matices to be used as segment 
 *		transformations. There are currently four functions:
 *			void		_NhlDestroySegTransDat
 *			void		_NhlResetSegTransDat
 *			NhlTransDat	_NhlInitSegTransDat
 *			void		_NhlComputeSegTransMat
 *			void		_NhlStartSegment
 *			void		_NhlSetSegTrans
 *			void		_NhlEndSegment
 *			void 		_NhlEvalTrans
 *			int		_NhlDrawSegment
 *				
 *
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/Segments.h>


void ludcmp3d();
void lubksb3d();


/*
* At first this id will be used as the segment name for NCAR GKS. If segments
* are routinly created and freed some mechanism for reclaiming spent id may
* be needed
*/
/*
 * GKS only allows segment numbers from 1-99; it is now easily possible
 * to go way beyond that with the HLU's so a method of retrieving 
 * discarded segment ids is necessary
 */
#define SEG_NOT_SET NgNOT_A_SEGMENT
#define MAX_SEG 100
static NhlBoolean Init_Required = True;
static char Id_Assigned[MAX_SEG];
static int Id = 1;



/*
Input data must be ordered:

p1-------------p2
|
|
|
|
|
p0

p0 = (x[0],y[0])
p1 = (x[1],y[1])
p2 = (x[2],y[2])
*/

/*
 * Function:	_NhlDestroySegTransDat
 *
 * Description: Free's the NhlTransDat data structure.
 *
 * In Args:	The instance of NhlTransDat to be freed.
 *
 * Out Args:	The instance is NULL.
 *
 * Return Values: NONE
 *
 * Side Effects: NONE
 */
void	_NhlDestroySegTransDat
#if	NhlNeedProto
(NhlTransDat	*transdat)
#else
(transdat) 
	NhlTransDat	*transdat;
#endif
{
	if(transdat->id != SEG_NOT_SET) {
/* FORTRAN */   _NHLCALLF(gdsg,GDSG)(&(transdat->id));
		Id_Assigned[transdat->id] = 0;
		if (transdat->id < Id)
			Id = transdat->id;
	}

	NhlFree(transdat);
	transdat = NULL;
	return;
}

/*
 * Function:	_NInitSegTransDat
 *
 * Description: Initializes information needed to compute segment 
 *	transformations.
 *
 * In Args:	x and y are arrays of points representing a bounding box. The
 * 	coordinate pairs must be ordered in the following fashion:
 *
 *Input data must be ordered:
 *
 *	p1-------------p2
 *	|
 *	|
 *	|
 *	|
 *	|
 *	p0
 *
 *	p0 = (x[0],y[0])
 *	p1 = (x[1],y[1])
 *	p2 = (x[2],y[2])
 *
 * Out Args: NONE
 *
 * Return Values: an instance of NhlTransDat is created and returned.
 *
 * Side Effects: The static variable id is incremented.
 */

NhlTransDat *_NhlInitSegTransDat
#if	NhlNeedProto
(float* x,float* y)
#else
(x,y)
	float *x,*y;
#endif
{
	int k,j;
	NhlTransDat *transdat;

	transdat = (NhlTransDat*)NhlMalloc((unsigned)sizeof(NhlTransDat));
	if(transdat != NULL ){

                transdat->xmin = x[0];
                transdat->xmax = x[2];
                transdat->ymin = y[0];
                transdat->ymax = y[2];

		transdat->a[0][0] = x[0];
		transdat->a[1][0] = y[0];
		transdat->a[2][0] = 1.0;
		transdat->a[0][1] = x[1];
		transdat->a[1][1] = y[1];
		transdat->a[2][1] = 1.0;
		transdat->a[0][2] = x[2];
		transdat->a[1][2] = y[2];
		transdat->a[2][2] = 1.0;
	
		transdat->indx[0] = 0;
		transdat->indx[1] = 0;
		transdat->indx[2] = 0;

		transdat->d = 0;
		transdat->id = SEG_NOT_SET;

		k = 3;
		j = 3;
/*
* Computes LU factorization routine located in util.a
*/

/* FORTRAN */ _NHLCALLF(sgefa,SGEFA)(transdat->a,&k,&j, transdat->indx, &(transdat->d)); 
	}
	return(transdat);
}

/*
 * Function:	_NhlResetSegTransDat
 *
 * Description: Does exactly what NhlInitSegTransDat does except uses an 
 * 	already allocated NhlTransDat instance and it does not increment the 
 *	static variable id.
 *
 * In Args: transdat is the NhlTransDat instance to be reset.
 *	    x and y are the arrays of points that function as the base for
 *	    any tranformations to be computed.
 *
 *Input data must be ordered:
 *
 *	p1-------------p2
 *	|
 *	|
 *	|
 *	|
 *	|
 *	p0
 *
 *	p0 = (x[0],y[0])
 *	p1 = (x[1],y[1])
 *	p2 = (x[2],y[2])
 *
 *
 * Out Args: transdat contains new segment information.
 *
 * Return Values: transdat is returned as the return value.
 *
 * Side Effects: No incrementing of id. If a create segment isn't done
 *	before drawing this segment again the segment transformation matrix
 *	will not be reset to identity.
 */
void	_NhlResetSegTransDat
#if	NhlNeedProto
(NhlTransDat *transdat, float *x, float *y)
#else
(transdat,x,y) 
	NhlTransDat	*transdat;
	float 	*x,*y;
#endif
{
	int k,j;

	if(transdat != NULL ){

                transdat->xmin = x[0];
                transdat->xmax = x[2];
                transdat->ymin = y[0];
                transdat->ymax = y[2];

		transdat->a[0][0] = x[0];
		transdat->a[1][0] = y[0];
		transdat->a[2][0] = 1.0;
		transdat->a[0][1] = x[1];
		transdat->a[1][1] = y[1];
		transdat->a[2][1] = 1.0;
		transdat->a[0][2] = x[2];
		transdat->a[1][2] = y[2];
		transdat->a[2][2] = 1.0;
	
		transdat->indx[0] = 0;
		transdat->indx[1] = 0;
		transdat->indx[2] = 0;

		transdat->d = 0;

		k = 3;
		j = 3;

/* FORTRAN */ _NHLCALLF(sgefa,SGEFA)(transdat->a,&k,&j, transdat->indx, &(transdat->d)); 
	}
}
/*
 * Function:	_NhlComputeSegTrans
 *
 * Description: Computes a segment transformation that can be used as a 
 * 	parameter to the NCAR function GSSGT based on new points provided
 *	as parameters. 
 *
 * In Args: xprime and yprime are arrays of the new points to which to 
 * 	transform points to. transdat is the information needed to compute the
 *	transformation matrix. transform is a 2x3 array into which the 
 *	transformation matrix is written.
 *
 *Input data must be ordered:
 *
 *	p1-------------p2
 *	|
 *	|
 *	|
 *	|
 *	|
 *	p0
 *
 *	p0 = (x[0],y[0])
 *	p1 = (x[1],y[1])
 *	p2 = (x[2],y[2])
 *
 *
 * Out Args:	transform contatins matrix
 *
 * Return Values: transform is also returned as a return value.
 *
 * Side Effects: NONE
 */

void	_NhlComputeSegTrans
#if	NhlNeedProto
(NhlTransDat	*transdat, float *transform, float *xprime, float *yprime)
#else
(transdat,transform,xprime,yprime) 
	NhlTransDat	*transdat;
	float	*transform;
	float *xprime;
	float *yprime;
#endif
{
	int i,j,k;
	float xprimef[3];
	float yprimef[3];
	k = 3;
	j = 3;
	i = 0;
	
	xprimef[0] = xprime[0];
	xprimef[1] = xprime[1];
	xprimef[2] = xprime[2];
	yprimef[0] = yprime[0];
	yprimef[1] = yprime[1];
	yprimef[2] = yprime[2];


/* FORTRAN */ _NHLCALLF(sgesl,SGESL)(transdat->a,&k,&j,transdat->indx,xprimef,&i);
	transform[0] = (float)xprimef[0];
	transform[2] = (float)xprimef[1];
	transform[4] = (float)xprimef[2];

/* FORTRAN */ _NHLCALLF(sgesl,SGESL)(transdat->a,&k,&j,transdat->indx,yprimef,&i);
	transform[1] = (float)yprimef[0];
	transform[3] = (float)yprimef[1];
	transform[5] = (float)yprimef[2];
	
	return;
	
}

/*
 * Function:	_NhlSegmentSpansArea
 *
 * Description:	Checks to see if a segment, which may be incomplete because
 * it was drawn when the viewport extends partially outside the viewspace,
 * covers the area that now is requested. If not, it returns False.
 *
 * In Args:	transdat	contains among other things the min and max
 *                              x and y when the segment was created
 *              xmin,xmax,ymin,ymax the current viewport boundaries.
 *
 * Out Args: 	NONE
 *
 * Return Values: 	True if the segment contains enough info to draw
 *                      the picture.
 *
 * Side Effects:	Segment drawn to workstation.
 */

NhlBoolean _NhlSegmentSpansArea
#if	NhlNeedProto
(
        NhlTransDat	*transdat,
        float		xmin,
        float		xmax,
        float		ymin,
        float		ymax
        )
#else
(transdat,xmin,xmax,ymin,ymax)
	NhlTransDat	*transdat;
        float		xmin;
        float		xmax;
        float		ymin;
        float		ymax;
#endif
{
        float owidth,oheight,nwidth,nheight;

        if (! transdat || transdat->id == SEG_NOT_SET)
                return False;

        if (transdat->xmin >= 0.0 && transdat->xmax <= 1.0 &&
            transdat->ymin >= 0.0 && transdat->ymax <= 1.0)
                return True;
                
        owidth = transdat->xmax - transdat->xmin;
        oheight = transdat->ymax - transdat->ymin;
        nwidth = xmax - xmin;
        nheight = ymax - ymin;

        if (transdat->xmin < 0.0) {
                if (xmin >= 0.0)
                        return False;
                if (transdat->xmin / owidth < xmin / nwidth)
                        return False;
        }
        if (transdat->xmax > 1.0) {
                if (xmax <= 1.0)
                        return False;
                if ((transdat->xmax-1.0) / owidth > (xmax-1.0) / nwidth)
                        return False;
        }
        if (transdat->ymin < 0.0) {
                if (ymin >= 0.0)
                        return False;
                if (transdat->ymin / oheight < ymin / nheight)
                        return False;
        }
        if (transdat->ymax > 1.0) {
                if (ymax <= 1.0)
                        return False;
                if ((transdat->ymax-1.0) / oheight > (ymax-1.0) / nheight)
                        return False;
        }

        return True;
}

/*
 * Function:	_NhlDrawSegment
 *
 * Description:	Draws an existing segment  on an open workstation.
 *
 * In Args:	transdat	contains all information needed.
 *		wksid 		workstation to copy segment to.
 *
 * Out Args: 	NONE
 *
 * Return Values: 	NONE
 *
 * Side Effects:	Segment drawn to workstation.
 */

NhlErrorTypes _NhlDrawSegment
#if	NhlNeedProto
(NhlTransDat	*transdat,int	wksid)
#else
(transdat,wksid)
	NhlTransDat	*transdat;
	int		wksid;
#endif
{
	if(transdat->id != SEG_NOT_SET) {
		if(wksisopn(wksid)&&(wksisact(wksid))) {
	/* FORTRAN */ _NHLCALLF(gcsgwk,GCSGWK)(&(wksid),&(transdat->id));
			return(NhlNOERROR);
		}
		else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NhlDrawSegment: Workstation is not open or inactive. ");
			return(NhlWARNING);
		}
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NhlDrawSegment: _NhlStartSegment never called. ");
		return(NhlWARNING);
		
	}
}



/*
 * Function:	_NhlEvalTrans
 *
 * Description:	 Transforms a point and returns it new location
 *		transformation matrix is ordered as follows:
 *		
 *		M = | M11 M12 M13 |
 *                  | M21 M22 M23 |
 *
 *		M11 = transform[0]
 *		M21 = transform[1]
 *		M12 = transform[2]
 *		M22 = transform[3]
 *		M13 = transform[4]
 *		M23 = transform[5]
 *
 * In Args:	tranform 	transformation matrix 
 *		x,y		original point
 *
 * Out Args: 	
 *		xprime,yprime	return pointers for output
 *
 * Return Values: NONE
 *
 * Side Effects: NONE
 */
void	_NhlEvalTrans
#if	NhlNeedProto
(float	*transform,float x,float y, float *xprime, float *yprime)
#else
(transform,x,y,xprime,yprime)
	float	*transform;
	float	x;
	float	y;
	float	*xprime;
	float	*yprime;
#endif
{
	*xprime = x * transform[0] + y * transform[2] + transform[4];
	*yprime = x * transform[1] + y * transform[3] + transform[5];
	return;
}
	

/*
 * Function:	_NhlStartSegment
 *
 * Description:	Starts recording a segment
 *
 * In Args: 	transdat	contains all necessary information.
 *
 * Out Args:	NONE
 *
 * Return Values: NhlErrorTypes
 *
 * Side Effects: Segment now open and recording if _NhlEndSegment not called
 *		then problems will arise.
 */

NhlErrorTypes	_NhlStartSegment
#if	NhlNeedProto
(NhlTransDat	*transdat)
#else
(transdat)
	NhlTransDat	*transdat;
#endif
{
	int i;
	if (Init_Required) {
		memset(&Id_Assigned,(char)0,MAX_SEG*sizeof(char));
		Init_Required = False;
	}
	if(transdat->id == SEG_NOT_SET) {
		if (Id == MAX_SEG) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
"%s: no more segments available; view object will be drawn to primary workstation only",
				  "_NhlStartSegment");
			return NhlWARNING;
		}
		transdat->id = Id;
		Id_Assigned[Id] = 1;
		for (i= Id;i<=MAX_SEG;i++) {
			if (! Id_Assigned[i]) {
				Id = i;
				break;
			}
		}
	}
/* FORTRAN */ _NHLCALLF(gcrsg,GCRSG)(&(transdat->id));
	return NhlNOERROR;
}

/*
 * Function:	_NhlSetSegTrans
 *
 * Description:  Assigns a tranformation matrix to a segment id.
 *
 * In Args:	transdat	all information needed to set segment trans
 *		transform	tranformation matrix.
 *
 * Out Args: 	NONE
 *
 * Return Values: NONE
 *
 * Side Effects: segment tranfromation of segid is changed
 */

void _NhlSetSegTrans
#if	NhlNeedProto
(NhlTransDat	*transdat, float *transform)
#else
(transdat,transform)
	NhlTransDat	*transdat;
	float		*transform;
#endif
{
	if(transdat->id != SEG_NOT_SET) {
/* FORTRAN */ _NHLCALLF(gssgt,GSSGT)(&(transdat->id),transform);
	} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NhlSetSegTrans: _NhlStartSeg was never called. ");
			return;
	}
	return;
}


/*
 * Function:	_NhlEndSegment
 *
 * Description:	Closes a previously opened segment segment
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	NONE
 *
 * Side Effects:  Segment stops recording primatives.
 */

void	_NhlEndSegment
#if	NhlNeedProto
(
	NhlTransDat	*transdat
)
#else
(transdat)
	NhlTransDat	*transdat;
#endif
{
	if(transdat->id != SEG_NOT_SET) {
/* FORTRAN */ 
		_NHLCALLF(gclsg,GCLSG)();
	}
	return;
}
