/*
 *      $Id: xcb.c,v 1.1 1997-06-11 20:49:22 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1997			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xcb.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Mar 4 14:05:09 MST 1997
 *
 *	Description:	
 */
#include <Xcb/xcbP.h>

#include <X11/Xatom.h>
#include <X11/Xlibint.h>

#include <Xm/Xm.h>

Visual
*XcbBestDepthVisual
(
	Display		*dpy,
	int		scr,
	NhlBoolean	rw
)
{
	unsigned long	flags;
	XVisualInfo	*vinfo,templ;
	Visual		*vis;
	int		numvis,i,best;
	int		best_depth;
	NhlBoolean	best_rw,curr_rw;
	NhlBoolean	curr_gray,best_gray;

	flags = VisualScreenMask;
	templ.screen = scr;
	vinfo = XGetVisualInfo(dpy,flags,&templ,&numvis);
	if(!vinfo)
		return DefaultVisual(dpy,scr);

	best = 0;
	best_depth = vinfo[0].depth;
	best_gray = False;
	switch(vinfo[0].class){
		case StaticGray:
			best_gray = True;
		case StaticColor:
		case TrueColor:
			best_rw = False;
			break;
		case GrayScale:
			best_gray = True;
		case PseudoColor:
		case DirectColor:
			best_rw = True;
			break;
		default:
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,"Unknown Visual"));
			XFree(vinfo);
			return DefaultVisual(dpy,scr);
	}

	for(i=1;i<numvis;i++){
		curr_gray = False;
		switch(vinfo[i].class){
			case StaticGray:
				curr_gray = True;
			case StaticColor:
			case TrueColor:
				curr_rw = False;
				break;
			case GrayScale:
				curr_gray = True;
			case PseudoColor:
			case DirectColor:
				curr_rw = True;
				break;
			default:
				continue;
		}

		if(vinfo[i].depth < best_depth)
			continue;

		/*
		 * The depth is better take it, or take color over gray,
		 * or take the requested rw/ro model, or
		 * take TrueColor and DirectColor over other models.
		 */
		if(	(vinfo[i].depth > best_depth)		||
			(best_gray && !curr_gray)		||
			(!best_rw && curr_rw)			||
			(vinfo[i].class == DirectColor)		||
			(!best_rw && (vinfo[i].class == TrueColor))){
			best = i;
			best_depth = vinfo[i].depth;
			best_rw = curr_rw;
			best_gray = curr_gray;
		}
	}

	vis = vinfo[best].visual;
	XFree(vinfo);

	return vis;
}

static float
_XcbRGBDist
(
	XColor	*req,
	XColor	*cmpcol,
	void	*cmp_data
)
{
	Xcb	xcb = (Xcb)cmp_data;
	float	t,cerr;

	if(!cmpcol)
		return xcb->rgb_err_sqr;

	t = req->red - cmpcol->red;
	cerr = t*t;
	t = req->green - cmpcol->green;
	cerr += (t*t);
	t = req->blue - cmpcol->blue;
	cerr += (t*t);

	return cerr;
}

static
int
GetStdCmap(
	Xcb	xcb
)
{
	int			status=0;
	XStandardColormap	*def=NULL,*best=NULL;
	int			ndef=0,nbest=0,i;

	if(xcb->gray){
		XGetRGBColormaps(xcb->dpy,RootWindow(xcb->dpy,xcb->scr),
						&best,&nbest,XA_RGB_GRAY_MAP);
		XGetRGBColormaps(xcb->dpy,RootWindow(xcb->dpy,xcb->scr),
						&def,&ndef,XA_RGB_DEFAULT_MAP);
	}
	else{
		XGetRGBColormaps(xcb->dpy,RootWindow(xcb->dpy,xcb->scr),
						&best,&nbest,XA_RGB_BEST_MAP);
		XGetRGBColormaps(xcb->dpy,RootWindow(xcb->dpy,xcb->scr),
						&def,&ndef,XA_RGB_DEFAULT_MAP);
	}

	for(i=0;i<nbest;i++){
		if((best[i].visualid == xcb->visinfo->visualid) &&
		((xcb->cmap == None)||(best[i].colormap == xcb->cmap)) &&
		((xcb->gray  && best[i].red_max+1 >= xcb->graylevels) ||
		(best[i].red_max+1 >= xcb->rlevels) &&
		(best[i].green_max+1 >= xcb->glevels) &&
		(best[i].blue_max+1 >= xcb->blevels) &&
		(best[i].red_max+1 <= 8) &&
		(best[i].green_max+1 <= 8) &&
		(best[i].blue_max+1 <= 8))){
			xcb->stdcmap = best[i];
			xcb->cmap = best[i].colormap;
			status = 1;
			break;
		}
	}
	if(best)
		XFree(best);

	if(!status)
		for(i=0;i<ndef;i++){
			if((def[i].visualid == xcb->visinfo->visualid) &&
			((xcb->cmap == None)||(def[i].colormap == xcb->cmap)) &&
			(def[i].red_max+1 >= xcb->rlevels) &&
			(def[i].green_max+1 >= xcb->glevels) &&
			(def[i].blue_max+1 >= xcb->blevels) &&
			(best[i].red_max+1 <= 8) &&
			(best[i].green_max+1 <= 8) &&
			(best[i].blue_max+1 <= 8)){
				xcb->stdcmap = def[i];
				xcb->cmap = def[i].colormap;
				status = 1;
				break;
			}
		}
	if(def)
		XFree(def);

	if(!status)
		return status;

	/*
	 * Hack to set green/blue maxs correctly.  From XCC (John L. Cwikla)
	 * modified to set green/blue max to one.
	 */

	if((!xcb->stdcmap.green_max + !xcb->stdcmap.blue_max +
						!xcb->stdcmap.red_max) > 1){
		if(xcb->stdcmap.green_max){
			xcb->stdcmap.red_max = xcb->stdcmap.green_max;
			xcb->stdcmap.red_mult = xcb->stdcmap.green_mult;
		}
		else if(xcb->stdcmap.blue_max){
			xcb->stdcmap.red_max = xcb->stdcmap.blue_max;
			xcb->stdcmap.red_mult = xcb->stdcmap.blue_mult;
		}
		xcb->stdcmap.green_max = xcb->stdcmap.blue_max = 0;
		xcb->stdcmap.green_mult = xcb->stdcmap.blue_mult = 0;

		xcb->graylevels = xcb->stdcmap.red_max + 1;
	}

	xcb->rlevels = xcb->stdcmap.red_max + 1;
	xcb->glevels = xcb->stdcmap.green_max + 1;
	xcb->blevels = xcb->stdcmap.blue_max + 1;

	return status;
}

static int
_XcbRefSort(
	const void	*arg1,
	const void	*arg2
)
{
	_XcbCStat	s1 = *(_XcbCStat*)arg1;
	_XcbCStat	s2 = *(_XcbCStat*)arg2;
	int		i;

	i = s1->ref - s2->ref;

	if(i!=0)
		return i;

	/*
	 * The larger the pixel number, the smaller its ref number should be.
	 * (most apps alloc from low end of table first, this will cause this
	 * one to alloc from top first - hopefully reducing color flash)
	 */
	return (int)((long long)s2->xcol.pixel-(long long)s1->xcol.pixel);
}

static int
_XcbDMapRefSort(
	const void	*arg1,
	const void	*arg2
)
{
	_XcbDMStat	s1 = *(_XcbDMStat*)arg1;
	_XcbDMStat	s2 = *(_XcbDMStat*)arg2;
	int		i;

	i = s1->ref - s2->ref;

	if(i!=0)
		return i;

	return (s2->val - s1->val);
}

static void
_XcbDoRefSort(
	Xcb	xcb
)
{
	if(!xcb || (xcb->sort_kind == XcbREFSORT))
		return;

	if(xcb->visinfo->class == DirectColor){
		qsort(xcb->rsort,xcb->rval+1,sizeof(_XcbDMStat),
							_XcbDMapRefSort);
		qsort(xcb->gsort,xcb->gval+1,sizeof(_XcbDMStat),
							_XcbDMapRefSort);
		qsort(xcb->bsort,xcb->bval+1,sizeof(_XcbDMStat),
							_XcbDMapRefSort);
	}
	else if(xcb->ncsort)
		qsort(xcb->csort,xcb->ncsort,sizeof(_XcbCStat),_XcbRefSort);

	xcb->sort_kind = XcbREFSORT;

	return;
}

static int
_XcbPixSort(
	const void	*arg1,
	const void	*arg2
)
{
	return (*(_XcbCStat*)arg1)->xcol.pixel-(*(_XcbCStat*)arg2)->xcol.pixel;
}

static void
_XcbDoPixSort(
	Xcb	xcb
)
{
	if(!xcb || (xcb->sort_kind == XcbPIXSORT))
		return;

	if(xcb->ncsort)
		qsort(xcb->csort,xcb->ncsort,sizeof(_XcbCStat),_XcbPixSort);

	xcb->sort_kind = XcbPIXSORT;

	return;
}

static int
_XcbDMapValSort(
	const void	*arg1,
	const void	*arg2
)
{
	_XcbDMStat	s1 = *(_XcbDMStat*)arg1;
	_XcbDMStat	s2 = *(_XcbDMStat*)arg2;

	if((s1->ref < 1) && (s2->ref < 1))
		return 0;
	if(s1->ref < 1)
		return 1;
	if(s2->ref < 1)
		return -1;

	if(s1->rw == s2->rw)
		return (s1->val - s2->val);

	if(s1->rw)
		return 1;

	return -1;
}

static void
_XcbDoValSort(
	Xcb	xcb
)
{
	if(!xcb || (xcb->visinfo->class != DirectColor) ||
						(xcb->sort_kind == XcbVALSORT))
		return;

	qsort(xcb->rsort,xcb->rval+1,sizeof(_XcbDMStat),_XcbDMapValSort);
	qsort(xcb->gsort,xcb->gval+1,sizeof(_XcbDMStat),_XcbDMapValSort);
	qsort(xcb->bsort,xcb->bval+1,sizeof(_XcbDMStat),_XcbDMapValSort);

	xcb->sort_kind = XcbVALSORT;

	return;
}

static _XcbCStat
_XcbAllocNode(
void
)
{
	_XcbCStat	new = Xmalloc(sizeof(_XcbCStatRec));

	if(!new){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	memset(new,0,sizeof(_XcbCStatRec));
	return new;
}

static void
_XcbAddToSort(
	Xcb		xcb,
	_XcbCStat	node
)
{
	if(!xcb->csort)
		return;

	if((xcb->ncsort+1)>xcb->scsort){
		xcb->scsort *= 2;
		xcb->csort = Xrealloc(xcb->csort,sizeof(_XcbCStat)*xcb->scsort);
		if(!xcb->csort){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			xcb->ncsort = 0;
			xcb->scsort = 0;
			return;
		}
	}
	xcb->csort[xcb->ncsort++] = node;

	xcb->sort_kind = XcbNOSORT;

	return;
}

static void
_XcbAddToRW(
	Xcb		xcb,
	_XcbCStat	node
)
{
	if(!xcb->srwcells || ((xcb->nrwcells+1) > xcb->srwcells)){
		if(xcb->srwcells > 0)
			xcb->srwcells *= 2;
		else
			xcb->srwcells = 8;
		xcb->rwcells = Xrealloc(xcb->rwcells,
					sizeof(_XcbCStat)*xcb->srwcells);
		if(!xcb->rwcells){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			xcb->nrwcells = 0;
			xcb->srwcells = 0;
			return;
		}
	}
	xcb->rwcells[xcb->nrwcells++] = node;

	return;
}

static _XcbCStat
_XcbNewNode(
	Xcb		xcb,
	unsigned long	pix
)
{
	_XcbCStat	new;
	
	if(xcb->cstat){
		if(pix >= xcb->scstat){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
						"Invalid Pixel Value!?!?!"));
			return NULL;
		}
		new = &xcb->cstat[pix];
		if(new->ref != 0){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Not New!!!"));
			return NULL;
		}
		new->alloc = XcbNONE;
		new->cube = False;
		new->next = NULL;
	}
	else{
		new = _XcbAllocNode();
	}

	new->xcol.pixel = pix;
	_XcbAddToSort(xcb,new);

	return new;
}

/*
 * Must call DoRefSort before calling this...
 */
static NhlBoolean
GetNullIndex(
	_XcbDMStat	*sort,
	unsigned int	size,
	_XcbDMStat	plist,
	unsigned long	*ret
)
{
	int		i=0;
	unsigned long	pix;
	int		ref;

	if(sort[i]->ref > 0)
		return False;
	pix = sort[i]->index;
	if(plist){
		ref = plist[pix].ref;
		for(i=1;(i<size)&& sort[i]->ref < 1;i++){
			if(plist[sort[i]->index].ref < ref){
				pix = sort[i]->index;
				ref = plist[sort[i]->index].ref;
			}
		}
	}
	*ret = pix;
	return True;
}

static _XcbCStat
_XcbFindNullRefNode(
	Xcb	xcb
)
{
	if(!xcb->rw || !xcb->my_cmap) return NULL;

	if(xcb->visinfo->class != DirectColor){
		NhlBoolean	found=False;
		int		i,sorti=0,psorti=0;
		unsigned long	bpix,npix;
		int		bpref,npref;
		_XcbCStat	node;

		_XcbDoPixSort(xcb);
		_XcbDoPixSort(xcb->parent);

		for(i=0;i<xcb->visinfo->colormap_size;i++){
			while(sorti<xcb->ncsort){
				node = xcb->csort[sorti++];
				if(node->xcol.pixel < i)
					continue;
				if(node->xcol.pixel > i)
					break;
				goto NEXTINDEX;
			}

			found = True;
			bpix = i;
			break;

			NEXTINDEX:
			;
		}
		if(!found) return NULL;

		if(xcb->parent){
			found=False;
			while(psorti<xcb->parent->ncsort){
				node = xcb->parent->csort[psorti++];
				if(node->xcol.pixel < bpix)
					continue;
				if(node->xcol.pixel > bpix)
					break;
				found=True;
				bpref = node->ref;
				break;
			}

			/*
			 * If found, parent is using the first pixel we found
			 * in xcb, so we should continue looking for the
			 * smallest parent ref.
			 */
			if(found){
				for(i=bpix+1;i<xcb->visinfo->colormap_size;i++){
					found = False;
					while(sorti<xcb->ncsort){
						node = xcb->csort[sorti++];
						if(node->xcol.pixel < i)
							continue;
						if(node->xcol.pixel > i)
							break;
						found = True;
						break;
					}

					if(found) continue;

					npix = i;
					found = False;
					while(psorti<xcb->parent->ncsort){
						node = xcb->parent->csort[psorti++];
						if(node->xcol.pixel < npix)
							continue;
						if(node->xcol.pixel > npix)
							break;
						found=True;
						if(node->ref < bpref){
							bpref = node->ref;
							bpix = npix;
						}
						break;
					}
					if(!found)
						break;
				}
			}
		}
		return _XcbNewNode(xcb,bpix);
	}
	else{
		/*
		 * Do DirectColor case.
		 */
		unsigned long	ir,ig,ib;
	
		_XcbDoRefSort(xcb);
		if(!GetNullIndex(xcb->rsort,xcb->rval+1,
				(xcb->parent)?(xcb->parent->rmap):(NULL),&ir))
			return NULL;
		if(!GetNullIndex(xcb->gsort,xcb->gval+1,
				(xcb->parent)?(xcb->parent->gmap):(NULL),&ig))
			return NULL;
		if(!GetNullIndex(xcb->bsort,xcb->bval+1,
				(xcb->parent)?(xcb->parent->bmap):(NULL),&ib))
			return NULL;
		ir = (ir << xcb->shifts.red) & xcb->masks.red;
		ig = (ig << xcb->shifts.green) & xcb->masks.green;
		ib = (ib << xcb->shifts.blue) & xcb->masks.blue;
	
		return _XcbNewNode(xcb,ir|ig|ib);
	}
}

static _XcbCStat
_XcbGetCubeNodeList(
	Xcb		xcb,
	XColor		*col
)
{
	/*
	 * Compute "cube" indices.
	 */
	xcb->ired = col->red * (xcb->mycube.red_max + 1) / 0xffff;
	if(xcb->ired > (xcb->mycube.red_max))
		xcb->ired = xcb->mycube.red_max;
	xcb->icube = xcb->ired * xcb->mycube.red_mult;

	xcb->igreen = col->green * (xcb->mycube.green_max + 1) / 0xffff;
	if(xcb->igreen > (xcb->mycube.green_max))
		xcb->igreen = xcb->mycube.green_max;
	xcb->icube += (xcb->igreen * xcb->mycube.green_mult);

	xcb->iblue = col->blue * (xcb->mycube.blue_max + 1) / 0xffff;
	if(xcb->iblue > (xcb->mycube.blue_max))
		xcb->iblue = xcb->mycube.blue_max;
	xcb->icube += (xcb->iblue * xcb->mycube.blue_mult);

	return xcb->cube[xcb->icube];
}

static _XcbCStat
FindDupNode(
	Xcb		xcb,
	XColor		*col
)
{
	_XcbCStat	node = _XcbGetCubeNodeList(xcb,col);
	while(node && (node->xcol.pixel != col->pixel))
		node = node->next;
	return node;
}

static void
_XcbRemoveRWCell(
	Xcb		xcb,
	_XcbCStat	node
)
{
	int		i;
	NhlBoolean	found = False;

	for(i=0;i<xcb->nrwcells;i++){
		if(xcb->rwcells[i] == node){
			found = True;
			break;
		}
	}

	if(found){
		xcb->nrwcells--;
		for(;i<xcb->nrwcells;i++)
			xcb->rwcells[i]=xcb->rwcells[i+1];
	}

	return;
}

void
InitBW(
	Xcb	xcb
)
{
	XColor	col;

	xcb->vtype = XcbBW;
	if(xcb->visinfo->visual != DefaultVisual(xcb->dpy,xcb->scr)){
		int		n;
		XVisualInfo	vtmpl;

		XFree(xcb->visinfo);
		xcb->vis = DefaultVisual(xcb->dpy,xcb->scr);
		vtmpl.visualid = XVisualIDFromVisual(xcb->vis);
		xcb->visinfo = XGetVisualInfo(xcb->dpy,VisualIDMask,&vtmpl,&n);
	}
	xcb->cmap = DefaultColormap(xcb->dpy,xcb->scr);
	xcb->mode = XcbSHAREDCMAP;

	xcb->black = BlackPixel(xcb->dpy,xcb->scr);
	xcb->white = WhitePixel(xcb->dpy,xcb->scr);

	xcb->rw = False;
	xcb->rinc = 0xffff;
	xcb->ginc = 0;
	xcb->binc = 0;

	return;
}

static void
InitTrue(
	Xcb	xcb
)
{
	unsigned long rmask,gmask,bmask;

	xcb->vtype = XcbTRUE;

	rmask = xcb->masks.red = xcb->visinfo->red_mask;
	xcb->shifts.red = 0;
	xcb->bits.red = 0;
	while(!(rmask & 1)){
		rmask >>= 1;
		xcb->shifts.red++;
	}
	while(rmask & 1){
		rmask >>= 1;
		xcb->bits.red++;
	}

	gmask = xcb->masks.green = xcb->visinfo->green_mask;
	xcb->shifts.green = 0;
	xcb->bits.green = 0;
	while(!(gmask & 1)){
		gmask >>= 1;
		xcb->shifts.green++;
	}
	while(gmask & 1){
		gmask >>= 1;
		xcb->bits.green++;
	}

	bmask = xcb->masks.blue = xcb->visinfo->blue_mask;
	xcb->shifts.blue = 0;
	xcb->bits.blue = 0;
	while(!(bmask & 1)){
		bmask >>= 1;
		xcb->shifts.blue++;
	}
	while(bmask & 1){
		bmask >>= 1;
		xcb->bits.blue++;
	}

	return;
}

void
InitDirect(
	Xcb	xcb
)
{
	int	i;

	xcb->scube = xcb->rlevels * xcb->glevels * xcb->blevels;
	xcb->scstat = 0;
	xcb->scsort = 64;

	xcb->cube = Xmalloc(sizeof(_XcbCStat)*xcb->scube);
	xcb->csort = Xmalloc(sizeof(_XcbCStat)*xcb->scsort);

	if(!xcb->cube || !xcb->csort){
		InitBW(xcb);
		return;
	}

	memset(xcb->cube,0,sizeof(_XcbCStat)*xcb->scube);
	memset(xcb->csort,0,sizeof(_XcbCStat)*xcb->scsort);

	/*
	 * Use stdcmap structure to index "cube"
	 */
	xcb->mycube.red_max = xcb->rlevels - 1;
	xcb->mycube.green_max = xcb->glevels - 1;
	xcb->mycube.blue_max = xcb->blevels - 1;
	xcb->mycube.red_mult = xcb->glevels * xcb->blevels;
	xcb->mycube.green_mult = xcb->blevels;
	xcb->mycube.blue_mult = 1;
	xcb->mycube.base_pixel = 0;

	xcb->rinc = ((float)0xffff)/(xcb->mycube.red_max);
	xcb->ginc = ((float)0xffff)/(xcb->mycube.green_max);
	xcb->binc = ((float)0xffff)/(xcb->mycube.blue_max);

	xcb->rval = xcb->visinfo->red_mask >> xcb->shifts.red;
	xcb->gval = xcb->visinfo->green_mask >> xcb->shifts.green;
	xcb->bval = xcb->visinfo->blue_mask >> xcb->shifts.blue;

	xcb->rmap = Xmalloc(sizeof(_XcbDMStatRec)*(xcb->rval+1));
	xcb->gmap = Xmalloc(sizeof(_XcbDMStatRec)*(xcb->gval+1));
	xcb->bmap = Xmalloc(sizeof(_XcbDMStatRec)*(xcb->bval+1));
	xcb->rsort = Xmalloc(sizeof(_XcbDMStat)*(xcb->rval+1));
	xcb->gsort = Xmalloc(sizeof(_XcbDMStat)*(xcb->gval+1));
	xcb->bsort = Xmalloc(sizeof(_XcbDMStat)*(xcb->bval+1));
	if(!xcb->rmap || !xcb->gmap || !xcb->bmap ||
				!xcb->rsort || !xcb->gsort || !xcb->bsort){
		XFree(xcb->cube);
		InitBW(xcb);
		return;
	}

	memset(xcb->rmap,0,sizeof(_XcbDMStatRec)*(xcb->rval+1));
	memset(xcb->gmap,0,sizeof(_XcbDMStatRec)*(xcb->gval+1));
	memset(xcb->bmap,0,sizeof(_XcbDMStatRec)*(xcb->bval+1));
	for(i=0;i<=xcb->rval;i++){
		xcb->rmap[i].index = i;
		xcb->rsort[i] = &xcb->rmap[i];
	}
	for(i=0;i<=xcb->gval;i++){
		xcb->gmap[i].index = i;
		xcb->gsort[i] = &xcb->gmap[i];
	}
	for(i=0;i<=xcb->bval;i++){
		xcb->bmap[i].index = i;
		xcb->bsort[i] = &xcb->bmap[i];
	}

	return;
}

void
InitGray(
	Xcb	xcb
)
{
	int	i;

	xcb->vtype = XcbGRAY;

	if(xcb->do_stdcmap)
		xcb->scube = xcb->rlevels * xcb->glevels * xcb->blevels;
	else
		xcb->scube = xcb->graylevels;
	xcb->scstat = xcb->visinfo->colormap_size;

	if(xcb->scstat > 512){
		xcb->scstat = 0;
		xcb->scsort = 256;
	}
	else{
		xcb->scsort = xcb->scstat;
	}

	if(xcb->scstat)
		xcb->cstat = Xmalloc(sizeof(_XcbCStatRec)*xcb->scstat);
	else
		xcb->cstat = NULL;
	xcb->csort = Xmalloc(sizeof(_XcbCStat)*(xcb->scsort));

	xcb->cube = Xmalloc(sizeof(_XcbCStat)*xcb->scube);

	if((xcb->scstat && !xcb->cstat) || !xcb->cube || !xcb->csort){
		InitBW(xcb);
		return;
	}

	if(xcb->cstat){
		memset(xcb->cstat,0,sizeof(_XcbCStatRec)*xcb->scstat);
		for(i=0;i<xcb->scstat;i++)
			xcb->cstat[i].xcol.pixel = i;
	}
	memset(xcb->csort,0,sizeof(_XcbCStat)*xcb->scsort);
	memset(xcb->cube,0,sizeof(_XcbCStat)*xcb->scube);

	xcb->rinc = ((float)0xffff)/(xcb->scube-1);
	xcb->ginc = 0.0;
	xcb->binc = 0.0;

	/*
	 * Use stdcmap structure to index "cube".
	 */
	xcb->mycube.red_max = xcb->scube - 1;
	xcb->mycube.red_mult = 1;
	xcb->mycube.green_max = xcb->mycube.blue_max = 0;
	xcb->mycube.green_mult = xcb->mycube.green_mult = 0;
	xcb->mycube.base_pixel = 0;

	return;
}

void
InitColor(
	Xcb	xcb
)
{
	int	i,j,k;

	xcb->vtype = XcbCOLOR;

	xcb->scube = xcb->rlevels * xcb->glevels * xcb->blevels;
	xcb->scstat = xcb->visinfo->colormap_size;

	if(xcb->scstat > 512){
		xcb->scstat = 0;
		xcb->scsort = 256;
	}
	else{
		xcb->scsort = xcb->scstat;
	}

	if(xcb->scstat)
		xcb->cstat = Xmalloc(sizeof(_XcbCStatRec)*xcb->scstat);
	else
		xcb->cstat = NULL;
	xcb->csort = Xmalloc(sizeof(_XcbCStat)*(xcb->scsort));
	xcb->cube = Xmalloc(sizeof(_XcbCStat)*xcb->scube);

	if((xcb->scstat && !xcb->cstat) || !xcb->cube || !xcb->csort){
		InitBW(xcb);
		return;
	}

	if(xcb->cstat){
		memset(xcb->cstat,0,sizeof(_XcbCStatRec)*xcb->scstat);
		for(i=0;i<xcb->scstat;i++)
			xcb->cstat[i].xcol.pixel = i;
	}

	memset(xcb->csort,0,sizeof(_XcbCStat)*xcb->scsort);
	memset(xcb->cube,0,sizeof(_XcbCStat)*xcb->scube);

	/*
	 * Use stdcmap structure to index "cube"
	 */
	xcb->mycube.red_max = xcb->rlevels - 1;
	xcb->mycube.green_max = xcb->glevels - 1;
	xcb->mycube.blue_max = xcb->blevels - 1;
	xcb->mycube.red_mult = xcb->glevels * xcb->blevels;
	xcb->mycube.green_mult = xcb->blevels;
	xcb->mycube.blue_mult = 1;
	xcb->mycube.base_pixel = 0;
	xcb->rinc = ((float)0xffff)/(xcb->mycube.red_max);
	xcb->ginc = ((float)0xffff)/(xcb->mycube.green_max);
	xcb->binc = ((float)0xffff)/(xcb->mycube.blue_max);

	return;
}

static void
FreeAllPixels
(
	Xcb	xcb
)
{
	_XcbCStat	node;
	unsigned long	fpix[64];
	int		i,nfpix=0;

	/*
	 * Free colors allocated from xcb->cmap...
	 */
	i=0;nfpix=0;
	while(i<xcb->ncsort){
		node = xcb->csort[i];
		if((NhlNumber(fpix)-nfpix) > 0)
			fpix[nfpix++] = node->xcol.pixel;
		else if(nfpix > 0){
			/*
			 * This nodes ref's won't fit, so empty
			 * fpix.
			 */
			if(xcb->parent)
				XcbFreeColors(xcb->parent,fpix,nfpix);
			else
				XFreeColors(xcb->dpy,xcb->cmap,fpix,
							nfpix,0);
			nfpix = 0;
			continue;
		}
		i++;
	}
	if(nfpix > 0){
		if(xcb->parent)
			XcbFreeColors(xcb->parent,fpix,nfpix);
		else
			XFreeColors(xcb->dpy,xcb->cmap,fpix,nfpix,0);
	}

	return;
}

static NhlBoolean
_XcbCFault(
	Xcb	xcb
)
{
	Visual		*old_vis;
	Colormap	new_cmap;
	XColor		cols[256];
	int		i,j,k,n;
	_XcbCStat	node;
	NhlArgVal	sel,cbdata;

	/*
	 * Don't cfault if XcbSHAREDCMAP, or my_cmap(already faulted...).
	 */
	if(((xcb->cmap != None) && (xcb->mode == XcbSHAREDCMAP))||xcb->my_cmap)
		return False;


	if(xcb->cmap == None){
		if(xcb->parent){
			old_vis = xcb->parent->vis;
			xcb->cmap = xcb->parent->cmap;
		}
		else{
			old_vis = DefaultVisual(xcb->dpy,xcb->scr);
			xcb->cmap = DefaultColormap(xcb->dpy,xcb->scr);
		}
	}
	else{
		old_vis = xcb->vis;
	}

	new_cmap = None;
	if(!xcb->rw && xcb->vis == old_vis){
		Display		*dpy = NULL;
		/*
		 * To copy the colormap without freeing the cells that have
		 * already been allocated, I have to open a new connection
		 * to the display, and create the new colormap from there.
		 * Then try allocating all the cells we are already using from
		 * the first colormap, if we don't get the same pixel numbers
		 * we had the first time around, then CFault has failed.
		 */

		dpy = XOpenDisplay(DisplayString(xcb->dpy));
		if(!dpy){
			xcb->mode = XcbSHAREDCMAP;
			return False;
		}

		/*
		 * If this actually free's the colormap, then
		 * we can only do this if (xcb->cmap == DefaultColormap)
		 * but that isn't clear from the documentation, so
		 * I'll try it.
		 */
		new_cmap = XCopyColormapAndFree(dpy,xcb->cmap);

		/*
		 * Allocate colors from new colormap.
		 * CMAPFAILED if it ever returns a different
		 * pixel then from the first cmap.
		 * (notice allocating colors using original display connecting,
		 * and not new one.  That is so they can be free'd from
		 * the original display connection, even after changing
		 * the closedownmode of the new connection.
		 */
		for(i=0; i < xcb->ncsort;i+=NhlNumber(cols)){
			n=MIN(NhlNumber(cols),(xcb->ncsort-i));
			for(j=0;j<n;j++)
				cols[j].pixel = xcb->csort[i+j]->xcol.pixel;
			XQueryColors(xcb->dpy,xcb->cmap,cols,n);
			for(j=0;j<n;j++){
				node = xcb->csort[i+j];
				XAllocColor(xcb->dpy,new_cmap,&cols[j]);
				if(cols[j].pixel != node->xcol.pixel){
					NHLPERROR((NhlFATAL,NhlEUNKNOWN,
							"Wrong Pix returned!"));
					new_cmap = None;
					goto FAILED;
				}
				node->alloc = XcbRO;
			}
		}
		/*
		 * Copying the colormap succeded - change the
		 * CloseDownMode so that the new colormap isn't
		 * destroyed when we close the "new" Display.
		 */
		XSetCloseDownMode(dpy,RetainPermanent);
	FAILED:
		XCloseDisplay(dpy);
	}
	else{
		new_cmap = XCreateColormap(xcb->dpy,
				RootWindow(xcb->dpy,xcb->scr),xcb->vis,
				((xcb->rw)?AllocAll:AllocNone));
	}

	if(new_cmap == None){
		xcb->mode = XcbSHAREDCMAP;
		return False;
	}

	FreeAllPixels(xcb);

	NhlINITVAR(sel);
	NhlINITVAR(cbdata);

	xcb->do_stdcmap = False;
	if(!xcb->rw){
		xcb->my_cmap = True;

		cbdata.ulngval = xcb->cmap = new_cmap;
		_NhlCBCallCallbacks(xcb->cfaultCBL,sel,cbdata);

		return True;
	}

	/*
	 * minimize color flashing by pre-loading
	 * colormap with all colors from old colormap.
	 */
	if(xcb->vis == old_vis){

		/*
		 * Attempt to minimize color flashing by
		 * pre-loading colormap with old cmap colors.
		 */
		if(xcb->visinfo->class == DirectColor){
			unsigned long	max;
			unsigned long	val;
			
			max = MAX(xcb->rval,MAX(xcb->gval,xcb->bval))+1;
			for(i=0;i < max;i+=NhlNumber(cols)){
				n = MIN(NhlNumber(cols),max-i);
				for(j=0;j<n;j++){
					val = i+j;
					cols[j].pixel =
				((val << xcb->shifts.red)& xcb->masks.red)|
				((val << xcb->shifts.green)& xcb->masks.green)|
				((val << xcb->shifts.blue)& xcb->masks.blue);
				}
				XQueryColors(xcb->dpy,xcb->cmap,cols,n);
				XStoreColors(xcb->dpy,new_cmap,cols,n);
			}
		}
		else{
			for(i=0; i < xcb->visinfo->colormap_size;
							i+=NhlNumber(cols)){
				n=MIN(NhlNumber(cols),
					(xcb->visinfo->colormap_size-i));
				for(j=0;j<n;j++)
					cols[j].pixel = i+j;
				XQueryColors(xcb->dpy,xcb->cmap,cols,n);
				XStoreColors(xcb->dpy,new_cmap,cols,n);
			}
		}
	}
	else if(xcb->ncsort > 0){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"Can't copy colors - WRONG VIS!"));
		XFreeColormap(xcb->dpy,new_cmap);
		xcb->mode = XcbSHAREDCMAP;
		return False;
	}

	for(i=0;i<xcb->ncsort;i++)
		xcb->csort[i]->alloc = XcbMYCMAP;
		
	xcb->my_cmap = True;
	cbdata.ulngval = xcb->cmap = new_cmap;
	_NhlCBCallCallbacks(xcb->cfaultCBL,sel,cbdata);

	return True;
}

static void
AllocCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	XColor		*col = (XColor*)cbdata.ptrval;
	XColor		xcol;
	Xcb		xcb = (Xcb)udata.ptrval;
	_XcbCStat	node=NULL;
	NhlArgVal	sel;

	if(!xcb->my_cmap || !xcb->rw || FindDupNode(xcb,col))
		goto DONE;

	xcol = *col;
	xcol.flags = 0;
	if(xcb->visinfo->class == DirectColor){
		unsigned long	ir,ig,ib;

		ir = (col->pixel & xcb->masks.red) >> xcb->shifts.red;
		ig = (col->pixel & xcb->masks.green) >> xcb->shifts.green;
		ib = (col->pixel & xcb->masks.blue) >> xcb->shifts.blue;
		if(xcb->rmap[ir].ref < 1)
			xcol.flags |= DoRed;
		if(xcb->gmap[ig].ref < 1)
			xcol.flags |= DoGreen;
		if(xcb->bmap[ib].ref < 1)
			xcol.flags |= DoBlue;
	}
	else
		xcol.flags = DoRed|DoGreen|DoBlue;

	XStoreColor(xcb->dpy,xcb->cmap,&xcol);

DONE:
	NhlINITVAR(sel);
	_NhlCBCallCallbacks(xcb->allocCBL,sel,cbdata);

	return;
}

static void
CFaultCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NhlArgVal	sel;
	Colormap	cmap = cbdata.ulngval;
	Xcb		xcb = (Xcb)udata.ptrval;

	if(xcb->my_cmap)
		return;

	xcb->cmap = cmap;
	/*
	 * Change colormap for all Widgets/Windows
	 */

	NhlINITVAR(sel);
	_NhlCBCallCallbacks(xcb->cfaultCBL,sel,cbdata);

	return;
}

static void
DestroyCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	XcbDestroy((Xcb)udata.ptrval);

	return;
}

typedef struct InitXmColorsRec{
	Display		*dpy;
	XrmDatabase	db;
} InitXmColorsRec;

static XrmQuark	bgQ = NULLQUARK;
static XrmQuark	BgQ = NULLQUARK;
static XrmQuark	strQ = NULLQUARK;
static XrmQuark	fgQ = NULLQUARK;
static XrmQuark hlQ = NULLQUARK;
static XrmQuark armQ = NULLQUARK;
static XrmQuark troughQ = NULLQUARK;
static XrmQuark scQ = NULLQUARK;
static XrmQuark tsQ = NULLQUARK;
static XrmQuark bsQ = NULLQUARK;

static Bool
InitXmColors
(
	XrmDatabase	*db,
	XrmBindingList	bindings,
	XrmQuarkList	quarks,
	XrmRepresentation	*type,
	XrmValue		*value,
	XPointer		closure
)
{
	InitXmColorsRec	*rec = (InitXmColorsRec*)closure;
	XrmQuark	*qptr = quarks;
	XrmQuark	stack_quarks[100];
	XmColorProc	color_proc;
	char		*fg_str;
	char		*sc_str;
	char		*ts_str;
	char		*bs_str;
	XColor		bg,fg,sc,ts,bs;
	Colormap	cmap=DefaultColormap(rec->dpy,DefaultScreen(rec->dpy));
	XrmValue	val;

	while(*(qptr+1))
		qptr++;
	if((*qptr != bgQ) && (*qptr != BgQ))
		return False;


	if((*type != strQ) || !XParseColor(rec->dpy,cmap,value->addr,&bg))
		return False;
		
	/* Background specification found - set motif colors at this level */

	fg_str = Xmalloc(sizeof(char)*19);
	sc_str = Xmalloc(sizeof(char)*19);
	ts_str = Xmalloc(sizeof(char)*19);
	bs_str = Xmalloc(sizeof(char)*19);

	color_proc = XmGetColorCalculation();
	(*color_proc)(&bg,&fg,&sc,&ts,&bs);
	sprintf(fg_str,"rgb:%04x/%04x/%04x",fg.red,fg.blue,fg.green);
	sprintf(sc_str,"rgb:%04x/%04x/%04x",sc.red,sc.blue,sc.green);
	sprintf(ts_str,"rgb:%04x/%04x/%04x",ts.red,ts.blue,ts.green);
	sprintf(bs_str,"rgb:%04x/%04x/%04x",bs.red,bs.blue,bs.green);

	qptr = stack_quarks;
	while(*quarks)
		*qptr++ = *quarks++;
	*qptr = NULLQUARK;
	qptr--; /* point qptr at background specification */


	/*
	 * Foreground Colors...
	 */
	val.size = strlen(fg_str);
	val.addr = fg_str;

	*qptr = fgQ;/*XmNforeground*/
	XrmQPutResource(&rec->db,bindings,stack_quarks,strQ,&val);
	*qptr = hlQ;/*XmNhighlightColor*/
	XrmQPutResource(&rec->db,bindings,stack_quarks,strQ,&val);

	/*
	 * Select Colors...
	 */
	val.size = strlen(sc_str);
	val.addr = sc_str;

	*qptr = armQ;/*XmNarmColor*/
	XrmQPutResource(&rec->db,bindings,stack_quarks,strQ,&val);
	*qptr = troughQ;/*XmNtroughColor*/
	XrmQPutResource(&rec->db,bindings,stack_quarks,strQ,&val);
	*qptr = scQ;/*XmNselectColor*/
	XrmQPutResource(&rec->db,bindings,stack_quarks,strQ,&val);

	/*
	 * Top Shadow
	 */
	val.size = strlen(ts_str);
	val.addr = ts_str;

	*qptr = tsQ;/*XmNtopShadowColor*/
	XrmQPutResource(&rec->db,bindings,stack_quarks,strQ,&val);

	/*
	 * Bottom Shadow
	 */
	val.size = strlen(bs_str);
	val.addr = bs_str;
	*qptr = bsQ;/*XmNbottomShadowColor*/
	XrmQPutResource(&rec->db,bindings,stack_quarks,strQ,&val);

	return False;
}

Xcb
XcbCreate(
	Display		*dpy,
	XcbAttr		attr,
	unsigned long	attr_mask
)
{
	Xcb		xcb;
	Visual		*def_vis;
	Colormap	def_cmap;
	unsigned long	mask;
	int		n,i,j;
	XVisualInfo	vtempl;
	int		got_stdcmap=0;
	NhlBoolean	rw_req;
	XcbColCmp	cmp = XcbCmpRGB_DIST;

	if(!dpy)
		return NULL;

	xcb = Xmalloc(sizeof(XcbRec));
	if(!xcb){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	if(!attr)
		mask = 0;
	else
		mask = attr_mask;

	xcb->dpy = dpy;

	if(mask & XcbMODE)
		xcb->mode = attr->mode;
	else
		xcb->mode = XcbMIXEDCMAP;

	if(mask & XcbPARENT)
		xcb->parent = attr->parent;
	else
		xcb->parent = NULL;

	if(mask & XcbSCREEN)
		xcb->scr = attr->scr;
	else
		xcb->scr = DefaultScreen(dpy);

	def_vis = DefaultVisual(dpy,xcb->scr);
	def_cmap = DefaultColormap(dpy,xcb->scr);

	if(mask & XcbRWCOLS)
		rw_req = !(!attr->rw);
	else
		rw_req = 0;

	if(mask & XcbMAXNCOLS)
		xcb->max_ncols = attr->max_ncols;
	else
		xcb->max_ncols = 0;

	if(mask & XcbMINNCOLS)
		xcb->min_ncols = attr->min_ncols;
	else
		xcb->min_ncols = 0;

	xcb->ncols = 0;

	if(xcb->parent){
		i = xcb->parent->rlevels;
		j = xcb->parent->glevels;
		n = xcb->parent->blevels;
	}
	else{
		i = 6;
		j = 6;
		n = 6;
	}
	if(mask & XcbRGBLEVELS){
		xcb->rlevels = attr->rlevels;
		xcb->glevels = attr->glevels;
		xcb->blevels = attr->blevels;
		if(xcb->rlevels > 8 || xcb->rlevels < 2)
			xcb->rlevels = i;
		if(xcb->glevels > 8 || xcb->glevels < 2)
			xcb->glevels = j;
		if(xcb->blevels > 8 || xcb->blevels < 2)
			xcb->blevels = n;
	}
	else{
		xcb->rlevels = i;
		xcb->glevels = j;
		xcb->blevels = n;
	}

	if((mask & XcbGRAYLEVELS) && (attr->graylevels <= 256) &&
						(attr->graylevels > 2))
		xcb->graylevels = attr->graylevels;
	else if(xcb->parent)
		xcb->graylevels = xcb->parent->graylevels;
	else
		xcb->graylevels = 64;

	if(mask & XcbCOLCMP)
		cmp = attr->cmp;

	switch(cmp){
		case XcbCmpFUNC:
			xcb->cmpfunc = attr->cmpfunc;
			xcb->cmp_data = attr->cmp_data;
			break;

		default:
			xcb->cmpfunc = _XcbRGBDist;
			xcb->cmp_data = xcb;

			break;
	}

	if(xcb->parent)
		xcb->vis = xcb->parent->vis;
	else if(mask & XcbVIS)
		xcb->vis = attr->vis;
	else{
		if(xcb->mode == XcbSHAREDCMAP)
			xcb->vis = def_vis;
		else
			xcb->vis = XcbBestDepthVisual(dpy,xcb->scr,rw_req);
	}
	vtempl.visualid = XVisualIDFromVisual(xcb->vis);
	xcb->visinfo = XGetVisualInfo(dpy,VisualIDMask,&vtempl,&n);

	if(xcb->visinfo->class == TrueColor)
		xcb->mode = XcbSHAREDCMAP;

	if(xcb->parent && (xcb->mode != XcbPRIVATECMAP))
		xcb->cmap = xcb->parent->cmap;
	else if((mask & XcbCMAP) && (mask & XcbVIS))
		xcb->cmap = attr->cmap;
	else
		xcb->cmap = None;

	xcb->gray = False;
	switch(xcb->visinfo->class){
		case StaticGray:
			xcb->gray = True;
		case StaticColor:
		case TrueColor:
			xcb->rw = False;
			break;
		case GrayScale:
			xcb->gray = True;
		case PseudoColor:
		case DirectColor:
			xcb->rw = True;
			break;
	}

	/*
	 * stdcmap's are not really useful for a TrueColor visual.
	 */
	if((xcb->mode == XcbPRIVATECMAP) ||
		(xcb->visinfo->class == TrueColor) ||
		(xcb->visinfo->class == DirectColor))
		xcb->do_stdcmap = 0;
	else
		xcb->do_stdcmap = GetStdCmap(xcb);

	xcb->sort_kind = XcbNOSORT;
	xcb->cube = NULL;
	xcb->csort = NULL;
	xcb->cstat = NULL;
	xcb->scube = 0;
	xcb->scstat = 0;
	xcb->ncsort = 0;
	xcb->rmap = NULL;
	xcb->gmap = NULL;
	xcb->bmap = NULL;
	xcb->rsort = NULL;
	xcb->gsort = NULL;
	xcb->bsort = NULL;
	xcb->nrwcells = 0;
	xcb->srwcells = 0;
	xcb->rwcells = NULL;

	switch(xcb->visinfo->class){
		case StaticGray:
		case GrayScale:
			if(xcb->visinfo->colormap_size == 2)
				InitBW(xcb);
			else
				InitGray(xcb);
			break;

		case TrueColor:
			InitTrue(xcb);
			break;

		case DirectColor:
			InitTrue(xcb);
			InitDirect(xcb);
			break;

		case StaticColor:
		case PseudoColor:
			InitColor(xcb);
			break;
	}

	/*
	 * Make sure we have a colormap...
	 */
	xcb->my_cmap = False;
	if(xcb->cmap == None){
		if(xcb->vis == def_vis)
			xcb->cmap = def_cmap;
		else if (!_XcbCFault(xcb)){
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
						"Unable to create cmap"));
			XFree(xcb->visinfo);
			XFree(xcb);
			return NULL;
		}
	}

	if(xcb->cmpfunc == _XcbRGBDist){
		XColor	c1,c2;
		float	err;

		if((mask & XcbRGBERR) &&
				!(attr->rgberr < 0 || attr->rgberr > 100)){
			if(attr->rgberr >= 50)
				attr->rgberr = 0;
			xcb->percent_rgb_err = attr->rgberr;
		}
		else
			xcb->percent_rgb_err = 10;
		xcb->rgb_err_sqr = (float)xcb->percent_rgb_err *
				((float)_XcbDIST_BW/(float)100);
		xcb->rgb_err_sqr *= xcb->rgb_err_sqr;

		c1.red = c1.green = c1.blue = 0;
		c2.red = 1 * xcb->rinc;
		c2.green = 1 * xcb->ginc;
		c2.blue = 1 * xcb->binc;

		/*
		 * If the error from diagonal nodes in the rgb cube is less
		 * then the percentage error, then there is no reason to
		 * keep track of error (Any color at one of the nodes will
		 * work for any requested color.)
		 */
		err = _XcbRGBDist(&c1,&c2,xcb);
		if(err < xcb->rgb_err_sqr)
			xcb->rgb_err_sqr = 0;
	}

	xcb->allocCB = NULL;
	xcb->cfaultCB = NULL;
	xcb->destroyCB = NULL;

	xcb->allocCBL = NULL;
	xcb->cfaultCBL = _NhlCBCreate(0,NULL,NULL,NULL,NULL);
	xcb->destroyCBL = _NhlCBCreate(0,NULL,NULL,NULL,NULL);

	if(xcb->parent){
		NhlArgVal	sel,udata;

		NhlINITVAR(sel);
		NhlINITVAR(udata);
		sel.lngval = 0;
		udata.ptrval = xcb;

		/*
		 * Allocate Callback lists in the parent.
		 */
		if(!xcb->parent->allocCBL)
			xcb->parent->allocCBL = _NhlCBCreate(0,NULL,NULL,NULL,
									NULL);
		/*
		 * Add this col objects callbacks to the parent
		 */
		xcb->allocCB = _NhlCBAdd(xcb->parent->allocCBL,sel,AllocCB,
									udata);
		xcb->cfaultCB = _NhlCBAdd(xcb->parent->cfaultCBL,sel,CFaultCB,
									udata);
		xcb->destroyCB = _NhlCBAdd(xcb->parent->destroyCBL,sel,
							DestroyCB,udata);
	}

	if(mask & XcbINITMOTIF){
		static int	init = 0;

		XrmDatabase	sdb = XtScreenDatabase(
						DefaultScreenOfDisplay(dpy));
		InitXmColorsRec	rec;
		XrmValue	ret_val;
		char		*ret_type;
		XrmQuark	nQ[2] = {NULLQUARK,NULLQUARK};
		XrmQuark	cQ[2] = {NULLQUARK,NULLQUARK};
		
		if(attr->app_class_name){
			nQ[0] = XrmStringToQuark("no.res");
			cQ[0] = XrmStringToQuark(attr->app_class_name);
		}

		if(XrmGetResource(sdb,"XcbINITMOTIF","XcbINITMOTIF",&ret_type,
								&ret_val))
			goto XmDONE;

		if(!init){
			bgQ = XrmStringToQuark(XmNbackground);
			BgQ = XrmStringToQuark(XmCBackground);
			strQ = XrmStringToQuark(XmRString);
			fgQ = XrmStringToQuark(XmNforeground);
			hlQ = XrmStringToQuark(XmNhighlightColor);
			armQ = XrmStringToQuark(XmNarmColor);
			troughQ = XrmStringToQuark(XmNtroughColor);
			scQ = XrmStringToQuark(XmNselectColor);
			tsQ = XrmStringToQuark(XmNtopShadowColor);
			bsQ = XrmStringToQuark(XmNbottomShadowColor);
		}

		rec.dpy = dpy;
		rec.db = NULL;
		XrmPutStringResource(&rec.db,"XcbINITMOTIF","True");
		/*
		 * This fills the rec.db database with MotifColor
		 * specifications everywhere a Background specification
		 * is found in the current database.
		 */
		XrmEnumerateDatabase(sdb,nQ,cQ,XrmEnumAllLevels,
			InitXmColors,(XPointer)&rec);
		XrmCombineDatabase(rec.db,&sdb,False);
	}
XmDONE:

	return xcb;
}

void
XcbDestroy(
	Xcb	xcb
)
{
	NhlArgVal	sel,cbdata;
	/*
	 * Call destroy callback - this should destroy all children
	 * xcb objects, and allow any widgets/windows to be notified
	 * of the destroy before we actually destroy anything at this
	 * level.
	 */
	NhlINITVAR(sel);
	NhlINITVAR(cbdata);
	cbdata.ptrval = xcb;
	_NhlCBCallCallbacks(xcb->destroyCBL,sel,cbdata);

	_NhlCBDestroy(xcb->allocCBL);
	_NhlCBDestroy(xcb->cfaultCBL);
	_NhlCBDestroy(xcb->destroyCBL);
	_NhlCBDelete(xcb->allocCB);
	_NhlCBDelete(xcb->cfaultCB);
	_NhlCBDelete(xcb->destroyCB);

	/*
	 * This should hopefully be a no-op,since the callbacks should
	 * have caused anything that allocated colors to free them, but
	 * just in case...
	 */
	FreeAllPixels(xcb);

	if(xcb->my_cmap)
		XFreeColormap(xcb->dpy,xcb->cmap);
	XFree(xcb->visinfo);
#define NXFREE(ptr) if(ptr) XFree(ptr)
	NXFREE(xcb->rmap);
	NXFREE(xcb->gmap);
	NXFREE(xcb->bmap);
	NXFREE(xcb->rsort);
	NXFREE(xcb->gsort);
	NXFREE(xcb->bsort);
	NXFREE(xcb->cube);
	NXFREE(xcb->rwcells);
	NXFREE(xcb->csort);
	NXFREE(xcb->cstat);
#undef	NXFREE

	return;
}

static int
ulcmp(
	const void	*arg1,
	const void	*arg2
)
{
	return *(unsigned long*)arg1 - *(unsigned long*)arg2;
}

void
XcbFreeColors(
	Xcb		xcb,
	unsigned long	*pixels,
	int		npixels
)
{
	_XcbCStat	node;
	unsigned long	stack_spix[256],stack_fpix[256];
	unsigned long	*spix,*fpix;
	int		i,j,sorti,nfpix=0;

	if((xcb->visinfo->class == TrueColor) || (xcb->vtype == XcbBW))
		return;

	spix = _XcbStAlloc(sizeof(unsigned long)*npixels,stack_spix);
	fpix = _XcbStAlloc(sizeof(unsigned long)*npixels,stack_fpix);
	if(!spix || !fpix){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	memcpy(spix,pixels,sizeof(unsigned long)*npixels);
	qsort(spix,npixels,sizeof(unsigned long),ulcmp);

	_XcbDoPixSort(xcb);
	i=0;sorti=0;
	for(i=0;i<npixels;i++){
		while((sorti < xcb->ncsort) &&
					xcb->csort[sorti]->xcol.pixel < spix[i])
			sorti++;
		/*
		 * If we have no more nodes to search through, then the
		 * rest of the pixels in spix are invalid.
		 */
		if(sorti >= xcb->ncsort)
			break;

		node = xcb->csort[sorti];
		/*
		 * spix[i] is not an allocated pixel - skip it.
		 */
		if(node->xcol.pixel != spix[i])
			continue;

		if(xcb->parent && !xcb->my_cmap &&
				((node->alloc==XcbRO)||(node->alloc==XcbRW)))
			fpix[nfpix++] = spix[i];

		/*
		 * We have the node for the pixel we need to free.
		 * Decrease the ref count, and free the node if necessary.
		 */
		node->ref--;
		if(xcb->rmap){
			xcb->rmap[(node->xcol.pixel&xcb->masks.red) >>
						xcb->shifts.red].ref--;
			xcb->gmap[(node->xcol.pixel & xcb->masks.green) >>
						xcb->shifts.green].ref--;
			xcb->bmap[(node->xcol.pixel & xcb->masks.red) >>
						xcb->shifts.red].ref--;
		}

		if(node->ref < 1){
			NhlBoolean	rwfound=False;

			/*
			 * Add to the fpix list if it is an "allocated" pixel.
			 */
			if((!xcb->parent || xcb->my_cmap) &&
				((node->alloc==XcbRO)||(node->alloc==XcbRW)))
				fpix[nfpix++] = spix[i];
			else
				xcb->ncols--;

			/*
			 * Remove from sort.
			 */
			xcb->ncsort--;
			for(j=sorti;j<xcb->ncsort;j++)
				xcb->csort[j]=xcb->csort[j+1];

			/*
			 * Remove from RW list.
			 */
			for(j=0;j<xcb->nrwcells;j++)
				if(node == xcb->rwcells[j]){
					xcb->nrwcells--;
					rwfound=True;
					break;
				}
			for(;j<xcb->nrwcells;j++)
				xcb->rwcells[j]=xcb->rwcells[j+1];

			/*
			 * Remove from cube.
			 */
			if(!rwfound){
				_XcbCStat	*tnode;

				/* updates index's */
				(void)_XcbGetCubeNodeList(xcb,&node->xcol);
				tnode = &xcb->cube[xcb->icube];
				while(*tnode && (*tnode != node))
					tnode = &(*tnode)->next;
				if(*tnode)
					*tnode = (*tnode)->next;
			}

			if(!xcb->cstat)
				XFree(node);
			else{
				node->alloc = XcbNONE;
				node->ref = 0;
				node->cube = False;
				node->next = NULL;
			}
		}
	}

	if(nfpix > 0){
		if(xcb->parent && !xcb->my_cmap)
			XcbFreeColors(xcb->parent,fpix,nfpix);
		else{
			XFreeColors(xcb->dpy,xcb->cmap,fpix,nfpix,0);
			xcb->ncols -= nfpix;
		}
	}

	_XcbStFree(spix,stack_spix);
	_XcbStFree(fpix,stack_fpix);
}

void
XcbFreeColor(
	Xcb		xcb,
	unsigned long	pix
)
{
	XcbFreeColors(xcb,&pix,1);
	return;
}

NhlBoolean
XcbAllocRWColor(
	Xcb		xcb,
	unsigned long	*pix
)
{
	_XcbCStat	node=NULL;

	if(!xcb->rw)
		return False;

	if(xcb->my_cmap){
		node = _XcbFindNullRefNode(xcb);
		if(!node)
			return False;
		node->alloc = XcbMYCMAP;
	}
	else{
		unsigned long	newpix;
		unsigned long	dummy;
		NhlBoolean	valid=False;

		if(xcb->parent)
			valid = XcbAllocRWColor(xcb->parent,&newpix);
		else if(xcb->max_ncols && !(xcb->ncols < xcb->max_ncols))
			valid = False;
		else
			valid = XAllocColorCells(xcb->dpy,xcb->cmap,False,
							&dummy,0,&newpix,1);
		if(!valid){
			if(_XcbCFault(xcb))
				return XcbAllocRWColor(xcb,pix);
			return False;
		}

		node = _XcbNewNode(xcb,newpix);
		if(!node){
			if(xcb->parent)
				XcbFreeColor(xcb->parent,newpix);
			else
				XFreeColors(xcb->dpy,xcb->cmap,&newpix,1,0);
			return False;
		}
		node->alloc = XcbRW;
		if(!xcb->parent)
			xcb->ncols++;
	}

	node->ref = 1;
	if(xcb->rmap){
		xcb->rmap[(node->xcol.pixel & xcb->masks.red) >>
						xcb->shifts.red].ref = 1;
		xcb->gmap[(node->xcol.pixel & xcb->masks.green) >>
						xcb->shifts.green].ref = 1;
		xcb->bmap[(node->xcol.pixel & xcb->masks.red) >>
						xcb->shifts.red].ref = 1;
	}

	_XcbAddToRW(xcb,node);

	*pix = node->xcol.pixel;
	return True;
}

static void
TrueAlloc(
	Xcb		xcb,
	XColor		*col
)
{
	unsigned long	ired,igreen,iblue;
	unsigned long	vred,vgreen,vblue;

	vred = col->red >> (16 - xcb->bits.red);
	vgreen = col->green >> (16 - xcb->bits.green);
	vblue = col->blue >> (16 - xcb->bits.blue);

	ired = (vred << xcb->shifts.red) & xcb->masks.red;
	igreen = (vgreen << xcb->shifts.green) & xcb->masks.green;
	iblue = (vblue << xcb->shifts.blue) & xcb->masks.blue;

	col->red = (ired >> xcb->shifts.red) << (16 - xcb->bits.red);
	col->green = (igreen >> xcb->shifts.green) <<
						(16 - xcb->bits.green);
	col->blue = (iblue >> xcb->shifts.blue) <<
						(16 - xcb->bits.blue);
	col->pixel = ired|igreen|iblue;

	return;
}

static void
NoAlloc(
	Xcb		xcb,
	XColor		*col
)
{
	double	intensity;

	/*
	 * NTSC conversion.
	 */
	intensity = (double)col->red/0xffff * 0.3 +
			(double)col->green/0xffff * 0.59 +
			(double)col->blue/0xffff * 0.11;
	col->red=col->green=col->blue= (unsigned short)
						(intensity * 0xffff);
	if(intensity > 0.5){
		col->pixel = WhitePixel(xcb->dpy,xcb->scr);
		col->red=col->green=col->blue=0xffff;
	}
	else{
		col->pixel = BlackPixel(xcb->dpy,xcb->scr);
		col->red=col->green=col->blue=0;
	}

	return;
}

static void
BWAlloc(
	Xcb		xcb,
	XColor		*col
)
{
	double	intensity;

	/*
	 * NTSC conversion.
	 */
	intensity = (double)col->red/0xffff * 0.3 +
			(double)col->green/0xffff * 0.59 +
			(double)col->blue/0xffff * 0.11;
	col->red=col->green=col->blue= (unsigned short)
						(intensity * 0xffff);
	if(intensity > 0.5){
		col->pixel = xcb->white;
		col->red=col->green=col->blue=0xffff;
	}
	else{
		col->pixel = xcb->black;
		col->red=col->green=col->blue=0;
	}

	return;
}

static NhlBoolean
_XcbValidColor(
	Xcb		xcb,
	XColor		*req,
	XColor		*cmp,
	float		*ret
)
{
	float	maxerr,err;

	maxerr = (*xcb->cmpfunc)(req,NULL,xcb->cmp_data);
	err = (*xcb->cmpfunc)(req,cmp,xcb->cmp_data);
	if(ret)
		*ret = err;

	return ((maxerr == 0.0) || (err <= maxerr));
}

static NhlBoolean
GetBestIndex(
	_XcbDMStat	*list,
	int		start,
	int		size,
	unsigned short	val,
	unsigned long	*ret
)
{
	int	mid;

	if(size < 1)
		return False;

	if(list[start]->rw)
		return False;

	if(size == 1){
		*ret = list[start]->index;
		return True;
	}

	if(size==2){
		long	d1,d2;

		if(list[start+1]->rw){
			*ret = list[start]->index;
			return True;
		}
		d1 = list[start]->val - val;
		d2 = list[start+1]->val - val;
		if(d1 < 0) d1 = -d1;
		if(d2 < 0) d2 = -d2;
		if(d1 < d2)
			*ret = list[start]->index;
		else
			*ret = list[start+1]->index;
		return True;
	}

	mid = size/2;
	if(list[start+mid]->rw)
		return GetBestIndex(list,start,mid,val,ret);
	if(list[start+mid]->val > val)
		return GetBestIndex(list,start,mid+1,val,ret);
	return GetBestIndex(list,start+mid,mid+(size%2),val,ret);
}

/*
 * This is necessary because the DirectColor model allows for colors that
 * are not represented in the cube yet.
 */
static NhlBoolean
_XcbDirectClosestColor(
	Xcb		xcb,
	XColor		*req,
	XColor		*close
)
{
	NhlBoolean	dored,dogreen,doblue;
	unsigned long	ired,igreen,iblue;

	_XcbDoValSort(xcb);
	dored = GetBestIndex(xcb->rsort,0,xcb->rval+1,req->red,&ired);
	dogreen = GetBestIndex(xcb->gsort,0,xcb->gval+1,req->green,&igreen);
	doblue = GetBestIndex(xcb->bsort,0,xcb->bval+1,req->blue,&iblue);

	if(!dored || !dogreen || !doblue)
		return False;

	close->red = xcb->rmap[ired].val;
	close->green = xcb->gmap[igreen].val;
	close->blue = xcb->bmap[iblue].val;

	ired = (ired << xcb->shifts.red) & xcb->masks.red;
	igreen = (igreen << xcb->shifts.green) & xcb->masks.green;
	iblue = (iblue << xcb->shifts.blue) & xcb->masks.blue;

	close->pixel = (ired | igreen | iblue);
}

static _XcbCStat
CheckDiffs(
	Xcb		xcb,
	XColor		*col,
	unsigned int	rd,
	unsigned int	gd,
	unsigned int	bd
)
{
	_XcbCStat	new = NULL,list;
	int		i;
	int		ired,igreen,iblue;

	for(i=0;i<8;i++){
		/*
		 * red
		 */
		if(i%2){
			ired = xcb->ired - rd;
			if(ired < 0) continue;
		}
		else{
			if(rd==0) continue;
			ired = xcb->ired + rd;
			if(ired > xcb->mycube.red_max) continue;
		}
		/*
		 * green
		 */
		if((i/2)%2){
			igreen = xcb->igreen - gd;
			if(igreen < 0) continue;
		}
		else{
			if(gd==0) continue;
			igreen = xcb->igreen + gd;
			if(igreen > xcb->mycube.green_max) continue;
		}
		/*
		 * blue
		 */
		if((i/4)%2){
			iblue = xcb->iblue - bd;
			if(iblue < 0) continue;
		}
		else{
			if(bd==0) continue;
			iblue = xcb->iblue + bd;
			if(iblue > xcb->mycube.blue_max) continue;
		}

		ired *= xcb->mycube.red_mult;
		igreen *= xcb->mycube.green_mult;
		iblue *= xcb->mycube.blue_mult;

		list = xcb->cube[ired+igreen+iblue];
		while(list){
			float		cerr,berr;

			(void)_XcbValidColor(xcb,col,&list->xcol,&cerr);
			if(!new || (cerr < berr)){
				new = list;
				berr = cerr;
			}
			list = list->next;
		}
		if(new)
			return new;
	}

	return NULL;
}


static _XcbCStat
_XcbClosestColor(
	Xcb		xcb,
	XColor		*col
)
{
	_XcbCStat	new=NULL,list,*nptr;
	XColor		nc;
	unsigned int	i,j,D,M;
	
	list = _XcbGetCubeNodeList(xcb,col);
	if(xcb->my_cmap && (xcb->visinfo->class == DirectColor) &&
				_XcbDirectClosestColor(xcb,col,&nc)){
		if(new = FindDupNode(xcb,&nc))
			return new;
		new = _XcbNewNode(xcb,nc.pixel);
		if(!new)
			return NULL;
		new->xcol = nc;
		*col = nc;
		new->alloc = XcbMYCMAP;

		/*
		 * add node to cube.
		 */
		nptr = &xcb->cube[xcb->icube];
		if(((xcb->ired*xcb->rinc) == nc.red) &&
				(xcb->igreen*xcb->ginc == nc.green)&&
				(xcb->iblue*xcb->binc == nc.blue)){
			new->cube = True;
			new->next = *nptr;
			*nptr = new;
		}
		else{
			while(*nptr)
				nptr = &(*nptr)->next;
			*nptr = new;
		}

		return new;
	}

	new = NULL;
	while(list){
		float		cerr,berr;

		_XcbValidColor(xcb,col,&list->xcol,&cerr);
		if(!new || (cerr < berr)){
			new = list;
			berr = cerr;
		}
		list = list->next;
	}
	if(new)
		return new;

	/*
	 * Start searching out from the current cube node.
	 *
	 * M - Max distance to go out from node.
	 * D - Current Distance from node.
	 */
	M = MAX(xcb->ired,(xcb->mycube.red_max - xcb->ired));
	M = MAX(M,MAX(xcb->igreen,(xcb->mycube.green_max - xcb->igreen)));
	M = MAX(M,MAX(xcb->iblue,(xcb->mycube.blue_max - xcb->iblue)));

	for(D=1;D<=M;D++)
		for(i=0;i<=D;i++)
			for(j=i;j<=D;j++){
		if(new = CheckDiffs(xcb,col,i,j,D)) return new;
		if(i==D) continue;
		if(i==j){
			if(new = CheckDiffs(xcb,col,i,D,j)) return new;
			if(new = CheckDiffs(xcb,col,D,i,j)) return new;
		}
		else if(j==D){
			if(new = CheckDiffs(xcb,col,j,i,D)) return new;
			if(new = CheckDiffs(xcb,col,j,D,i)) return new;
		}
		else{
			if(new = CheckDiffs(xcb,col,i,D,j)) return new;
			if(new = CheckDiffs(xcb,col,j,i,D)) return new;
			if(new = CheckDiffs(xcb,col,j,D,i)) return new;
			if(new = CheckDiffs(xcb,col,D,i,j)) return new;
			if(new = CheckDiffs(xcb,col,D,j,i)) return new;
		}
	}

	return NULL;
}

static _XcbCStat
_XcbXAllocColor(
	Xcb		xcb,
	XColor		*col
)
{
	_XcbCStat	new=NULL;
	int		i;

	if(!xcb->rw || !xcb->my_cmap){
		if(!xcb->my_cmap && xcb->max_ncols &&
						!(xcb->ncols < xcb->max_ncols))
			return NULL;
		if(!XAllocColor(xcb->dpy,xcb->cmap,col))
			return NULL;
		if(new = FindDupNode(xcb,col)){
			XFreeColors(xcb->dpy,xcb->cmap,&col->pixel,1,0);
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,"Duplicate pix..."));
			return NULL;
		}
		new = _XcbNewNode(xcb,col->pixel);
		if(!new){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			XFreeColors(xcb->dpy,xcb->cmap,&col->pixel,1,0);
			return NULL;
		}

		xcb->ncols++;
		new->xcol = *col;
		new->alloc = XcbRO;
		return new;
	}

	if(xcb->visinfo->class == DirectColor){
		XColor		nc;
		NhlBoolean	nr,ng,nb;
		unsigned short	dr,dg,db;
		unsigned long	ir,ig,ib;

		/*
		 * First try and re-use color elements already allocated.
		 * Set n{r/g/b} (New) to true if we can't re-use the element.
		 */
		_XcbDoValSort(xcb);
		nr = !GetBestIndex(xcb->rsort,0,xcb->rval+1,col->red,&ir);
		ng = !GetBestIndex(xcb->gsort,0,xcb->gval+1,col->green,&ig);
		nb = !GetBestIndex(xcb->bsort,0,xcb->bval+1,col->blue,&ib);
		nc.flags = 0;
		if(nr){
			nc.red = col->red;
			nc.flags |= DoRed;
		}
		else
			nc.red = xcb->rmap[ir].val;
		if(ng){
			nc.green = col->green;
			nc.flags |= DoGreen;
		}
		else
			nc.green = xcb->gmap[ig].val;
		if(nb){
			nc.blue = col->blue;
			nc.flags |= DoBlue;
		}
		else
			nc.blue = xcb->bmap[ib].val;

		while((!nr||!ng||!nb) &&
					!_XcbValidColor(xcb,col,&nc,NULL)){
			long	dr,dg,db;

			if(nr)
				dr = 0;
			else
				dr = labs(nc.red-col->red);
			if(ng)
				dg = 0;
			else
				dg = labs(nc.green-col->green);
			if(nb)
				db = 0;
			else
				db = labs(nc.blue-col->blue);

			if((dg >= dr) && (dg >= db)){
				ng = True;
				nc.green = col->green;
				nc.flags |= DoGreen;
			}
			else if((dr >= dg) && (dr >= db)){
				nr = True;
				nc.red = col->red;
				nc.flags |= DoRed;
			}
			else{
				nb = True;
				nc.blue = col->blue;
				nc.flags |= DoBlue;
			}
		}

		/*
		 * Find empty color status element for "new" elements
		 * we have to allocate.
		 */
		if(nr||ng||nb)
			_XcbDoRefSort(xcb);
		if(nr && !GetNullIndex(xcb->rsort,xcb->rval+1,
				(xcb->parent)?(xcb->parent->rmap):(NULL),&ir))
			return NULL;
		if(ng && !GetNullIndex(xcb->gsort,xcb->gval+1,
				(xcb->parent)?(xcb->parent->gmap):(NULL),&ig))
			return NULL;
		if(nb && !GetNullIndex(xcb->bsort,xcb->bval+1,
				(xcb->parent)?(xcb->parent->bmap):(NULL),&ib))
			return NULL;
		ir = (ir << xcb->shifts.red) & xcb->masks.red;
		ig = (ig << xcb->shifts.green) & xcb->masks.green;
		ib = (ib << xcb->shifts.blue) & xcb->masks.blue;

		nc.pixel = (ir | ig | ib);
		if(new = FindDupNode(xcb,&nc)){
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,"Duplicate??"));
			return NULL;
		}
		new = _XcbNewNode(xcb,nc.pixel);
		if(!new)
			return NULL;
		new->xcol = nc;

	}
	else{

		new = _XcbFindNullRefNode(xcb);
		if(!new)
			return NULL;
		new->xcol.flags=DoRed|DoGreen|DoBlue;
		new->xcol.red = col->red;
		new->xcol.green = col->green;
		new->xcol.blue = col->blue;
	}

	XStoreColor(xcb->dpy,xcb->cmap,&new->xcol);
	new->alloc = XcbMYCMAP;
	return new;
}

static NhlBoolean
_XcbAllocROColor(
	Xcb		xcb,
	XColor		*col
);

static NhlBoolean
_XcbAllocROColorFromParent(
	Xcb		xcb,
	XColor		*col
)
{
	if(_XcbAllocROColor(xcb,col))
		return True;
	if(!xcb->min_ncols || (xcb->ncols >= xcb->min_ncols))
		return False;
	XcbAllocROColor(xcb,col);
	return True;
}

static NhlBoolean
_XcbAllocROColor(
	Xcb		xcb,
	XColor		*col
)
{
	NhlBoolean	newcolor = False;
	_XcbCStat	list,node=NULL,*nptr;
	XColor		mycol = *col;
	XColor		tcol;
	unsigned long	ired,igreen,iblue;
	NhlArgVal	sel,cbdata;

	if(xcb->parent && !xcb->my_cmap){
		if(_XcbAllocROColorFromParent(xcb->parent,&mycol)){
			node = FindDupNode(xcb,&mycol);
			if(!node){
				xcb->ncols++;
				node = _XcbNewNode(xcb,mycol.pixel);
				if(!node){
					NHLPERROR((NhlFATAL,ENOMEM,NULL));
					return False;
				}
				newcolor = True;
				node->alloc = XcbRO;
				node->xcol = mycol;
				nptr = &xcb->cube[xcb->icube];
				if(((xcb->ired*xcb->rinc) == mycol.red) &&
					(xcb->igreen*xcb->ginc == mycol.green)&&
					(xcb->iblue*xcb->binc == mycol.blue)){
					node->cube = True;
					node->next = *nptr;
					*nptr = node;
				}
				else{
					while(*nptr)
						nptr = &(*nptr)->next;
					*nptr = node;
				}
			}
			goto DONE;
		}
		return False;
	}

	/*
	 * Convert to NTSC before allocating colors or looking in grayscale
	 * cube.
	 */
	if(xcb->vtype == XcbGRAY){
		mycol.red = mycol.red*.3 + mycol.green*.59 + mycol.blue*.1;
		mycol.green = 0;
		mycol.blue = 0;
	}

	if(xcb->do_stdcmap){

		/*
		 * compute indexes for "mycol", then check stdcmap native
		 * values for those indexes, and determine if the stdcmap
		 * color is close enough to use.
		 */
		ired = mycol.red * (xcb->stdcmap.red_max + 1) / 0xffff;
		if(ired > xcb->stdcmap.red_max)
			ired = xcb->stdcmap.red_max;
		igreen = mycol.green * (xcb->stdcmap.green_max + 1) / 0xffff;
		if(igreen > xcb->stdcmap.green_max)
			igreen = xcb->stdcmap.green_max;
		iblue = mycol.blue * (xcb->stdcmap.blue_max + 1) / 0xffff;
		if(iblue > xcb->stdcmap.blue_max)
			iblue = xcb->stdcmap.blue_max;

		tcol.red = ired * xcb->rinc;
		tcol.green = igreen * xcb->ginc;
		tcol.blue = iblue * xcb->binc;
		tcol.pixel = (xcb->stdcmap.base_pixel +
				(ired * xcb->stdcmap.red_mult) +
				(igreen * xcb->stdcmap.green_mult) +
				(iblue * xcb->stdcmap.blue_mult)) & 0xffffffff;

		if(_XcbValidColor(xcb,&mycol,&tcol,NULL)){
			mycol = tcol;
			node = FindDupNode(xcb,&mycol);
			if(!node){
				node = _XcbNewNode(xcb,mycol.pixel);
				if(!node){
					NHLPERROR((NhlFATAL,ENOMEM,NULL));
					return False;
				}
				newcolor = True;
				node->alloc = XcbSTDCMAP;
				node->cube = True;
				node->xcol = mycol;
				list = _XcbGetCubeNodeList(xcb,&mycol);
				if(list->cube){
					NHLPERROR((NhlWARNING,NhlEUNKNOWN,
						"Duplicate Cube Nodes?!?!"));
				}
				node->next = list;
				xcb->cube[xcb->icube] = node;
			}
			goto DONE;
		}
	}

	list = _XcbGetCubeNodeList(xcb,col);
	if(!list || !list->cube){
		tcol.red = xcb->ired * xcb->rinc;
		tcol.green = xcb->igreen * xcb->ginc;
		tcol.blue = xcb->iblue * xcb->binc;
		if(_XcbValidColor(xcb,&mycol,&tcol,NULL) &&
					(node = _XcbXAllocColor(xcb,&tcol))){
			newcolor = True;
			node->cube = True;
			node->next =xcb->cube[xcb->icube];
			xcb->cube[xcb->icube] = node;
			goto DONE;
		}
	}

	node = NULL;
	while(list){
		float		cerr,berr;

		if(_XcbValidColor(xcb,&mycol,&list->xcol,&cerr)){
			if(!node || (cerr < berr)){
				node = list;
				berr = cerr;
			}
		}
		list = list->next;
	}
	if(node)
		goto DONE;

	node = _XcbXAllocColor(xcb,&mycol);
	if(!node)
		return False;

	newcolor = True;
	(void)_XcbGetCubeNodeList(xcb,&node->xcol);
	nptr = &xcb->cube[xcb->icube];
	if(((xcb->ired*xcb->rinc) == node->xcol.red) &&
		(xcb->igreen*xcb->ginc == node->xcol.green)&&
		(xcb->iblue*xcb->binc == node->xcol.blue)){
		node->cube = True;
		node->next = *nptr;
		*nptr = node;
	}
	else{
		while(*nptr)
			nptr = &(*nptr)->next;
		*nptr = node;
	}

DONE:
	node->ref++;
	if(xcb->rmap){
		xcb->rmap[(node->xcol.pixel&xcb->masks.red) >>
						xcb->shifts.red].ref++;
		xcb->gmap[(node->xcol.pixel&xcb->masks.green) >>
						xcb->shifts.green].ref++;
		xcb->bmap[(node->xcol.pixel&xcb->masks.blue) >>
						xcb->shifts.blue].ref++;
	}
	NhlINITVAR(sel);
	NhlINITVAR(cbdata);
	cbdata.ptrval = &node->xcol;
	_NhlCBCallCallbacks(xcb->allocCBL,sel,cbdata);
	*col = node->xcol;

	return True;
}

void
XcbAllocROColor(
	Xcb		xcb,
	XColor		*col
)
{
	NhlBoolean	valid;
	_XcbCStat	node=NULL,*nptr;
	float		maxerr;
	XColor		mycol;
	NhlArgVal	sel,cbdata;

	if(xcb->visinfo->class == TrueColor){
		TrueAlloc(xcb,col);
		return;
	}

	if(xcb->vtype == XcbBW){
		BWAlloc(xcb,col);
		return;
	}

	mycol = *col;

	if(xcb->parent && !xcb->my_cmap){
	
		if(xcb->mode == XcbSHAREDCMAP){
			XcbAllocROColor(xcb->parent,&mycol);
			valid = True;
		}
		else{
			valid = _XcbAllocROColorFromParent(xcb->parent,&mycol);
			if(valid && !_XcbValidColor(xcb,col,&mycol,NULL)){
				XcbFreeColor(xcb->parent,mycol.pixel);
				valid = False;
			}
			if(!valid){
				(void)_XcbCFault(xcb);
				XcbAllocROColor(xcb,col);
				return;
			}
		}

		node = FindDupNode(xcb,&mycol);
		if(!node){
			xcb->ncols++;
			node = _XcbNewNode(xcb,mycol.pixel);
			if(!node){
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return;
			}
			node->alloc = XcbRO;
			node->xcol = mycol;
			(void)_XcbGetCubeNodeList(xcb,&mycol);
			nptr = &xcb->cube[xcb->icube];

			/*
			 * If cube node, add to front of list.
			 * else add to end.
			 */
			if(((xcb->ired*xcb->rinc) == mycol.red) &&
				(xcb->igreen*xcb->ginc == mycol.green) &&
				(xcb->iblue*xcb->binc == mycol.blue)){
				node->cube = True;
				node->next = *nptr;
				*nptr = node;
			}
			else{
				while(*nptr)
					nptr = &(*nptr)->next;
				*nptr = node;
			}
			NhlINITVAR(sel);
			NhlINITVAR(cbdata);
			cbdata.ptrval = &node->xcol;
			_NhlCBCallCallbacks(xcb->allocCBL,sel,cbdata);
		}
		node->ref++;
		if(xcb->rmap){
			xcb->rmap[(node->xcol.pixel&xcb->masks.red) >>
						xcb->shifts.red].ref++;
			xcb->gmap[(node->xcol.pixel&xcb->masks.green) >>
						xcb->shifts.green].ref++;
			xcb->bmap[(node->xcol.pixel&xcb->masks.blue) >>
						xcb->shifts.blue].ref++;
		}
		*col = node->xcol;
		return;
	}

	valid = _XcbAllocROColor(xcb,&mycol);
	if(valid && !_XcbValidColor(xcb,col,&mycol,NULL)){
		XcbFreeColor(xcb->parent,mycol.pixel);
		valid = False;
	}
	if(!valid){
		if(_XcbCFault(xcb)){
			XcbAllocROColor(xcb,col);
			return;
		}
		node = _XcbClosestColor(xcb,col);
		if(!node){
			NoAlloc(xcb,col);
			return;
		}
		node->ref++;
		if(xcb->rmap){
			xcb->rmap[(node->xcol.pixel&xcb->masks.red) >>
						xcb->shifts.red].ref++;
			xcb->gmap[(node->xcol.pixel&xcb->masks.green) >>
						xcb->shifts.green].ref++;
			xcb->bmap[(node->xcol.pixel&xcb->masks.blue) >>
						xcb->shifts.blue].ref++;
		}
		*col = node->xcol;
		return;
	}

	*col = mycol;

	return;
}

void
XcbAllocCloseROColor(
	Xcb		xcb,
	XColor		*col
)
{
	XColor		nc;

	if((xcb->vtype == XcbBW) || (xcb->visinfo->class == TrueColor))
		nc = *col;
	else{
		(void)_XcbGetCubeNodeList(xcb,col);
		nc.red = xcb->ired * xcb->rinc;
		nc.green = xcb->igreen * xcb->ginc;
		nc.blue = xcb->iblue * xcb->binc;
	}
	XcbAllocROColor(xcb,&nc);
	*col = nc;

	return;
}

static Boolean
_XcbAllocNamedColor(
	Xcb		xcb,
	String		name,
	XColor		*col,
	Boolean		close
)
{
	XColor		nc;
	String		msg,str,params[1];
	Cardinal	num_params=1;

	if(!name)
		return False;
	else if(strcasecmp(name,XtDefaultBackground) == 0)
		str = "white";
	else if(strcasecmp(name,XtDefaultForeground) == 0)
		str = "black";
	else
		str = name;

	if(!XParseColor(xcb->dpy,xcb->cmap,str,&nc)){
		params[0] = str;
		msg = "Color name \"%s\" is not defined";
		XtAppWarningMsg(XtDisplayToApplicationContext(xcb->dpy),
			"badValue","XcbAllocNamedColor",
			"XcbError",msg,params,&num_params);
		return False;
	}
	if(close)
		XcbAllocCloseROColor(xcb,&nc);
	else
		XcbAllocROColor(xcb,&nc);

	*col = nc;

	return True;
}

Boolean
XcbAllocCloseNamedColor(
	Xcb		xcb,
	String		name,
	XColor		*col
)
{
	return _XcbAllocNamedColor(xcb,name,col,True);
}

Boolean
XcbAllocNamedColor(
	Xcb		xcb,
	String		name,
	XColor		*col
)
{
	return _XcbAllocNamedColor(xcb,name,col,False);
}

Visual
*XcbGetVisual(
	Xcb	xcb
)
{
	if(xcb)
		return xcb->visinfo->visual;
	return NULL;
}

Colormap
XcbGetColormap(
	Xcb	xcb
)
{
	if(xcb)
		return xcb->cmap;
	return None;
}

unsigned int
XcbGetDepth(
	Xcb	xcb
)
{
	if(xcb)
		return xcb->visinfo->depth;
	return 0;
}
