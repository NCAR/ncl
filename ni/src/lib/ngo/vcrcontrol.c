/*
 *      $Id: vcrcontrol.c,v 1.9 2000-03-21 02:35:55 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		vcrcontrol.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 11 11:53:31 MST 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/vcrcontrolP.h>
#include <ncarg/ngo/xutil.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Form.h>
#include  <Xm/DrawnBP.h>
#include  <ncarg/ngo/XmL.h>
#include <float.h>

NhlErrorTypes NgUpdateVcrControl
(
        NgVcrControl		*vcr_control
        )
{
        NgVcrControlRec *vcrp;
        
        vcrp = (NgVcrControlRec *) vcr_control;
        if (!vcrp) return NhlFATAL;
        
        return NhlNOERROR;
}

static Widget CreateDrawnButton
(
        NgVcrControlRec *vcrp,
        char *name,
        int pos,
        int type,
        NhlBoolean forward
        )
{
        Widget ret = NULL;
        int dir;
        char *prev_attach,*next_attach,*prev_pos,*next_pos;
        int npos;

        if (vcrp->horizontal) {
                prev_attach = XmNleftAttachment;
                prev_pos = XmNleftPosition;
                next_attach = XmNrightAttachment;
                next_pos = XmNrightPosition;
                dir = forward ? XmDRAWNB_RIGHT : XmDRAWNB_LEFT;
                npos = pos + 1;
        }
        else {
                prev_attach = XmNbottomAttachment;
                next_attach = XmNtopAttachment;
                prev_pos = XmNbottomPosition;
                next_pos = XmNtopPosition;
                dir = forward ? XmDRAWNB_UP : XmDRAWNB_DOWN;
                npos = pos - 1;
        }

        ret = XtVaCreateManagedWidget(name,xmDrawnButtonWidgetClass,
                                      vcrp->public.form,
                                      prev_attach,XmATTACH_POSITION,
                                      next_attach,XmATTACH_POSITION,
                                      prev_pos,pos,
                                      next_pos,npos,
                                      XmNheight,vcrp->size,
                                      XmNwidth,vcrp->size,
				      XmNpushButtonEnabled,True,
				      XmNsensitive,True,
				      XmNtraversalOn,True,
                                      NULL);

        if (ret) {
                XmLDrawnButtonSetType(ret,type,dir);
        }
	_NgGOWidgetTranslations(vcrp->go,ret);
	XmProcessTraversal(ret, XmTRAVERSE_CURRENT);

        return ret;
}
        
        
NgVcrControl *NgCreateVcrControl
(
	int			go_id,
        NhlString		name,
        Widget			parent,
        Dimension		size,
        NhlBoolean		horizontal,
        NhlBoolean		begin,
        NhlBoolean		fast_reverse,
        NhlBoolean		reverse,
        NhlBoolean		start_stop,
        NhlBoolean		forward,
        NhlBoolean		fast_forward,
        NhlBoolean		end
        )
{
	NgGO	go = (NgGO) _NhlGetLayer(go_id);
        NgVcrControlRec *vcrp;
	NgVcrControl *vcr;
        int pos,count = 0,ix = 0;

	if (! go)
		return NULL;

        if (begin) count++;
        if (fast_reverse) count++;
        if (reverse) count++;
        if (start_stop) count++;
        if (forward) count++;
        if (fast_forward) count++;
        if (end) count++;
        
        vcrp = NhlMalloc(sizeof(NgVcrControlRec));
        if (!vcrp) return NULL;

	vcr = &vcrp->public;
        vcrp->go = go;
        vcrp->horizontal = horizontal;
        vcrp->size = size;

        vcr->begin = vcr->fast_reverse = vcr->reverse =
                vcr->start_stop =
                vcr->forward = vcr->fast_forward = vcr->end = NULL;
        
        vcr->form = XtVaCreateManagedWidget(name,xmFormWidgetClass,parent,
                                             XmNfractionBase,count,
                                             NULL);

        if (begin) {
                pos = horizontal ? ix : count - ix;
                vcr->begin =
                        CreateDrawnButton(vcrp,"Begin",
                                          pos,XmDRAWNB_ARROWLINE,False);
                ix++;
        }
        if (fast_reverse) {
                pos = horizontal ? ix : count - ix;
                vcr->fast_reverse =
                        CreateDrawnButton(vcrp,"Fast_Reverse",
                                          pos,XmDRAWNB_DOUBLEARROW,False);
                ix++;
        }
        if (reverse) {
                pos = horizontal ? ix : count - ix;
                vcr->reverse =
                        CreateDrawnButton(vcrp,"Reverse",
                                          pos,XmDRAWNB_ARROW,False);
                ix++;
        }
        if (start_stop) {
                pos = horizontal ? ix : count - ix;
                vcr->start_stop =
                        CreateDrawnButton(vcrp,"Start_Stop",
                                          pos,XmDRAWNB_SQUARE,False);
                ix++;
        }
        if (forward) {
                pos = horizontal ? ix : count - ix;
                vcr->forward =
                        CreateDrawnButton(vcrp,
                                          "Forward",
                                          pos,XmDRAWNB_ARROW,True);
                ix++;
        }
        if (fast_forward) {
                pos = horizontal ? ix : count - ix;
                vcr->fast_forward =
                        CreateDrawnButton(vcrp,"Fast_Forward",
                                          pos,XmDRAWNB_DOUBLEARROW,True);
                ix++;
        }
        if (end) {
                pos = horizontal ? ix : count - ix;
                vcr->end =
                        CreateDrawnButton(vcrp,"End",
                                          pos,XmDRAWNB_ARROWLINE,True);
        }
        
	_NgGOWidgetTranslations(vcrp->go,vcr->form);

        return (NgVcrControl *) vcrp;
}

void NgDestroyVcrControl
(
        NgVcrControl		*vcr_control
        )
{
        NgVcrControlRec *vcrp;
        
        vcrp = (NgVcrControlRec *) vcr_control;
        if (!vcrp) return;

        NhlFree(vcrp);

        return;
}

        


