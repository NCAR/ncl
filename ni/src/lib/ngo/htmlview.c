/*
 *      $Id: htmlview.c,v 1.9 1998-09-24 19:54:07 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		htmlview.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Nov  3 17:35:23 MST 1997
 *
 *	Description:	
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include  <Xm/ScrolledW.h>
#include  <Xm/ScrollBar.h>
#include <stdio.h>
#if 0
#include <XmHTML/debug.h>
#endif

#include <ncarg/ngo/htmlviewP.h>
#include <ncarg/ngo/nclstate.h>

#if 0
#define DOCTOP "ngdoc/ng4.0/"
#else 
#define DOCTOP "ngdoc/ng4.1/"
#endif

#define DOCHLURESDIR "ref/hlu/obj/"
#define DOCRESEXT ".res.html"
#define MAX_HTML_OBJECT_LIST_LEN 10
#define NULL_HTMLVIEW_ID 0

static _hvHtmlObject *Html_Object_List = NULL;
static HtmlViewId HtmlViewIdValue = 0;
static NrmQuark QerrorNotFound = NrmNULLQUARK;
static Widget Cache_Html_Widget = NULL;

static char Error_Content[] = 
	"<html><body>Description not available.</body></html>";

static void
swvsbCB(Widget w, XtPointer udata, XtPointer data)
{
        NgHtmlViewRec	*hvp = (NgHtmlViewRec	*) udata;
        int val,slide,inc,page_inc,max;
#if DEBUG_HTML        
	fprintf(stderr,"outvsbCB!\n");
#endif
        XmScrollBarGetValues(hvp->swvsb,&val,&slide,&inc,&page_inc);
        _XmHTMLMoveToPos(hvp->vsb,(XmHTMLWidget)hvp->html,val);
}

static int HtmlObjectListSize
(
        _hvHtmlObject *hobject_list
        )
{
        _hvHtmlObject *hop;
        int count = 0;
        
        for (hop = hobject_list; hop != NULL; hop = hop->next) 
        {
                XmHTMLObject *elp;
                XmHTMLObjectTable *nanp;
                XmHTMLAnchor *andp;
                XmHTMLObjectTable *fop;

                count += sizeof(_hvHtmlObject);
                count += hop->anchor_words * sizeof(XmHTMLWord);
                for (elp = hop->elements; elp != NULL; elp = elp->next) {
                        count += sizeof(XmHTMLObject);
                        count += 2 + strlen(elp->element) +
                                (elp->attributes ? strlen(elp->attributes):1);
                }
                count += (1+hop->num_named_anchors)*sizeof(XmHTMLObjectTable);
                for (andp = hop->anchor_data; andp != NULL; andp = andp->next){
                        count += sizeof(XmHTMLAnchor);
                        if (andp->href)
                                count += strlen(andp->href);
                        if (andp->name)
                                count += strlen(andp->name);
                        if (andp->target)
                                count += strlen(andp->target);
                        if (andp->rel)
                                count += strlen(andp->rel);
                        if (andp->rev)
                                count += strlen(andp->rev);
                        if (andp->title)
                                count += strlen(andp->title);
                }
                for (fop = hop->formatted; fop != NULL; fop = fop->next){
                        count += sizeof(XmHTMLObjectTable);
                        if(fop->text)
                                count += strlen(fop->text) + 1;
                        if (fop->n_words) {
                                count += strlen(fop->words[0].word);
                                count += fop->n_words * sizeof(XmHTMLWord *);
                        }
                }
        }
        return count;
}

void _XmHTMLClearWidget
(
        XmHTMLWidget html
        )
{
        html->html.anchors = NULL;
        html->html.elements = NULL;
        html->html.named_anchors = NULL;
        html->html.anchor_data = NULL;
        html->html.formatted = NULL;
	html->html.images = NULL;
        html->html.num_named_anchors = 0;
        html->html.anchor_words = 0;
        if (html->html.value)
                free(html->html.value);
        html->html.value = "1";
        XmHTMLTextSetString((Widget)html,NULL);
        html->html.value = NULL;
        return;
}

void _XmHTMLSetWidget
(
        XmHTMLWidget html,
        _hvHtmlObject *hobject
        )
{
        html->html.anchors = hobject->anchors;
        html->html.elements = hobject->elements;
        html->html.named_anchors = hobject->named_anchors;
        html->html.anchor_data = hobject->anchor_data;
        html->html.formatted = hobject->formatted;
	html->html.images = hobject->images;
        html->html.num_named_anchors = hobject->num_named_anchors;
        html->html.anchor_words = hobject->anchor_words;
        
        return;
}

void _XmHTMLSetObject
(
        XmHTMLWidget html,
        _hvHtmlObject *hobject
        )
{
        hobject->anchors = html->html.anchors;
        hobject->elements = html->html.elements;
        hobject->named_anchors = html->html.named_anchors;
        hobject->anchor_data = html->html.anchor_data;
        hobject->formatted = html->html.formatted;
        hobject->images = html->html.images;
        hobject->num_named_anchors = html->html.num_named_anchors;
        hobject->anchor_words = html->html.anchor_words;
        
        return;
}

static _hvHtmlObject *
FindObjectInObjectList(
        _hvHtmlObject	*hobject,
        NrmQuark	locator
        )
{
        _hvHtmlObject	*ho;

        for (ho = hobject; ho != NULL; ho = ho->next) {
                if (ho->locator == locator)
                        return ho;
        }
        return NULL;
}

static String GetContent(
        NgHtmlViewRec	*hvp,
        NrmQuark	locator
        )
{
        String url;
        char filename[64];
        char sbuf[1024];
        char *content;
        char *cp;
        FILE *fp;
        int count,html_len = -1;

        if (locator == QerrorNotFound) {
                content = NhlMalloc(strlen(Error_Content)+1);
                strcpy(content,Error_Content);
                return content;
        }
	if (! hvp->filebase) {
		hvp->filebase = getenv("NCARG_DOC_ROOT");
	}
        if (! hvp->filebase && ! hvp->urlbase)
                hvp->urlbase = (char *)_NGGetNCARGEnv("ngurl");
	if (! hvp->filebase && ! hvp->urlbase) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "No path to documentation"));
		return NULL;
	}

        sprintf(filename,"%s",NrmQuarkToString(locator));
        cp = strstr(filename,"Class");
        if (cp)
                *cp = '\0';
        filename[0] = toupper(filename[0]);
        if (!strcmp(filename,"PsWorkstation"))
                filename[1] = 'S';
#if 0        
        else if(!strcmp(filename,"Transformation"))
                sprintf(filename,"TransObj");
        else if(!strcmp(filename,"MapTransformation"))
                sprintf(filename,"MapTransObj");
        else if(!strcmp(filename,"IrregularTransformation"))
                sprintf(filename,"IrregularTransObj");
        else if(!strcmp(filename,"LogLinTransformation"))
                sprintf(filename,"LogLinTransObj");
#endif        

        if (! hvp->filebase) {
		/* contruct a command to get the doc for this resource */


#if DEBUG_HTML        
		sprintf(sbuf,"wget -T 15 -t 3 -s -S -O - '%s%s%s%s%s'",
			hvp->urlbase,DOCTOP,DOCHLURESDIR,filename,DOCRESEXT);
		fprintf(stderr,"%s\n",sbuf);
#else
		sprintf(sbuf,"wget -q -T 15 -t 3 -s -S -O - '%s%s%s%s%s'",
			hvp->urlbase,DOCTOP,DOCHLURESDIR,filename,DOCRESEXT);
#endif
		fp = popen(sbuf,"r");

		count = 0;
		while (1) {
			if (fgets(sbuf,1024,fp) != NULL) {
				if (!strncasecmp("content-length:",sbuf,15)) {
                                	sscanf(&sbuf[15],"%d",&html_len);
					break;
				}
			}
			count++;
			if (count == 20) {
			  NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				     "Error retrieving %s doc\n",
				     NrmQuarkToString(locator)));
			  return NULL;
			}
		}
		if (html_len <= 0) {
		  NHLPERROR((NhlFATAL,ENOMEM,"Error retrieving %s doc\n",
			     NrmQuarkToString(locator)));
		  return NULL;
		}
#if DEBUG_HTML        
		fprintf(stderr,"retrieving %d bytes\n",html_len);
#endif
	}
	else {
		struct stat statbuf;

		sprintf(sbuf,"%s%s%s%s",
			hvp->filebase,DOCHLURESDIR,filename,DOCRESEXT);
#if DEBUG_HTML        
		fprintf(stderr,"%s\n",sbuf);
#endif
		if (stat(sbuf,&statbuf) || ! S_ISREG(statbuf.st_mode)) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "No path to documentation"));
			return NULL;
		}
		fp = fopen(sbuf,"r");
		if (! fp) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "Error opening %s doc\n",
				   NrmQuarkToString(locator)));
			return NULL;
		}
		html_len = statbuf.st_size;
	}
        content = NhlMalloc(html_len +1);
	if (! content) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
                return NULL;
        }
	count = fread(content,1,html_len,fp);
	pclose(fp);
        content[html_len] = '\0';

	if (count < html_len) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "Incomplete retrieval of %s doc\n",
			   NrmQuarkToString(locator)));
                NhlFree(content);
                return NULL;
        }
	return content;
}

static _hvHtmlObject *
GetHtmlObject(
        NgHtmlViewRec	*hvp,
        NrmQuark	locator
        )
{
        char	filename[64];
        char	buf[1024];
        int	count,html_len;
        char	*content;
        FILE	*fp;
        char	*cp;
	_hvHtmlObject *hobject;
        XmHTMLWidget	xmhtml = (XmHTMLWidget) hvp->html;

        _XmHTMLClearWidget(xmhtml);
        hobject = FindObjectInObjectList(Html_Object_List,locator);

        if (hobject) {
                _XmHTMLSetWidget(xmhtml,hobject);
                _XmHTMLComputeLayout(xmhtml);
                hvp->public.locator = hobject->locator = locator;
                return hobject;
        }
        content = GetContent(hvp,locator);

	if (! content) {
                hobject = FindObjectInObjectList
                        (Html_Object_List,QerrorNotFound);
                if (hobject)
                        return hobject;
		content = GetContent(hvp,QerrorNotFound);
                locator = QerrorNotFound;
                if (! content) {
                        NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NULL;
		}
        }

	xmhtml->html.elements = _XmHTMLparseHTML(xmhtml,NULL,content,NULL);
	if (! xmhtml->html.elements) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"XmHTML parsing error"));
                return NULL;
        }
	_XmHTMLformatObjects(xmhtml,xmhtml);
	if (! xmhtml->html.formatted) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"XmHTML formatting error"));
                return NULL;
        }
	hobject = NhlMalloc(sizeof(_hvHtmlObject));
	if (! hobject) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
                return NULL;
        }
        
        hvp->public.locator = hobject->locator = locator;
	_XmHTMLSetObject(xmhtml,hobject);
	hobject->next = Html_Object_List;
	Html_Object_List = hobject;
        NhlFree(content);
        
        _XmHTMLComputeLayout(xmhtml);

        return hobject;
}
static XmHTMLObjectTableElement
FindEndOfResource
(
        XmHTMLObjectTableElement begin
        )
{
        XmHTMLObjectTableElement el;
        NhlBoolean started = False;
        int level = 0;

/*
 * look for an opening and closing HT_DD at level 0
 */
        for (el = begin; el != NULL; el = el->next) {
                if (el->object->id == HT_DD && level == 0 &&
                    el->object->terminated && !el->object->is_end) {
                        started = True;
                        continue;
                }
                if (el->object->id == HT_DD && started && level == 0 &&
                    el->object->is_end)
                        return el;
                if (el->object->id == HT_DL) {
                        if (! el->object->is_end && el->object->terminated)
                                level++;
                        else if (el->object->is_end && el->object->terminated)
                                level--;
                }
        }
        return NULL;
}

/*
 * Interface routines used by the browser only (at least for now)
 */
static void SetErrorNotice
(
        NgHtmlViewRec *hvp
        )
{
        Dimension form_width;
        
        XtVaSetValues(hvp->public.htmlview,
                      XmNheight,39,
                      NULL);
        XtVaSetValues(hvp->swvsb,
                      XmNmaximum,20,
                      XmNminimum,0,
                      XmNvalue,0,
                      XmNsliderSize,20,
                      NULL);
        XtVaSetValues(hvp->scrwin,
                      XmNleftOffset,0,
                      XmNtopOffset,0,
                      NULL);
        XtVaGetValues(hvp->public.htmlview,
                      XmNwidth,&form_width,
                      NULL);
        hvp->public.height = 39;
        hvp->public.width = form_width;
        return;
}


NhlErrorTypes
_NgSetHtmlContent(
        NgHtmlView	*htmlview,
        NrmQuark	locator,
        NrmQuark	name
        )
{
        NgHtmlViewRec *hvp = (NgHtmlViewRec *) htmlview;
        _hvHtmlObject *hobject;
        char	buf[1024];
        char		*cp;
        NhlBoolean	found_hr = False;
        int		max,min;
        Dimension	form_width,lmax_width = 0;
        Dimension height,form_height,scroll_height,work_height,html_height;
        XmHTMLObjectTableElement	begin,end,obj,objp,hr_objp;
        XmHTMLWidget			xmhtml = (XmHTMLWidget) hvp->html;
        static NhlBoolean first = True;
        NrmQuark error_ret;

        if (! htmlview->managed) {
                XtManageChild(htmlview->htmlview);
                htmlview->managed = True;
        }
        if (locator != htmlview->locator) {
                hobject = GetHtmlObject(hvp,locator);
                if (!hobject)
                        return NhlFATAL;
        }
        
        if (htmlview->locator != QerrorNotFound) {
                sprintf(buf,"#%s",NrmQuarkToString(name));
                begin = _XmHTMLGetAnchorByName(xmhtml,buf);
        }
        if (! begin) {
                NHLPERROR((NhlINFO,NhlEUNKNOWN,
                           "No description found in reference documentation"));
                hobject = GetHtmlObject(hvp,QerrorNotFound);
                if (!hobject)
                        return NhlFATAL;
        }
        if (htmlview->locator == QerrorNotFound) {
                SetErrorNotice(hvp);
                return NhlINFO;
        }
        else {
                end = FindEndOfResource(begin);
                if (first) {
                        end = FindEndOfResource(begin);
                        first = False;
                }
        }
        htmlview->name = name;

        XtVaGetValues(hvp->public.htmlview,
                      XmNheight,&form_height,
                      NULL);
        XtVaGetValues(hvp->scrwin,
                      XmNheight,&scroll_height,
                      NULL);
        XtVaGetValues(hvp->work,
                      XmNheight,&work_height,
                      NULL);
        XtVaGetValues(hvp->html,
                      XmNheight,&html_height,
                      NULL);

        if (end) {
                height = -10 + form_height - scroll_height +
                        end->y - begin->y - begin->height;
                work_height = 5 + end->y - begin->y - begin->height;
        }
        else {
                height = form_height - scroll_height + work_height;
        }
        
#if DEBUG_HTML        
        fprintf(stderr,"form %d scroll %d html %d desired html height %d\n",
               form_height,scroll_height,html_height,work_height);
#endif

        height = height < 1000 ? height : 1000;
        
        XtVaSetValues(hvp->public.htmlview,
                      XmNheight,height,
                      NULL);
        XtVaSetValues(hvp->html,
                      XmNheight,work_height,
                      NULL);
/*
 * hack to ensure that the whole work window gets cleared; otherwise there's
 * a core dump if a CopyArea is attempted befor there is a graphic context.
 */
	if (hvp->not_yet_mapped) {
                xmhtml->html.scroll_y = -60000;
		hvp->not_yet_mapped = False;
	}
        _XmHTMLMoveToPos(hvp->vsb,xmhtml,begin->y);

        XtVaSetValues(hvp->scrwin,
                      XmNleftOffset,-begin->next->ident,
                      XmNtopOffset,-5,
                      XmNbottomOffset,-15,
                      NULL);

        {
        int val,slide,inc,page_inc,max;
        
        XmScrollBarGetValues(hvp->vsb,&val,&slide,&inc,&page_inc);
#if DEBUG_HTML        
        fprintf(stderr,"vsb - val %d slide %d inc %d page_inc %d\n",
               val,slide,inc,page_inc);
#endif

        XmScrollBarGetValues(hvp->swvsb,&val,&slide,&inc,&page_inc);
#if DEBUG_HTML        
	fprintf(stderr,"outvsb - val %d slide %d inc %d page_inc %d\n",
               val,slide,inc,page_inc);
#endif
        }
        XtVaGetValues(hvp->public.htmlview,
                      XmNheight,&form_height,
                      NULL);
        XtVaGetValues(hvp->scrwin,
                      XmNheight,&scroll_height,
                      NULL);
        XtVaGetValues(hvp->work,
                      XmNheight,&height,
                      NULL);
        XtVaGetValues(hvp->html,
                      XmNheight,&html_height,
                      NULL);
#if DEBUG_HTML        
        fprintf(stderr,"form %d scroll %d html %d work height %d\n",
               form_height,scroll_height,html_height,height);
#endif
        XtVaSetValues(hvp->swvsb,
                      XmNmaximum,work_height + begin->y,
                      XmNminimum,begin->y,
                      XmNvalue,begin->y,
                      XmNsliderSize,work_height,
                      NULL);
        XtVaGetValues(hvp->swvsb,
                      XmNmaximum,&max,
                      XmNminimum,&min,
                      NULL);
#if DEBUG_HTML        
        fprintf(stderr,"scroll max %d min %d\n",max,min);
#endif
        if (scroll_height >= work_height)
             XmScrollBarSetValues
                     (hvp->swvsb,min,max-min,1,max-min,False);
        else {
                int slide = ((float)scroll_height / (float)work_height)
                        * (max-min);
                XmScrollBarSetValues
                        (hvp->swvsb,min,slide,(int)(slide * .1),slide,False);
        }
        XtVaGetValues(hvp->public.htmlview,
                      XmNheight,&form_height,
                      XmNwidth,&form_width,
                      NULL);

        hvp->public.height = form_height;
        hvp->public.width = form_width;

#if DEBUG_HTML        
        fprintf(stderr,"size of html object list is %d\n",
               HtmlObjectListSize(Html_Object_List));
#endif
        
        return NhlNOERROR;

}

static NhlErrorTypes
SetHtmlLocation(
        NgHtmlView	*htmlview,
        brPane		*pane,
        Widget		requestor,
        Position	x,
        Position	y
        )
{
        NgHtmlViewRec *hvp = (NgHtmlViewRec *) htmlview;
        Position pane_x,pane_y,req_x,req_y,html_x,html_y;

/*
 * The requestor widget is the pane descendent
 * for whom the x and y is relative.
 */
        XtTranslateCoords(pane->form,(Position) 0,(Position) 0,
                          &pane_x,&pane_y);
        XtTranslateCoords(requestor,(Position) 0,(Position) 0,
                          &req_x,&req_y);

        htmlview->x = req_x - pane_x + x;
        htmlview->y = req_y - pane_y + y;
        
        XtVaSetValues(htmlview->htmlview,
                      XmNleftOffset,htmlview->x,
                      XmNtopOffset,htmlview->y,
                      NULL);
        return NhlNOERROR;
}


NhlErrorTypes
_NgGetHtmlViewSize(
        NgHtmlView	*htmlview,
        Dimension	max_height,
        Dimension	avail_width,
        NhlBoolean	resize_width,
        Dimension	*height,
        Dimension	*width
        )
{
        *height = htmlview->height;
        *width = htmlview->width;
        
        return NhlNOERROR;
}

        
void
_NgMapHtmlView(
        NgHtmlView	*htmlview
        )
{
#if DEBUG_HTML        
        fprintf(stderr,"_NgMapHtmlView IN\n");
#endif
        if (! htmlview->mapped) {
                XtMapWidget(htmlview->htmlview);
                htmlview->mapped = True;
        }
        return;
}

void
_NgUnmapHtmlView(
        NgHtmlView	*htmlview
        )
{
#if DEBUG_HTML        
        fprintf(stderr,"in _NgUnmapHtmlView\n");
#endif
        if (htmlview->mapped) {
                XtUnmapWidget(htmlview->htmlview);
                htmlview->mapped = False;
        }
        if (htmlview->managed) {
                XtUnmanageChild(htmlview->htmlview);
                htmlview->managed = False;
        }
        return;
}

void
_NgDestroyHtmlView(
        NgHtmlView	*htmlview
        )
{
        NgHtmlViewRec	*hvp = (NgHtmlViewRec *)htmlview;
        
#if DEBUG_HTML        
        fprintf(stderr,"in _NgDestroyHtmlView\n");
#endif

        _XmHTMLClearWidget((XmHTMLWidget) hvp->html);
        XtDestroyWidget(htmlview->htmlview);
        NhlFree(htmlview);
        
        return;
        
}
NgHtmlView *
_NgCreateHtmlView(
        int		goid,
        Widget		parent,
        NgHtmlViewType	type
        )
{
        NgHtmlViewRec	*hvp;
        NgHtmlView	*pub_hvp;
        Widget		hsb,form;


/*
 * This code wraps the XmHTML widget with a form in order to allow a specific
 * block of text from an html file to be displayed without scrolling to
 * other parts of the file.
 */
 
#if DEBUG_HTML        
        fprintf(stderr,"in _NgCreateHtmlView\n");
#endif

        if (QerrorNotFound == NrmNULLQUARK) {
                QerrorNotFound = NrmStringToQuark("NgHtmlErrorNotFound");
        }
        if (! Cache_Html_Widget) {
              Cache_Html_Widget = XtVaCreateWidget
                ("Cache_Html_Widget",
                 xmHTMLWidgetClass,parent,
                 XmNmarginWidth, 0,
                 XmNmarginHeight, 0,
                 XmNwidth, 500,
                 XmNheight, 500,
		 XmNimageEnable,False,
                 NULL);
#if DEBUG_HTML	      
	      fprintf(stderr, "%s, %i\n",
		      XmHTMLVERSION_STRING, XmHTMLGetVersion());
#endif

        }
        
        hvp = NhlMalloc(sizeof(NgHtmlViewRec));
        if (! hvp) {
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                return NULL;
        }
        hvp->go = (NgGO)_NhlGetLayer(goid);
        hvp->parent = parent;
        hvp->type = type;
        hvp->cur_locator = NrmNULLQUARK;
        hvp->urlbase = NULL;
	hvp->filebase = NULL;
        pub_hvp = &hvp->public;

/*
 * The public access widget (parent of the hierarchy is simply a frame
 * widget
 */
        pub_hvp->htmlview = XtVaCreateWidget
                ("frame",
                 xmFrameWidgetClass,parent,
                 NULL);
        if (type == _hbHLURES)
                XtVaSetValues(pub_hvp->htmlview,
                              XmNtraversalOn, False,
                              NULL);
        
                              
	hvp->not_yet_mapped = True;
        XtSetMappedWhenManaged(pub_hvp->htmlview,False);
        XtManageChild(hvp->public.htmlview);
        pub_hvp->managed = True;
        
        form = XtVaCreateManagedWidget
                ("form",
                 xmFormWidgetClass,pub_hvp->htmlview,
                 NULL);
	hvp->scrwin = XtVaCreateManagedWidget
                ("scrwin",
                 xmScrolledWindowWidgetClass,form,
                 XmNtopAttachment, XmATTACH_FORM,
                 XmNtopOffset, 0,
                 XmNleftAttachment, XmATTACH_FORM,
                 XmNleftOffset, 0,
                 XmNrightAttachment, XmATTACH_FORM,
                 XmNrightOffset, 0,
                 XmNbottomAttachment, XmATTACH_FORM,
                 XmNbottomOffset, 0,
                 NULL);

        hvp->swvsb = XtVaCreateManagedWidget
                ("VertScrollBar",
                 xmScrollBarWidgetClass,hvp->scrwin,
                 NULL);
        XtVaSetValues(hvp->swvsb,
                      XmNwidth,1,
                      NULL);
        
        XtSetMappedWhenManaged(hvp->swvsb,False);
	XtAddCallback(hvp->swvsb, XmNvalueChangedCallback, 
		(XtCallbackProc)swvsbCB,hvp);
#if 0        
        _XmHTMLSelectDebugLevels("all");
#endif        
        
	hvp->html = XtVaCreateManagedWidget
                ("html",
                 xmHTMLWidgetClass,hvp->scrwin,
                 XmNmarginWidth, 0,
                 XmNmarginHeight, 0,
                 XmNwidth, 500,
                 XmNheight, 500,
		 XmNimageEnable,False,
                 NULL);

        XmScrolledWindowSetAreas(hvp->scrwin,NULL,hvp->swvsb,hvp->html);
        
        XtVaGetValues(hvp->html,
                      XmNworkWindow,&hvp->work,
                      XmNhorizontalScrollBar,&hsb,
                      XmNverticalScrollBar,&hvp->vsb,
                      NULL);

        XtSetMappedWhenManaged(hsb,False);
        XtSetMappedWhenManaged(hvp->vsb,False);

        XtVaSetValues(hsb,
                      XmNheight,1,
                      NULL);
        XtVaSetValues(hvp->vsb,
                      XmNwidth,1,
                      NULL);

        pub_hvp->id = ++HtmlViewIdValue;
        pub_hvp->x = pub_hvp->y = pub_hvp->width = pub_hvp->height = 0;
        pub_hvp->max_height = 0;
        pub_hvp->resize_width = False;
        pub_hvp->user_page_id = NgNoPage;
        pub_hvp->locator = pub_hvp->name = NrmNULLQUARK;
        pub_hvp->managed = pub_hvp->mapped = False;
        
        return pub_hvp;
}

/*
 * The following routines access private browser data structures
 */

HtmlViewId NgGetHtmlView(
        int		goid,
        NgPageId	page_id,
        NgHtmlViewType	type,
        HtmlViewId	htmlview_id,
        NrmQuark	locator,
        NrmQuark	name,
        Widget		requestor,
        Position	requestor_x,
        Position	requestor_y
        )
{
        char func[] = "NgGetHtmlView";
        brPane		*pane = _NgGetPaneOfPage(goid,page_id);
        XRectangle	rect;
	int		i,j;
        NhlBoolean	found_one = False;
        int		pos = -1,new_pos = -1,max_y = 0;
        NgHtmlView	*hv,*htmlview = NULL,*save_hv = NULL;
        
#if DEBUG_HTML        
        fprintf(stderr,"in %s\n",func);
#endif
        if (! pane)
                return (int) NhlFATAL;

        _NgGetPaneVisibleArea(_NhlGetLayer(goid),pane,&rect);

/*
 * Look for an unmapped existing Htmlview, especially the one that has
 * the same id. If not found then 
 * look for the Htmlview that is furthest from the currently
 * visible area. Else create a new htmlview. 
 */
        for (i = 0; i < pane->htmlview_count; i++) {
                hv = XmLArrayGet(pane->htmlview_list,i);
                if (hv->mapped) {
                        if (hv->user_page_id == page_id &&
                            hv->id == htmlview_id) {
                                SetHtmlLocation
                                        (hv,pane,
                                         requestor,requestor_x,requestor_y);
                                if (locator != hv->locator ||
                                    name != hv->name)
                                        _NgSetHtmlContent(hv,locator,name);
                                return htmlview_id;
                        }
                }
                else {
                        pos = i;
                        save_hv = hv;
                        if (hv->id == htmlview_id) {
                                htmlview = hv;
                                break;
                        }
                }
#if 0                
                if (rect.y - (hv->y + hv->height) > max_y) {
                        pos = i;
                        save_hv = hv;
                }
                else if (hv->y - (rect.y + rect.height) > max_y) {
                        pos = i;
                        save_hv = hv;
                }
#endif                
        }
        if (! htmlview && save_hv) {
                htmlview = save_hv;
        }
        else if (! htmlview) { /* need to create a new HmtlView */
                htmlview = _NgCreateHtmlView(goid,pane->form,type);
                XtVaSetValues(htmlview->htmlview,
                              XmNbottomAttachment,XmATTACH_NONE,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNleftAttachment,XmATTACH_FORM,
                              XmNtopAttachment,XmATTACH_FORM,
                              XmNresizable,True,
                              NULL);
        }
        htmlview->user_page_id = page_id;
        SetHtmlLocation(htmlview,pane,requestor,requestor_x,requestor_y);
        _NgSetHtmlContent(htmlview,locator,name);

/*
 * Put the htmlview into the list ordered by y value
 */
        if (! pane->htmlview_list) {
                pane->htmlview_list = XmLArrayNew(0,0);
        }
        for (i = 0; i < pane->htmlview_count; i++) {
                hv = XmLArrayGet(pane->htmlview_list,i);
                if (htmlview->y > hv->y) {
                        new_pos = i;
                        break;
                }
        }
        if (new_pos < 0)
                new_pos = pane->htmlview_count;
                
        if (pos < 0) {
                XmLArrayAdd(pane->htmlview_list,new_pos,1);
                XmLArraySet(pane->htmlview_list,new_pos,htmlview);
                pane->htmlview_count++;
        }
        else if (new_pos != pos) {
                XmLArrayMove(pane->htmlview_list,new_pos,pos,1);
        }
        return htmlview->id;
}

NhlErrorTypes NgSetHtmlViewPosition(
        int		goid,
        NgPageId	page_id,
        HtmlViewId	htmlview_id,
        Widget		requestor,
        Position	requestor_x,
        Position	requestor_y
        )
{
        char func[] = "NgSetHtmlViewPosition";
        brPane		*pane = _NgGetPaneOfPage(goid,page_id);
	int		i;
        NgHtmlView	*hv;
        
#if DEBUG_HTML        
        fprintf(stderr,"in %s\n",func);
#endif
        if (! pane)
                return NhlFATAL;

        for (i =0; i < pane->htmlview_count; i++) {
                hv = XmLArrayGet(pane->htmlview_list,i);
                if (htmlview_id == hv->id) {
                        SetHtmlLocation(hv,pane,
                                        requestor,requestor_x,requestor_y);
                        return NhlNOERROR;
                }
        }
                
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                   "%s: htmlview object id %d not found",func,htmlview_id));
                
        return NhlFATAL;

}
        

NhlErrorTypes NgGetHtmlViewSize(
        int		goid,
        NgPageId	page_id,
        HtmlViewId	htmlview_id,
        Dimension	avail_width,
        NhlBoolean	resize_width,
        Dimension	max_height,
        Dimension	*height,
        Dimension	*width
        )
{
        char func[] = "NgGetHtmlViewSize";
        brPane		*pane = _NgGetPaneOfPage(goid,page_id);
	int		i;
        NgHtmlView	*hv;
        
#if DEBUG_HTML        
        fprintf(stderr,"in %s\n",func);
#endif
        if (! pane)
                return NhlFATAL;

        for (i =0; i < pane->htmlview_count; i++) {
                hv = XmLArrayGet(pane->htmlview_list,i);
                if (htmlview_id == hv->id) {
                        _NgGetHtmlViewSize(hv,0,0,False,height,width);
                        return NhlNOERROR;
                }
        }
                
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                   "%s: htmlview object id %d not found",func,htmlview_id));
                
        return NhlFATAL;

}


NhlErrorTypes NgShowHtmlView(
        int		goid,
        NgPageId	page_id,
        HtmlViewId	htmlview_id
        )
{
        char func[] = "NgShowHtmlView";
        brPane		*pane = _NgGetPaneOfPage(goid,page_id);
	int		i;
        NgHtmlView	*hv;
        
#if DEBUG_HTML        
        fprintf(stderr,"in %s\n",func);
#endif
        if (! pane)
                return NhlFATAL;

        for (i =0; i < pane->htmlview_count; i++) {
                hv = XmLArrayGet(pane->htmlview_list,i);
                if (htmlview_id == hv->id) {
                        _NgMapHtmlView(hv);
                        return NhlNOERROR;
                }
        }
                
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                   "%s: htmlview object id %d not found",func,htmlview_id));
                
        return NhlFATAL;
}


NhlErrorTypes NgReleaseHtmlView(
        int		goid,
        NgPageId	page_id,
        HtmlViewId	htmlview_id
        )
{
        char func[] = "NgReleaseHtmlView";
        brPane		*pane = _NgGetPaneOfPage(goid,page_id);
	int		i;
        NgHtmlView	*hv;
        
#if DEBUG_HTML        
        fprintf(stderr,"in %s\n",func);
#endif
        if (! pane)
                return NhlFATAL;

        for (i =0; i < pane->htmlview_count; i++) {
                hv = XmLArrayGet(pane->htmlview_list,i);
                if (htmlview_id == hv->id) {
			XmLArrayDel(pane->htmlview_list,i,1);
			pane->htmlview_count--;
                        _NgDestroyHtmlView(hv);
                        return NhlNOERROR;
                }
        }
                
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                   "%s: htmlview object id %d not found",func,htmlview_id));
                
        return NhlFATAL;
}

NhlErrorTypes NgReleasePageHtmlViews(
        int		goid,
        NgPageId	page_id
        )
{
        char func[] = "NgReleasePageHtmlViews";
        brPane		*pane = _NgGetPaneOfPage(goid,page_id);
	int		i = 0;
        NgHtmlView	*hv;
        
#if DEBUG_HTML        
        fprintf(stderr,"in %s\n",func);
#endif
        if (! pane)
                return NhlFATAL;

        while (i < pane->htmlview_count) {
                hv = XmLArrayGet(pane->htmlview_list,i);
                if (page_id == hv->user_page_id) {
			XmLArrayDel(pane->htmlview_list,i,1);
			pane->htmlview_count--;
                        _NgDestroyHtmlView(hv);
                        continue;
                }
                i++;
        }
                
        return NhlNOERROR;
}

        
