/*
 *      $Id: print.c,v 1.3 1998-09-18 23:47:39 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		print.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 20 14:49:40 MDT 1998
 *
 *	Description:	
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <errno.h>

#include <ncarg/ngo/printP.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/hlu/PSWorkstation.h>
#include <ncarg/hlu/NcgmWorkstation.h>
#include <ncarg/hlu/View.h>

#include <Xm/Xm.h>
#include <Xm/MessageB.h>
#include <Xm/DialogS.h>
#include <Xm/RowColumn.h>
#include <Xm/ToggleBG.h>
#include <Xm/LabelG.h>
#include <Xm/Form.h>
#include <Xm/TextF.h>
#include <Xm/MenuShell.h>
#include <Xm/MwmUtil.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/List.h>
#include <ncarg/ngo/Folder.h>


#define	Oset(field)	NhlOffset(NgPrintRec,print.field)
static NhlResource resources[] = {
	{NgNprUpdateDialog,NgCprUpdateDialog,NhlTBoolean,sizeof(int),
	 Oset(update_dialog),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
	 _NhlRES_SONLY,NULL},      
};
#undef	Oset

static NhlErrorTypes PrintClassPartInitialize(
	NhlClass	lc
);

static NhlErrorTypes PrintInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlBoolean PrintCreateWin(
	NgGO	go
);

static NhlBoolean 
SavePlotToFile(
	NgPrint l,
	NhlBoolean overwrite
);

NgPrintClassRec NgprintClassRec = {
	{
/* class_name		*/	"printClass",
/* nrm_class		*/	NrmNULLQUARK,
/* layer_size		*/	sizeof(NgPrintRec),
/* class_inited		*/	False,
/* superclass		*/	(NhlClass)&NggOClassRec,
/* cvt_table		*/	NULL,

/* layer_resources	*/	resources,
/* num_resources	*/	NhlNumber(resources),
/* all_resources	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

/* class_part_initialize*/	PrintClassPartInitialize,
/* class_initialize	*/	NULL,
/* layer_initialize	*/	PrintInitialize,
/* layer_set_values	*/	NULL,
/* layer_set_values_hook*/	NULL,
/* layer_get_values	*/	NULL,
/* layer_reparent	*/	NULL,
/* layer_destroy	*/	NULL,

	},
	{
/* dialog		*/	NULL,
/* toplevel		*/	NULL,
/* manager		*/	NULL,

/* top_win_chain	*/	False,
/* create_win		*/	PrintCreateWin,
/* create_win_hook	*/	NULL,
/* create_win_hook	*/	_NgGOInheritClose,
	},
	{
/* foo			*/	0,
	},
};

NhlClass NgprintClass = (NhlClass)&NgprintClassRec;

static NrmQuark Qngselectedwork = NrmNULLQUARK;

typedef struct _prPaperType {
        int type;
        float width;
        float height;
        char *name;
} prPaperType;


/* paper sizes are in PostScript points ( 1/72 inch) */

static prPaperType PaperTypes[] = {
        { prLETTER, 612,792, "Letter - (8.5\" x 11\")" },
        { prLEGAL, 612, 1008, "Legal - (8.5\" x 14\")"},
        { prA4, 595, 842, "A4 - (210 cm x 297 cm)"},
        { prA3, 842, 1190, "A3 - (297 cm x 420 cm)"} 
};

typedef struct _prFileType {
        int type;
        char *name;
        char *output_pathname;
        char *default_output_pathname;
} prFileType;

static prFileType FileTypes[] = {
        { prPS, "PS", NULL, "output.ps" },
        { prEPS,  "EPS", NULL, "output.eps"},
        { prEPSI,  "EPSI", NULL, "output.epsi"},
        { prNCGM,  "NCGM", NULL, "output.ncgm"},
        { prGIF,  "GIF", NULL, "output.gif"},
        { prPNG,  "PNG", NULL, "output.png"}
};

typedef struct _prOrientationType {
        int type;
        char *wname;
} prOrientationType;

static prOrientationType OrientationTypes[] = {
	{ prPORTRAIT,"portraitPb"},
	{ prLANDSCAPE,"landscapePb"},
	{ prAUTO_ORIENT,"autoOrientPb"}
};

/* static (global) options - remain in sync for all instantiations of the
   print/output dialog */

static NhlBoolean	To_Printer = True;
static NhlBoolean	MaximizeBB = True;
static int		Orient = prAUTO_ORIENT;
static int		Paper_Type = prLETTER;
static int		File_Type = prPS;
static String		Default_Print_Command = "lp";
static String		Print_Command = NULL;
static NhlBoolean	Overwrite_Warning = True;
static int		Resolution = 600;
static NhlBoolean	Select_All_Views = True;
static int		Selected_View_Count = 0;
static unsigned int	*Selected_View_Positions = NULL;

static NhlErrorTypes
PrintClassPartInitialize
(
	NhlClass	lc
)
{
	NgPrintClass	print = (NgPrintClass)lc;

	/*
	 * Over-ride go class definition of manager class.
	 */
	print->go_class.manager = xmMessageBoxWidgetClass;
	Qngselectedwork = NrmStringToQuark(Ng_SELECTED_WORK); 

	return NhlNOERROR;
}

static NhlErrorTypes
PrintInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	char		func[] = "PrintInitialize";
	NgPrint		ncl = (NgPrint)new;
	NgPrintPart	*np = &((NgPrint)new)->print;
	NgPrintPart	*rp = &((NgPrint)req)->print;
        int		i;

	np->qwks = NULL;
	np->wks_count = 0;
	np->wks_alloc = 0;
	np->wks_pb = NULL;
	np->wks_pb_alloc = 0;
        np->wks_pb_count = 0;
	np->qviews = NULL;
	np->view_ids = NULL;
	np->view_count = 0;
	np->view_alloc = 0;
	np->work_view_count = 0;
        np->up = False;
        np->selected_work_pos = 0;
	np->wks_menu = NULL;
	np->wks_optmenu = NULL;

        for (i = 0; i < NhlNumber(FileTypes); i++) {
                if (FileTypes[i].output_pathname)
                        continue;
                FileTypes[i].output_pathname =
                        FileTypes[i].default_output_pathname;
        }
	return NhlNOERROR;
}


static void CancelCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmAnyCallbackStruct	*cbs =
				(XmAnyCallbackStruct*)cbdata;
	NgPrint	l = (NgPrint)udata;

#if DEBUG_PRINT
        printf("releasing focus\n");
#endif        
        NgAppReleaseFocus(l->go.appmgr,l->base.id);
	NgGOPopdown(l->base.id);
        l->print.up = False;
}


static void
BasicErrorCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmAnyCallbackStruct	*cbs = (XmAnyCallbackStruct*)cbdata;
        NgGO go = (NgGO) udata;
	NgPrint	l = (NgPrint)go;
	NhlBoolean close_dialog;

	XtVaGetValues(w,
		      XmNuserData,&close_dialog,
		      NULL);

	XtDestroyWidget(XtParent(w));
	NgGOSensitive(go->base.id,True);

	if (close_dialog) {
#if DEBUG_PRINT
		printf("releasing focus\n");
#endif        
		NgAppReleaseFocus(l->go.appmgr,l->base.id);
		NgGOPopdown(l->base.id);
		l->print.up = False;
	}

	return;
}



static void
OverwriteMessageCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmAnyCallbackStruct	*cbs = (XmAnyCallbackStruct*)cbdata;
        NgGO go = (NgGO) udata;
	NgPrint	l = (NgPrint)go;
	NhlBoolean ret = False;
	
	XtDestroyWidget(XtParent(w));
	NgGOSensitive(go->base.id,True);

	if (cbs->reason == XmCR_OK) {
#if DEBUG_PRINT
		printf("OK\n");
#endif        
		ret = SavePlotToFile((NgPrint) l,True);
	}
	else if (cbs->reason == XmCR_CANCEL) {
#if DEBUG_PRINT
		printf("CANCEL\n");
#endif        
		return;
	}

	if (ret) {
#if DEBUG_PRINT
		printf("releasing focus\n");
#endif        
		NgAppReleaseFocus(l->go.appmgr,l->base.id);
		NgGOPopdown(l->base.id);
		l->print.up = False;
	}

	return;
}

/*
 * simple popup message facility. if button text pointers are NULL the
 * button is omitted. If (int) pointer == 1, use default button text.
 */ 

static void Message(
	NgGO go,
	char *title,
	char *message,
	int  type,
	int  default_button,
	char *ok_text,
	char *cancel_text,
	char *help_text,
	XtCallbackProc  cb,
	XtPointer	user_data
)
{
	Widget dialogsh,dialog;
	XmString xmmessage;
	Arg	ar[20];
	int	ac = 0;
	XmString xmok,xmcancel,xmhelp;

	NgGOSensitive(go->base.id,False);
	xmmessage = NgXAppCreateXmString(go->go.appmgr,message);

	if ((int) ok_text > 1) {
		xmok = NgXAppCreateXmString(go->go.appmgr,ok_text);
		XtSetArg(ar[ac],XmNokLabelString,xmok); ac++;
	}
	if ((int) cancel_text > 1) {
		xmcancel = NgXAppCreateXmString(go->go.appmgr,cancel_text);
		XtSetArg(ar[ac],XmNcancelLabelString,xmcancel); ac++;
	}
	if ((int) help_text > 1) {
		xmhelp = NgXAppCreateXmString(go->go.appmgr,help_text);
		XtSetArg(ar[ac],XmNhelpLabelString,xmhelp); ac++;
	}
	XtSetArg(ar[ac],XmNautoUnmanage,False); ac++;
	XtSetArg(ar[ac],XmNtitle,title); ac++;
	XtSetArg(ar[ac],XmNdialogType,type); ac++;
	XtSetArg(ar[ac],XmNdefaultButtonType,XmDIALOG_CANCEL_BUTTON); ac++;
	XtSetArg(ar[ac],XmNmessageString,xmmessage); ac++;
	XtSetArg(ar[ac],XmNuserData,user_data); ac++;
	dialog = XmCreateMessageDialog(go->go.manager,"Message",ar,ac);

	NgXAppFreeXmString(go->go.appmgr,xmmessage);
	if ((int) ok_text > 1) {
		NgXAppFreeXmString(go->go.appmgr,xmok);
	}
	if ((int) cancel_text > 1) {
		NgXAppFreeXmString(go->go.appmgr,xmcancel);
	}
	if ((int) help_text > 1) {
		NgXAppFreeXmString(go->go.appmgr,xmhelp);
	}

	if (help_text)
		XtAddCallback(dialog,XmNactivateCallback,cb,go);
	else
		XtUnmanageChild
			(XmMessageBoxGetChild(dialog,XmDIALOG_HELP_BUTTON));
	if (ok_text)
		XtAddCallback(dialog,XmNokCallback,cb,go);
	else
		XtUnmanageChild
			(XmMessageBoxGetChild(dialog,XmDIALOG_OK_BUTTON));
	if (cancel_text)
		XtAddCallback(dialog,XmNcancelCallback,cb,go);
	else
		XtUnmanageChild
			(XmMessageBoxGetChild(dialog,XmDIALOG_CANCEL_BUTTON));

	XtManageChild(dialog);
	return;
}
	


/*
 * gets the full pathname into buf, for a relative pathname, including a
 * tilde-prefixed pathname, Should check for buffer overflows, but doesn't
 * yet. also substitutes environment variables.
 */
		 
static NhlBoolean
GetFullPath(
	NgGO go,
	char *buf,
	char *rel_path,
	int  buflen
)
{
	const char 	*pathp;

	pathp = _NGResolvePath(rel_path);
	if (! pathp) {
		Message(go,"System Diagnostic","Invalid file name",
			XmDIALOG_ERROR,XmDIALOG_OK_BUTTON,
			(char *) 1,NULL,NULL,BasicErrorCB,
			(void *)False);
			return False;
	}
	if (pathp[0] == '/') {
		strcpy(buf,pathp);
	}
	else {
		getcwd(buf,1024);
		strcat(buf,"/");
		strcat(buf,pathp);
	}
	return True;

#if 0
	if (rel_path[0] == '/') { 
		strcpy(tbuf,rel_path);
	}
	else if (rel_path[0] == '~') {
		if (rel_path[1] == '/') { 
			pw = getpwnam(getuid());
			strcpy(tbuf,pw->pw_dir);
			strcat(tbuf,&rel_path[1]);
		}
		else {
			ep = strchr(relpath,'/');
			if (ep) {
				strncpy(tbuf,&relpath[1],ep-1);
				tbuf[ep-1] = '\0';
				pw = getpwnam(tbuf);
				strcpy(tbuf,pw->pw_dir);
				strcat(tbuf,&rel_path[ep]);
			}
			else {
				strcpy(tbuf,&relpath[1]);
				pw = getpwnam(tbuf);
				strcpy(tbuf,pw->pw_dir);
			}
		}
	}
	else {
		getcwd(tbuf,1024);
		strcat(tbuf,"/");
		strcat(tbuf,rel_path);
	}

	/* now substitute environment variables */

	cp = tbuf;
	buf[0] = '\0';
	while (! done) {
		sp = strchr(cp,'$');
		if (!sp) {
			done = 1;
			strcat(buf,cp);
			continue;
		}
		
	
	return;
#endif
}

static void 
GetBBOfViews(
	NgPrint l,
	NhlBoundingBox	*bb,
	NhlBoolean	all_views
)
{
	NgPrintPart	*np = &l->print;
	int		i;

	bb->set = 0;
	bb->t  = 0.0;
	bb->b  = 0.0;
	bb->l  = 0.0;
	bb->r  = 0.0;

	if (MaximizeBB) {
		for (i = 0; i < np->work_view_count; i++) {
			if (! all_views && 
			    ! XmListPosSelected(np->view_list,i+1))
				continue;
			_NhlGetBB(_NhlGetLayer(np->view_ids[i]),bb);
		}
	}
	else {
		bb->r = 1.0;
		bb->t = 1.0;
	}
	return;
}

static void 
OutputNcgm(
	NgPrint l,
	NhlString outfile
)
{
	NgPrintPart	*np = &l->print;
	NhlErrorTypes   ret = NhlNOERROR;
	int		wks;
	NhlGenArray 	cmap = NULL;
	NhlColor	*cmap_data;
        int             rl;
	Boolean		all_set;
	char		buf[512];
	int		i;


	NhlVAGetValues(np->selected_work_id,
		       NhlNwkColorMap,&cmap,
		       NULL);
	cmap_data = (NhlColor *) cmap->data;
	cmap_data[0][0] = -1;
	cmap_data[1][0] = -1;

	rl = NhlRLCreate(NhlSETRL);
        NhlRLSetString(rl,NhlNwkMetaName,outfile);
 	
        strcpy(buf,NrmQuarkToString(cmap->typeQ));
        strcat(buf,"GenArray");
        NhlRLSet(rl,NhlNwkColorMap,buf,cmap);
 
        ret = NhlCreate(&wks,"NcgmWrk",NhlncgmWorkstationClass,
			NhlDEFAULT_APP,rl);
	if (ret < NhlWARNING)
		return;

        NhlFreeGenArray(cmap);
        NhlRLDestroy(rl);

	for (i = 0; i < np->work_view_count; i++) {
		if (! Select_All_Views && 
		    ! XmListPosSelected(np->view_list,i+1))
			continue;
		ret = NhlChangeWorkstation(np->view_ids[i],wks);
		NhlDraw(np->view_ids[i]);
		ret = NhlChangeWorkstation(np->view_ids[i],
					   np->selected_work_id);
	}

	NhlFrame(wks);
	NhlDestroy(wks);
}

static NhlBoolean
ReadInteger(
	Widget	w,
	long	*value
)
{
	String svalue;
	char   *cp;


	XtVaGetValues(w,
		      XmNvalue,&svalue,
		      NULL);
	errno = 0;
	*value = strtol(svalue,&cp,0);
	
	XtFree(svalue);
	if (cp == svalue)
		return False;
	if (errno == ERANGE)
		return False;

	return True;
}

static NhlBoolean 
OutputGif(
     NgPrint l,
     NhlString outfile
     )
{
        NgPrintPart	*np = &l->print;
        NhlErrorTypes   ret = NhlNOERROR;
        const char	*tmpdir;
        char		buf[1024];
        char		buf1[1024];
	char		stderrfile[1024];
        pid_t		pid;
        char		*basename = "tmp.ncgm";
        float		margin = 0;
        NhlBoundingBox	bbox;
        float 		pw,ph;
	float		wlx,wly,wux,wuy;
	int		rh,rw;
	long		res;
	char		*postprocess;
        struct 		stat statbuf;

	if (ReadInteger(np->resolution_text,&res))
		Resolution = res;
	else {
		sprintf(buf,"%d",Resolution);
		XtVaSetValues(np->resolution_text,
			      XmNvalue,buf,
			      NULL);
	}
	if (MaximizeBB) {
		GetBBOfViews(l,&bbox,Select_All_Views);

		pw = bbox.r - bbox.l;
		ph = bbox.t - bbox.b;
		margin = 0.05 * MIN(pw,ph);
		wlx = MAX(0.0,bbox.l - margin);
		wly = MAX(0.0,bbox.b - margin);
		wux = MIN(1.0,bbox.r + margin);
		wuy = MIN(1.0,bbox.t + margin);
		ph = wuy - wly;
		pw = wux - wlx;
		if (ph > pw) {
			rh = Resolution;
			rw = pw / ph * Resolution;
		}
		else {
			rw = Resolution;
			rh = ph / pw * Resolution;
		}
	}
	else {
		wlx = wly = 0.0;
		wux = wuy = 1.0;
		rh = rw = Resolution;
	}

        tmpdir = GetNCARGPath("tmp");
        pid = getpid();
	
        if (strlen(tmpdir) + strlen(basename) + 12 > 1024) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "command exceeds buffer length"));
                return False;
        }
        sprintf(buf,"%s/%d%s",tmpdir,pid,basename);
 
        OutputNcgm(l,buf);

	sprintf(stderrfile,"%s/stderr.%d",tmpdir,pid);
	sprintf(buf1,
		"sh -c '( ctrans -d sun -res %dx%d -window %f:%f:%f:%f %s 2>%s | rasttopnm -quiet 2>>%s | ppmtogif -quiet 2>>%s >%s )' 1>>%s 2>&1",
		rw,rh,wlx,wly,wux,wuy,buf,
		stderrfile,stderrfile,stderrfile,outfile,stderrfile);
        
        system(buf1);
	unlink(buf);

        if (! stat(stderrfile,&statbuf)) {
		int i,rlen,len = MIN(1024,statbuf.st_size);
		if (len == 0) {
			unlink(stderrfile);
		}
		else {
			FILE *fp = fopen(stderrfile,"r");
			rlen = fread(buf,1,len,fp);
			buf[MIN(1023,rlen)] = '\0';
		
			for (i = rlen-1; i >= 0; i--) {
				if (isspace(buf[i]))
					buf[i] = '\0';
				else 
					break;
			}
		
			Message((NgGO)l,"System Diagnostic",buf,
				XmDIALOG_ERROR,XmDIALOG_OK_BUTTON,
				(char *) 1,NULL,NULL,BasicErrorCB,
				(void *)True);
			fclose(fp);
			unlink(stderrfile);
			return False;
		}
	}
	return True;
}

static void 
OutputPostScript(
	NgPrint l,
        int type,
	NhlString outfile
        )
{
        NgPrintPart	*np = &l->print;
        NhlErrorTypes   ret = NhlNOERROR;
        int		wks;
        NhlGenArray 	cmap = NULL;
        NhlColor	*cmap_data;
        int             rl;
        char		buf[512];
        int		i;
        float		margin = 36.0;
        float		lx,ly,ux,uy;
        NhlWorkOrientation orient = NhlPORTRAIT;
        NhlBoundingBox	bbox;
        float 		pw,ph;
        float		dw,dh;

        GetBBOfViews(l,&bbox,Select_All_Views);

        pw = bbox.r - bbox.l;
        ph = bbox.t - bbox.b;

        lx = margin;
        ly = margin;
        ux = PaperTypes[Paper_Type].width - margin;
        uy = PaperTypes[Paper_Type].height - margin;
        dw = ux - lx;
        dh = uy - ly;

        if (Orient == prPORTRAIT ||
            (Orient == prAUTO_ORIENT && ph / pw >= 1.0) ) {
                float ndc2du;
                if  (ph / pw > dh / dw)
                        /*  paper height limits size */
                        ndc2du = dh / ph;
                else
                        ndc2du = dw / pw;
                lx = margin + 0.5 * ( dw - pw * ndc2du)
                        - bbox.l * ndc2du;
                ly = margin + 0.5 * ( dh - ph * ndc2du) 
                        - bbox.b * ndc2du;
                ux = lx + ndc2du;
                uy = ly + ndc2du;
        }
        else {
                float ndc2du;
                orient = NhlLANDSCAPE;
                if  (pw / ph > dh / dw)
                        /*  paper height limits size */
                        ndc2du = dh / pw;
                else
                        ndc2du = dw / ph;

                ly = margin + 0.5 * (dh - pw * ndc2du) 
                        - (1.0 - bbox.r) * ndc2du;
                lx = margin + 0.5 * (dw - ph * ndc2du)
                        - bbox.b * ndc2du;
                ux = lx + ndc2du;
                uy = ly + ndc2du;
        }                                               

        NhlVAGetValues(np->selected_work_id,
                       NhlNwkColorMap,&cmap,
                       NULL);
        cmap_data = (NhlColor *) cmap->data;
        cmap_data[0][0] = -1;
        cmap_data[1][0] = -1;

        rl = NhlRLCreate(NhlSETRL);
        NhlRLSetString(rl,NhlNwkPSFileName,outfile);
 	
        strcpy(buf,NrmQuarkToString(cmap->typeQ));
        strcat(buf,"GenArray");
        NhlRLSet(rl,NhlNwkColorMap,buf,cmap);
        NhlRLSet(rl,NhlNwkDeviceLowerX,NhlTInteger,(int)lx);
        NhlRLSet(rl,NhlNwkDeviceUpperX,NhlTInteger,(int)ux);
        NhlRLSet(rl,NhlNwkDeviceLowerY,NhlTInteger,(int)ly);
        NhlRLSet(rl,NhlNwkDeviceUpperY,NhlTInteger,(int)uy);
        NhlRLSet(rl,NhlNwkOrientation,NhlTInteger,orient);
        NhlRLSet(rl,NhlNwkPSFormat,NhlTInteger,type);
 
        ret = NhlCreate(&wks,"PSWrk",NhlpsWorkstationClass,
                        NhlDEFAULT_APP,rl);
        if (ret < NhlWARNING)
                return;

        NhlFreeGenArray(cmap);
        NhlRLDestroy(rl);

        for (i = 0; i < np->work_view_count; i++) {
                if (! Select_All_Views && 
		    ! XmListPosSelected(np->view_list,i+1))
                        continue;
                ret = NhlChangeWorkstation(np->view_ids[i],wks);
                NhlDraw(np->view_ids[i]);
                ret = NhlChangeWorkstation(np->view_ids[i],
                                           np->selected_work_id);
        }

        NhlFrame(wks);
        NhlDestroy(wks);
}

	
static NhlBoolean
QualifyPathname(
	NgPrint l,
	String pathname
	)
{
        int ret;
        struct stat statbuf;
	FILE *fp;
	char pathbuf[1024];
	char buf[1024];

/*
 * pathname should be an absolute pathname before being 
 * being sent to this routine.
 */	
        if (! stat(pathname,&statbuf)) { /*file exists */
		/* try opening for append; that way it won't be damaged */

		errno = 0;
		fp = fopen(pathname,"a");
		if (errno) {
			sprintf(buf,"%s: %s",pathname,strerror(errno));
			Message((NgGO)l,"System Diagnostic",buf,
				XmDIALOG_ERROR,XmDIALOG_OK_BUTTON,
				(char *) 1,NULL,NULL,BasicErrorCB,
				(void *)False);
			return False;
		}
		if (fp)
			fclose(fp);

		if (Overwrite_Warning) {
			int len;
			sprintf(buf,"Overwrite %s?",pathname);
			Message((NgGO)l,"Overwrite?", buf,
				XmDIALOG_QUESTION,
				XmDIALOG_CANCEL_BUTTON,
				"Yes","No",NULL,
				OverwriteMessageCB,(void *)0);
			return False;
		}
        }
	else {
		errno = 0;
		fp = fopen(pathname,"w");
		if (errno) {
			sprintf(buf,"%s: %s",pathname,strerror(errno));
			Message((NgGO)l,"System Diagnostic",buf,
				XmDIALOG_ERROR,XmDIALOG_OK_BUTTON,
				(char *) 1,NULL,NULL,BasicErrorCB,
				(void *)False);
			return False;
		}
		if (fp)
			fclose(fp);
		unlink(pathname);
	}
	return True;
}
        
static NhlBoolean 
SavePlotToFile(
	NgPrint		l,
	NhlBoolean	from_cb
)
{
	NgPrintPart	*pp = &l->print;
        String  	pathname;
	char		fullpath[1024];

        XtVaGetValues(pp->file_text,
                      XmNvalue,&pathname,
                      NULL);

	if (! GetFullPath((NgGO)l,fullpath,pathname,1024))
		return False;

        if (! from_cb) {
		if (! QualifyPathname(l,fullpath))
			return False;
	}
        if (strcmp(pathname,FileTypes[File_Type].output_pathname)) {
                if (FileTypes[File_Type].output_pathname !=
                    FileTypes[File_Type].default_output_pathname)
                        XtFree(FileTypes[File_Type].output_pathname);
                FileTypes[File_Type].output_pathname = pathname;
        }

        switch(File_Type) {
            case prPS:
                    OutputPostScript(l,prPS,fullpath);
                    break;
            case prEPS:
                    OutputPostScript(l,prEPS,fullpath);
                    break;
            case prEPSI:
                    OutputPostScript(l,prEPSI,fullpath);
                    break;
            case prNCGM:
                    OutputNcgm(l,fullpath);
                    break;
            case prGIF:
                    return OutputGif(l,fullpath);
            case prPNG:
                    break;
        }
                    
	return True;
}

static NhlBoolean 
PrintPlot
(
	NgPrint l
)
{
	NgPrintPart	*np = &l->print;
	const char	*tmpdir;
	char		buf[1024];
	pid_t		pid;
	char		*basename = "tmp.ps";
	char		stderrfile[1024];
	String		print_command;
	int		status;
        struct 		stat statbuf;

	XtVaGetValues(np->print_text,
		      XmNvalue,&print_command,
		      NULL);
	XtFree(Print_Command);
	Print_Command = print_command;
	
	tmpdir = GetNCARGPath("tmp");
	pid = getpid();
	
	if (strlen(tmpdir) + strlen(basename) + 
	    strlen(Print_Command) +12 > 1024) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "print command exceeds buffer length"));
		return False;
	}
	sprintf(buf,"%s/%d%s",tmpdir,pid,basename);
	OutputPostScript(l,prPS,buf);
	sprintf(stderrfile,"%s/stderr.%d",tmpdir,pid);
	sprintf(buf,"sh -c '( %s %s/%d%s 1>%s 2>&1 )' 1>>%s 2>&1",
		Print_Command,tmpdir,pid,basename,stderrfile,stderrfile);
	errno = 0;
	system(buf);

	wait(&status);
#if DEBUG_PRINT
	printf("the status is %d ,errno is %d\n",WEXITSTATUS(status),errno);
#endif
	
	sprintf(buf,"%s/%d%s",tmpdir,pid,basename);
	unlink(buf);

        if (! stat(stderrfile,&statbuf)) {
		int i,rlen,len = MIN(1024,statbuf.st_size);
		if (len == 0) {
			unlink(stderrfile);
		}
		else {
			FILE *fp = fopen(stderrfile,"r");
			rlen = fread(buf,1,len,fp);
			buf[MIN(1023,rlen)] = '\0';
		
			for (i = rlen-1; i >= 0; i--) {
				if (isspace(buf[i]))
					buf[i] = '\0';
				else 
					break;
			}
		
			Message((NgGO)l,"System Diagnostic",buf,
				XmDIALOG_ERROR,XmDIALOG_OK_BUTTON,
				(char *) 1,NULL,NULL,BasicErrorCB,
				(void *)False);
			fclose(fp);
			unlink(stderrfile);
			return False;
		}
	}

	return True;
}

static void
PrintScriptOkCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	XmAnyCallbackStruct	*cbs =
				(XmAnyCallbackStruct*)cbdata;
	char	func[] = "PrintScriptOkCB";
	NgPrint	l = (NgPrint)udata;
	NgPrintPart	*np = &l->print;
	int	nsid = NhlDEFAULT_APP;
	char	line[512];
	Boolean	sensitive;
	Boolean ret;

	
	if (np->work_view_count >  0) {
		if (To_Printer)
			ret = PrintPlot(l);
        	else
			ret = SavePlotToFile(l,False);
	}
	if (ret) {
#if DEBUG_PRINT
		printf("releasing focus\n");
#endif        
		NgAppReleaseFocus(l->go.appmgr,l->base.id);
		NgGOPopdown(l->base.id);
		l->print.up = False;
	}

	return;
}

static NhlErrorTypes UpdateHLUObjectList
(
	NgGO go
)
{
	NgPrintPart	*np = &((NgPrint)go)->print;
	int		hlu_count;
	int		wk_count, view_count;
        NrmQuark	*hlu_qvars;
	int		i,selected_work_id = 0;
	int		id, *id_array;
	int 		id_array_count;
	NhlBoolean	first_work = True;


	hlu_qvars = NclGetHLUVarSymNames(&hlu_count);

	np->wks_count = np->view_count = 0;
	for (i = 0; i < hlu_count; i++) {
                NhlLayer l;
                
		/* don't list NgSelectedWork */
		if (hlu_qvars[i] == Qngselectedwork)
			continue;
		id = NgNclGetHluObjId
			(go->go.nclstate,NrmQuarkToString(hlu_qvars[i]),
			 &id_array_count,&id_array);
		if (id_array_count > 1)
			NhlFree(id_array);
		if (id <= NhlNULLOBJID) {
			/* not created yet */
			continue;
                }
                l = _NhlGetLayer(id);
		if (_NhlIsWorkstation(l)) {
			if (first_work) {
				selected_work_id = NgNclGetHluObjId
				  (go->go.nclstate,Ng_SELECTED_WORK,
				   &id_array_count,&id_array);
                                if (id_array_count > 1)
                                        NhlFree(id_array);
				first_work = False;
			}
			if (np->wks_count == np->wks_alloc) {
				np->wks_alloc += 5;
				np->qwks = NhlRealloc
				  (np->qwks,sizeof(NrmQuark)*np->wks_alloc);
			}
			if (id == selected_work_id) {
				np->selected_work_pos = np->wks_count;
                                np->selected_work_id = id;
                        }
			np->qwks[np->wks_count++] = hlu_qvars[i];
		}
		else if (_NhlIsView(l) && ! _NhlIsPlotMember(id)) {
			if (np->view_count == np->view_alloc) {
				np->view_alloc += 5;
				np->qviews = NhlRealloc
				  (np->qviews,
				   sizeof(NrmQuark)*np->view_alloc);
				np->view_ids = NhlRealloc
				  (np->view_ids,sizeof(int)*np->view_alloc);
			}
			np->qviews[np->view_count] = hlu_qvars[i];
			np->view_ids[np->view_count] = id;
			np->view_count++;
		}
	}
	return NhlNOERROR;
}

static void
UpdateLayoutDialog
(
	NgPrint	l
)
{
        NgPrintPart *pp = &l->print;

	XtVaSetValues
		(pp->orient_lbl,
		 XmNsensitive,
		 (To_Printer || File_Type < prNCGM) ? True : False,
		 NULL);
	XtVaSetValues
		(pp->orient_optmenu,
		 XmNmenuHistory,pp->orient_pbs[Orient],
		 XmNsensitive,
		 (To_Printer || File_Type < prNCGM) ? True : False,
		 NULL);
	XtVaSetValues
		(XmOptionButtonGadget(pp->orient_optmenu),
		 XmNsensitive,
                 (To_Printer || File_Type < prNCGM) ? True : False,
		 NULL);
	XtVaSetValues
		(pp->plot_bounds_lbl,	
		 XmNsensitive,
		 (To_Printer || File_Type != prNCGM) ? True : False,
		 NULL);
	XtVaSetValues
		(pp->maximize_bb_tgl,
		 XmNsensitive,
		 (To_Printer || File_Type != prNCGM) ? True : False,
		 XmNset,(MaximizeBB && File_Type != prNCGM) ? True: False,
		 NULL);
	XtVaSetValues
		(pp->full_viewspace_tgl,
		 XmNsensitive,
		 !To_Printer && File_Type == prNCGM ? False : True,
		 XmNset,(!MaximizeBB || File_Type == prNCGM) ? True: False,
		 NULL);
	XtVaSetValues
		(pp->paper_type_lbl,
		 XmNsensitive,To_Printer || File_Type < prNCGM ? True : False,
		 NULL);
	XtVaSetValues
		(pp->paper_type_optmenu,
		 XmNmenuHistory,pp->paper_type_pbs[Paper_Type],
		 XmNsensitive,To_Printer || File_Type < prNCGM ? True : False,
		 NULL);
	XtVaSetValues
		(XmOptionButtonGadget(pp->paper_type_optmenu),
		 XmNsensitive,
                 (To_Printer || File_Type < prNCGM) ? True : False,
		 NULL);
	XtVaSetValues
		(pp->resolution_text,
		 XmNsensitive,!To_Printer && File_Type >= prGIF ? True : False,
		 NULL);
	XtVaSetValues
		(pp->resolution_lbl,
		 XmNsensitive,!To_Printer && File_Type >= prGIF ? True : False,
		 NULL);
	return;
}
     
static void
ChooseOutputDestinationCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
        )
{
	XmToggleButtonCallbackStruct *cbs =
                (XmToggleButtonCallbackStruct*)cbdata;
	char	func[] = "ChooseOutputDestinationCB";
        NgPrint l = (NgPrint) udata;
	NgPrintPart	*np = &l->print;
	int		dest;

	if (! cbs->set) {
		XtVaSetValues (w, XmNset, True, 0);
	}
	else if (w == np->print_tgl) {
		XtVaSetValues(np->file_tgl,XmNset,False,NULL);
		XtVaSetValues(np->file_text,XmNsensitive,False,NULL);
		XtVaSetValues(np->file_text_lbl,XmNsensitive,False,NULL);
		XtVaSetValues(np->file_type_lbl,XmNsensitive,False,NULL);
		XtVaSetValues(np->file_type_optmenu,XmNsensitive,False,NULL);
		XtVaSetValues
			(XmOptionButtonGadget(np->file_type_optmenu),
			 XmNsensitive,False,NULL);
		XtVaSetValues(np->overwrite_tgl,XmNsensitive,False,NULL);
		XtVaSetValues(np->print_text_lbl,XmNsensitive,True,NULL);
		XtVaSetValues(np->print_text,XmNsensitive,True,NULL);
		XmProcessTraversal(np->print_text,XmTRAVERSE_CURRENT);
		To_Printer = True;
		
	}
	else if (w == np->file_tgl) {
		XtVaSetValues(np->print_tgl,XmNset,False,NULL);
		XtVaSetValues(np->print_text_lbl,XmNsensitive,False,NULL);
		XtVaSetValues(np->print_text,XmNsensitive,False,NULL);
		XtVaSetValues(np->file_text_lbl,XmNsensitive,True,NULL);
		XtVaSetValues(np->file_text,XmNsensitive,True, NULL);
		XtVaSetValues(np->file_type_lbl,XmNsensitive,True, NULL);
		XtVaSetValues(np->file_type_optmenu,XmNsensitive,True, NULL);
		XtVaSetValues
			(XmOptionButtonGadget(np->file_type_optmenu),
			 XmNsensitive,True,NULL);
		XtVaSetValues(np->overwrite_tgl,XmNsensitive,True,NULL);
		XmProcessTraversal(np->file_text, XmTRAVERSE_CURRENT);
		To_Printer = False;
	}
	else {
		abort();
	}
	UpdateLayoutDialog(l);
	return;
}

static void
SelectFileTypeCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
        )
{
	XmAnyCallbackStruct	*cbs = (XmAnyCallbackStruct*)cbdata;
	char	func[] = "SelectFileTypeCB";
	NgPrint	l = (NgPrint)udata;
        NgPrintPart *pp = &l->print;
	int	file_type;
        String  pathname;
        
        XtVaGetValues(w,
                      XmNuserData,&file_type,
                      NULL);
        XtVaGetValues(pp->file_text,
                      XmNvalue,&pathname,
                      NULL);

            /* save changes made to the pathname of the last type */
        if (strcmp(pathname,FileTypes[File_Type].output_pathname)) {
                if (FileTypes[File_Type].output_pathname !=
                    FileTypes[File_Type].default_output_pathname)
                        XtFree(FileTypes[File_Type].output_pathname);
                FileTypes[File_Type].output_pathname = pathname;
        }
        File_Type = file_type;
        XtVaSetValues(pp->file_text,
                      XmNvalue,FileTypes[File_Type].output_pathname,
                      NULL);

	UpdateLayoutDialog(l);

        return;
        
}

static void
OverwriteCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
        )
{
	XmToggleButtonCallbackStruct *cbs = 
		(XmToggleButtonCallbackStruct *)cbdata;
	char	func[] = "OverwriteCB";
	NgPrint	l = (NgPrint)udata;
        NgPrintPart *pp = &l->print;

	if (cbs->set)
		Overwrite_Warning = True;
	else
		Overwrite_Warning = False;
	return;
	
}
static void
SelectViewCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
        )
{
	XmListCallbackStruct *lcb = (XmListCallbackStruct *)cbdata;
        NgPrint l = (NgPrint) udata;
	NgPrintPart	*np = &l->print;
	int count;

	if (Select_All_Views) {
		if (! XmListPosSelected(np->view_list,lcb->item_position))
			XmListSelectPos
				(np->view_list,lcb->item_position,False);
		XmProcessTraversal(np->all_tgl,XmTRAVERSE_CURRENT);
		return;
	}
	XtVaSetValues(XmMessageBoxGetChild(l->go.manager,XmDIALOG_OK_BUTTON),
		      XmNsensitive,lcb->selected_item_count ? True : False,
		      NULL);

	return;
}

static void
SelectAllViewsCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
        )
{
	XmToggleButtonCallbackStruct *cbs =
                (XmToggleButtonCallbackStruct*)cbdata;
	char	func[] = "SelectAllViewsCB";
        NgPrint l = (NgPrint) udata;
	NgPrintPart	*np = &l->print;

	if (! cbs->set) {
		XtVaSetValues (w, XmNset, True, 0);
	}
	if (w == np->all_tgl) {
		int item_count,i;
		XtVaSetValues(np->selected_tgl,
			      XmNset,False,
			      NULL);
#if 0
		XtVaSetValues(np->view_list,
			      XmNsensitive,False,
			      NULL);
#endif
		XtVaGetValues(np->view_list,
			      XmNitemCount,&item_count,
			      NULL);
		for (i = 1; i <= item_count; i++) {
			if (! XmListPosSelected(np->view_list,i))
				XmListSelectPos(np->view_list,i,False);	
		}
		XtVaSetValues(XmMessageBoxGetChild
			      (l->go.manager,XmDIALOG_OK_BUTTON),
			      XmNsensitive,True,
			      NULL);
		Select_All_Views = True;
	}
	else if (w == np->selected_tgl) {
		XtVaSetValues(np->all_tgl,
			      XmNset,False,
			      NULL);
                XmListDeselectAllItems(np->view_list);
#if 0
		XtVaSetValues(np->view_list,
			      XmNsensitive,True,
			      NULL);
#endif
		Select_All_Views = False;
		XtVaSetValues(XmMessageBoxGetChild
			      (l->go.manager,XmDIALOG_OK_BUTTON),
			      XmNsensitive,False,
			      NULL);
	}	                
        return;
}


static void
SelectPaperSizeCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
        )
{
	XmAnyCallbackStruct	*cbs = (XmAnyCallbackStruct*)cbdata;
	char	func[] = "SelectPaperSizeCB";
	NgPrint	l = (NgPrint)udata;
        NgPrintPart *pp = &l->print;
	int	paper_type;

        XtVaGetValues(w,
                      XmNuserData,&paper_type,
                      NULL);
	Paper_Type = paper_type;
	return;
}

static void
SelectOrientationCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
        )
{
	XmAnyCallbackStruct	*cbs = (XmAnyCallbackStruct*)cbdata;
	char	func[] = "SelectOrientationCB";
	NgPrint	l = (NgPrint)udata;
        NgPrintPart *pp = &l->print;
	int	orient;

        XtVaGetValues(w,
                      XmNuserData,&orient,
                      NULL);
	Orient = orient;
	return;
}

static void
MaximizeBBCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
        )
{
	XmToggleButtonCallbackStruct *cbs =
                (XmToggleButtonCallbackStruct*)cbdata;
	char	func[] = "MaximizeBBCB";
        NgPrint l = (NgPrint) udata;
	NgPrintPart	*np = &l->print;
	int		dest;

	if (! cbs->set) {
		XtVaSetValues (w, XmNset, True, 0);
	}
	if (w == np->maximize_bb_tgl) {
		MaximizeBB = True;
		XtVaSetValues(np->full_viewspace_tgl,
			      XmNset,False,NULL);
	}
	else if (w == np->full_viewspace_tgl) {
		MaximizeBB = False;
		XtVaSetValues(np->maximize_bb_tgl,
			      XmNset,False,NULL);
	}
	return;
}

                
static void
UpdateViewListBox
(
	NgPrint	l,
        Widget top
)
{
	NgPrintPart	*np = &l->print;
	NhlBoolean	create;
        int		i,n;
	Arg		args[32];
        int		id,id_array_count;
        int		*id_array;
        Boolean		sensitive;
        
	create = top ? True : False;

        if (create) {
                n = 0;
                XtSetArg(args[n],XmNtopWidget,top); n++;
                XtSetArg(args[n],XmNtopAttachment,XmATTACH_WIDGET); n++;
                XtSetArg(args[n],XmNleftAttachment,XmATTACH_FORM); n++;
                XtSetArg(args[n],XmNrightAttachment,XmATTACH_FORM); n++;
                XtSetArg(args[n],XmNbottomAttachment,XmATTACH_NONE); n++;
                XtSetArg(args[n],XmNselectionPolicy,XmMULTIPLE_SELECT); n++;
                XtSetArg(args[n],XmNlistSizePolicy,XmCONSTANT); n++;
                XtSetArg(args[n],XmNscrollBarDisplayPolicy,XmAS_NEEDED); n++;
                XtSetArg(args[n],XmNvisibleItemCount,7); n++;
                XtSetArg(args[n],XmNnavigationType,XmSTICKY_TAB_GROUP); n++;
                XtSetArg(args[n],XmNautomaticSelection,True); n++;
                XtSetArg(args[n],XmNuserData,l); n++;
        
                np->view_list = XmCreateScrolledList
                        (np->form_parent,"viewList",args,n);
                XtManageChild(np->view_list);
		XtAddCallback(np->view_list,
			      XmNmultipleSelectionCallback,SelectViewCB,l);
        }
        XmListDeleteAllItems(np->view_list);

        if (np->wks_count == 0 && np->view_count == 0)
                return;
        
        np->work_view_count = 0;
        
        /* find views connected to current workstation */
        for (i = 0; i < np->view_count; i++) {
                int id = np->view_ids[i];
                if (np->selected_work_id == NhlGetParentWorkstation(id)) {
                        XmString xmview;
                        xmview = NgXAppCreateXmString
                                (l->go.appmgr,NrmQuarkToString(np->qviews[i]));
                        XmListAddItem(np->view_list,xmview,0);
			if (Select_All_Views)
				XmListSelectPos(np->view_list,0,False);	
                        NgXAppFreeXmString(l->go.appmgr,xmview);
                        np->work_view_count++;
                }
        }
        
        sensitive = np->work_view_count > 0 ? True : False;

        XtVaSetValues(np->plot_views_lbl,
                      XmNsensitive,sensitive,
                      NULL);
        XtVaSetValues(np->all_tgl,
                      XmNsensitive,sensitive,
		      XmNset,Select_All_Views ? True : False,
                      NULL);
        XtVaSetValues(np->selected_tgl,
                      XmNsensitive,sensitive,
		      XmNset,Select_All_Views ? False : True,
                      NULL);
		
#if 0
        XtVaSetValues(np->view_list,
                      XmNsensitive,
		      sensitive && ! Select_All_Views ? True : False,
                      NULL);
#endif        
        return;
}

static void
SelectWorkstationCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
        )
{
	XmAnyCallbackStruct	*cbs = (XmAnyCallbackStruct*)cbdata;
	char	func[] = "SelectWorkstationCB";
	NgPrint	l = (NgPrint)udata;
        NgPrintPart *pp = &l->print;
        NrmQuark qsym;
        int i,id,id_count;
        int *id_array;
	NhlBoolean sensitive;

        XtVaGetValues(w,
                      XmNuserData,&qsym,
                      NULL);
        id = NgNclGetHluObjId
                (l->go.nclstate,NrmQuarkToString(qsym),
                 &id_count,&id_array);
        if (id_count > 1)
                NhlFree(id_array);

        if (id == pp->selected_work_id)
                return;
        pp->selected_work_id = id;
        for (i = 0; i< pp->wks_pb_count; i++) {
                if (w == pp->wks_pb[i]) {
                        pp->selected_work_pos = i;
                        break;
                }
        }
	Select_All_Views = True;
        UpdateViewListBox(l,NULL);
	
	
	sensitive = pp->work_view_count > 0 && Select_All_Views? True : False;
	XtVaSetValues(XmMessageBoxGetChild(l->go.manager,XmDIALOG_OK_BUTTON),
		      XmNsensitive,sensitive,
		      NULL);
	return;
	
}

static void
UpdateGlobalSettings
(
	NgPrint	l
)
{
        NgPrintPart *pp = &l->print;

/*
 * For now set Select_All_Views true everytime we update. This is until
 * we figure out how to handle the concept of a globally selected workstation
 * versus this dialog. The workstation that appears each time the dialog
 * pops up is the NgSelectedWork. Right now there is no way to change it.
 */
	Select_All_Views = True;
	XtVaSetValues(pp->file_text,
		      XmNvalue,FileTypes[File_Type].output_pathname,
		      XmNsensitive,To_Printer ? False : True,
		      NULL);
	XtVaSetValues(pp->print_text,
		      XmNvalue,Print_Command,
		      XmNsensitive,To_Printer ? True : False,
		      NULL);
	XtVaSetValues(pp->file_tgl,XmNset,To_Printer ? False : True, NULL);
	XtVaSetValues(pp->print_tgl,XmNset,To_Printer ? True : False, NULL);
	if (To_Printer)
		XmProcessTraversal(pp->print_text, XmTRAVERSE_CURRENT);
	else
		XmProcessTraversal(pp->file_text, XmTRAVERSE_CURRENT);

	XtVaSetValues
		(pp->file_type_lbl,
		 XmNsensitive,To_Printer ? False : True,
		 NULL);
	XtVaSetValues
		(pp->file_type_optmenu,
		 XmNsensitive,To_Printer ? False : True,
		 XmNmenuHistory,pp->file_type_pbs[File_Type],
		 NULL);
	XtVaSetValues
		(XmOptionButtonGadget(pp->file_type_optmenu),
		 XmNsensitive,To_Printer ? False : True,
		 NULL);
	XtVaSetValues
		(pp->overwrite_tgl,
		 XmNsensitive,To_Printer ? False : True,
		 XmNset, Overwrite_Warning ? True : False, NULL);

	UpdateLayoutDialog(l);

	return;
}		

                
static void
UpdateSourceDialog
(
	NgGO		go,
        Widget		top,
	Dimension 	max_label_width
)
{
	NgPrint		l = (NgPrint) go;
	NgPrintPart	*np = &l->print;
	NhlBoolean	create;
        Widget		menush;
        int		i;
	NhlBoolean	sensitive;

	UpdateHLUObjectList(go);

	create = top ? True : False;

	if (create) {
		menush = XtVaCreatePopupShell("wksMenush",
					xmMenuShellWidgetClass,np->form_parent,
			XmNwidth,		5,
			XmNheight,		5,
			XmNallowShellResize,	True,
			XmNoverrideRedirect,	True,
			XmNdepth,		XcbGetDepth(l->go.xcb),
			XmNcolormap,		XcbGetColormap(l->go.xcb),
			XmNvisual,		XcbGetVisual(l->go.xcb),
			NULL);

                np->wks_menu = XtVaCreateWidget
                        ("wksMenu",xmRowColumnWidgetClass,menush,
                         XmNrowColumnType,XmMENU_PULLDOWN,
                         NULL);
        }
        if (np->wks_pb_alloc < MAX(10,np->wks_count)) {
                np->wks_pb_alloc = MAX(10,np->wks_count);
                np->wks_pb = NhlRealloc(np->wks_pb,
                                        sizeof(Widget) * np->wks_pb_alloc);
        }
        for (i = MAX(1,np->wks_count); i < np->wks_pb_count; i++) {
                XtDestroyWidget(np->wks_pb[i]);
                np->wks_pb_count = MAX(1,np->wks_count);
        }

	if (! np->wks_count) {
		XmString label = NgXAppCreateXmString
			(l->go.appmgr,"None");
                if (np->wks_pb_count < 1) {
                        np->wks_pb[0] = XtVaCreateManagedWidget
                                ("wks_pb",
                                 xmPushButtonGadgetClass,np->wks_menu,
                                 XmNlabelString,label,
                                 XmNalignment,XmALIGNMENT_CENTER,
                                 NULL);
                        np->wks_pb_count++;
                }
                else {
                        XtVaSetValues(np->wks_pb[0],
                                      XmNlabelString,label,
                                      NULL);
                }
		NgXAppFreeXmString(l->go.appmgr,label);
	}
	else {
		for (i = 0; i < np->wks_count; i++) {
			XmString label = NgXAppCreateXmString
				(l->go.appmgr,NrmQuarkToString(np->qwks[i]));
                        if (np->wks_pb_count <= i) {
                                np->wks_pb[i] = XtVaCreateManagedWidget
                                        ("wks_pb",
                                         xmPushButtonGadgetClass,np->wks_menu,
                                         XmNlabelString,label,
                                         XmNalignment,XmALIGNMENT_CENTER,
                                         XmNuserData,np->qwks[i],
                                         NULL);
                                np->wks_pb_count++;
                                XtAddCallback(np->wks_pb[i],
                                              XmNactivateCallback,
                                              SelectWorkstationCB,l); 
                        }
                        else {
                                XtVaSetValues(np->wks_pb[i],
                                              XmNlabelString,label,
                                              XmNuserData,np->qwks[i],
                                              NULL);
                        }
			NgXAppFreeXmString(l->go.appmgr,label);
		}
	}
        if (create) {
                Widget 		work_lbl,plot_views_lbl,labelg;
		Arg		arg[12];
		int		ac = 0;
		int		wc = 0;
		Widget		w[12];
		Dimension	work_width,plot_views_width;
                
		w[wc++] = work_lbl = XtCreateWidget
			("workstationLbl",
			 xmLabelGadgetClass,np->form_parent,arg,0);
		w[wc++] = np->plot_views_lbl = plot_views_lbl = XtCreateWidget
			("plotViewsLbl",
			 xmLabelGadgetClass,np->form_parent,arg,0);

		XtVaGetValues(work_lbl,XmNwidth,&work_width,NULL);
		max_label_width = MAX(work_width,max_label_width);
		XtVaGetValues(plot_views_lbl,XmNwidth,&plot_views_width,NULL);
		max_label_width = MAX(plot_views_width,max_label_width);

		w[wc++] = np->wks_optmenu = XtVaCreateWidget
                        ("wksoptMenu",
                         xmRowColumnWidgetClass,np->form_parent,
                         XmNspacing,0,
                         XmNtopAttachment,XmATTACH_WIDGET,
                         XmNtopWidget,top,
                         XmNleftAttachment,XmATTACH_FORM,
                         XmNleftOffset,max_label_width,
                         XmNrightAttachment,XmATTACH_NONE,
                         XmNbottomAttachment,XmATTACH_NONE,
                         XmNrowColumnType,XmMENU_OPTION,
                         XmNmenuHistory,np->wks_pb[np->selected_work_pos],
                         XmNsubMenuId,np->wks_menu,
                         NULL);

                w[wc++] = np->all_tgl = XtVaCreateWidget
                        ("selectAllTgl",
                         xmToggleButtonGadgetClass,np->form_parent,
                         XmNtopAttachment,XmATTACH_WIDGET,
                         XmNtopWidget,np->wks_optmenu,
                         XmNleftAttachment,XmATTACH_FORM,
			 XmNleftOffset,max_label_width,
                         XmNrightAttachment,XmATTACH_NONE,
                         XmNbottomAttachment,XmATTACH_NONE,
                         XmNsensitive,False,
                         XmNset,Select_All_Views ? True : False,
			 XmNindicatorType,XmONE_OF_MANY,
                         NULL);
                XtAddCallback(np->all_tgl,
                              XmNvalueChangedCallback,
                              SelectAllViewsCB,l); 
                
                w[wc++] = np->selected_tgl = XtVaCreateWidget
                        ("selectedViewsTgl",
                         xmToggleButtonGadgetClass,np->form_parent,
                         XmNtopAttachment,XmATTACH_WIDGET,
                         XmNtopWidget,np->wks_optmenu,
                         XmNleftAttachment,XmATTACH_WIDGET,
			 XmNleftWidget,np->all_tgl,
                         XmNrightAttachment,XmATTACH_NONE,
                         XmNbottomAttachment,XmATTACH_NONE,
                         XmNsensitive,False,
                         XmNset,Select_All_Views ? False : True,
			 XmNindicatorType,XmONE_OF_MANY,
                         NULL);
                XtAddCallback(np->selected_tgl,
                              XmNvalueChangedCallback,
                              SelectAllViewsCB,l); 

		XtVaSetValues
			(work_lbl,
			 XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET,
			 XmNtopWidget,np->wks_optmenu,
			 XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
			 XmNbottomWidget,np->wks_optmenu,
			 XmNleftAttachment,XmATTACH_FORM, 
			 XmNleftOffset,max_label_width - work_width,
			 XmNrightAttachment, XmATTACH_NONE,
			 NULL);

		XtVaSetValues
			(plot_views_lbl,
			 XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET,
			 XmNtopWidget,np->all_tgl,
			 XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
			 XmNbottomWidget,np->all_tgl,
			 XmNleftAttachment,XmATTACH_FORM, 
			 XmNleftOffset,max_label_width - plot_views_width,
			 XmNrightAttachment, XmATTACH_NONE,
                         XmNsensitive,False,
			 NULL);
		
		XtManageChildren(w,wc);
                labelg = XmOptionLabelGadget(np->wks_optmenu);
                if(labelg)
                        XtUnmanageChild(labelg);
        }
        else {
                XtVaSetValues
			(np->wks_optmenu,
			 XmNmenuHistory,np->wks_pb[np->selected_work_pos],
			 NULL);
        }

        UpdateViewListBox(l,create ? np->all_tgl : NULL);

	sensitive = np->work_view_count > 0 ? True : False;
	if (sensitive && ! Select_All_Views) {
		int count;
		XtVaGetValues(np->view_list,
			      XmNselectedItemCount,&count,
			      NULL);
		if (! count)
			sensitive = False;
	}
	XtVaSetValues(XmMessageBoxGetChild(l->go.manager,XmDIALOG_OK_BUTTON),
		      XmNsensitive,sensitive,
		      NULL);


        return;
        
}
static void
UpdateDialog
(
	NgPrint	l
)
{
	NgPrintPart	*np = &l->print;

	UpdateGlobalSettings(l);

	UpdateSourceDialog((NgGO)l,NULL,0);

        return;
}
static void
VisibilityEH
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
	NgPrint	l = (NgPrint)udata;
        

#if DEBUG_PRINT
        printf("in visibility eh\n");
#endif        

        if (! l->print.up) {
		UpdateDialog(l);
#if DEBUG_PRINT
                printf("grabbing focus\n");
#endif        
                NgAppGrabFocus(l->go.appmgr,l->base.id);
                l->print.up = True;
        }

	return;
}

static void 
CreateLayoutDialog(
	NgGO		go,
	Widget		parent,
	Widget		top_widget,
	Widget  	*bottom_widget,
	Dimension	*max_label_width
)
{
	NgPrintPart	*np = &((NgPrint)go)->print;
        Widget		form,label,printer,file;
	Widget		pb,selected_pb;
        Widget		menush,menu,optmenu;
        Widget		maximize,sep;
        int		i;
	Widget		orientation_lbl,plot_bounds_lbl,paper_size_lbl,
			resolution_lbl;
	Dimension	orientation_width,plot_bounds_width,
			paper_size_width,resolution_width,max_width;
	Arg		arg[30];
	int		ac = 0;
	int		wc = 0;
	Widget		w[30];
	char		res_text[12];

            /* orientation  */

        w[wc++] = np->orient_lbl = orientation_lbl = XtCreateWidget
		("orientationLbl",xmLabelGadgetClass,parent,arg,0);
        w[wc++] = np->plot_bounds_lbl = plot_bounds_lbl = XtCreateWidget
		("plotBoundsLbl",xmLabelGadgetClass,parent,arg,0);
        w[wc++] = np->paper_type_lbl = paper_size_lbl = XtCreateWidget
		("paperSizeLbl",xmLabelGadgetClass,parent,arg,0);
        w[wc++] = np->resolution_lbl = resolution_lbl = XtCreateWidget
		("resolutionLbl",xmLabelGadgetClass,parent,arg,0);
	
	XtVaGetValues(orientation_lbl,XmNwidth,&orientation_width,NULL);
	max_width = MAX(orientation_width,*max_label_width);
	XtVaGetValues(plot_bounds_lbl,XmNwidth,&plot_bounds_width,NULL);
	max_width = MAX(plot_bounds_width,max_width);
	XtVaGetValues(paper_size_lbl,XmNwidth,&paper_size_width,NULL);
	max_width = MAX(paper_size_width,max_width);
	XtVaGetValues(resolution_lbl,XmNwidth,&resolution_width,NULL);
	max_width = MAX(resolution_width,max_width);
	*max_label_width = max_width;

	menush = XtVaCreatePopupShell("orientMenush",
						xmMenuShellWidgetClass,parent,
			XmNwidth,		5,
			XmNheight,		5,
			XmNallowShellResize,	True,
			XmNoverrideRedirect,	True,
			XmNdepth,		XcbGetDepth(go->go.xcb),
			XmNcolormap,		XcbGetColormap(go->go.xcb),
			XmNvisual,		XcbGetVisual(go->go.xcb),
			NULL);

	menu = XtVaCreateWidget("orientMenu",xmRowColumnWidgetClass,menush,
                                XmNrowColumnType,XmMENU_PULLDOWN,
                                NULL);

	np->orient_pbs = NhlMalloc(3 * sizeof(Widget));

	for (i = 0; i < NhlNumber(OrientationTypes); i++) {
		pb = np->orient_pbs[i] = XtVaCreateManagedWidget
			(OrientationTypes[i].wname,
			 xmPushButtonGadgetClass,menu,
			 XmNalignment,XmALIGNMENT_CENTER,
			 XmNuserData,OrientationTypes[i].type,
			 NULL);
		XtAddCallback(pb,XmNactivateCallback,SelectOrientationCB,go); 
	}
        w[wc++] = optmenu = np->orient_optmenu = XtVaCreateWidget
		("orientoptMenu",xmRowColumnWidgetClass,parent,
		 XmNsubMenuId,menu,
		 XmNspacing,0,
		 XmNrowColumnType,XmMENU_OPTION,
		 XmNmenuHistory,np->orient_pbs[Orient],
		 NULL);
	label = XmOptionLabelGadget(optmenu);
	if(label)
		XtUnmanageChild(label);

        w[wc++] = np->maximize_bb_tgl = XtCreateWidget
		("maximizeBBTgl",xmToggleButtonGadgetClass,parent,arg,0);
	XtAddCallback
		(np->maximize_bb_tgl,XmNvalueChangedCallback,MaximizeBBCB,go); 
        w[wc++] = np->full_viewspace_tgl = XtCreateWidget
		("fullViewspaceTgl",xmToggleButtonGadgetClass,parent,arg,0);
	XtAddCallback
		(np->full_viewspace_tgl,
                 XmNvalueChangedCallback,MaximizeBBCB,go); 

	menush = XtVaCreatePopupShell("papersizeMenush",
						xmMenuShellWidgetClass,parent,
			XmNwidth,		5,
			XmNheight,		5,
			XmNallowShellResize,	True,
			XmNoverrideRedirect,	True,
			XmNdepth,		XcbGetDepth(go->go.xcb),
			XmNcolormap,		XcbGetColormap(go->go.xcb),
			XmNvisual,		XcbGetVisual(go->go.xcb),
			NULL);

	menu = XtVaCreateWidget("papersizeMenu",xmRowColumnWidgetClass,menush,
                                XmNrowColumnType,XmMENU_PULLDOWN,
                                NULL);

	np->paper_type_pbs = NhlMalloc(NhlNumber(PaperTypes) * sizeof(Widget));
        for (i = 0; i < NhlNumber(PaperTypes); i++) {
                pb = np->paper_type_pbs[i] = XtVaCreateManagedWidget
			(PaperTypes[i].name,xmPushButtonGadgetClass,menu,
			 XmNalignment,XmALIGNMENT_CENTER,
			 XmNuserData,i,
			 NULL);
                XtAddCallback(pb,XmNactivateCallback,SelectPaperSizeCB,go);
        }

        w[wc++] = optmenu = np->paper_type_optmenu = XtVaCreateWidget
		("papersizeoptMenu",xmRowColumnWidgetClass,parent,
		 XmNsubMenuId,menu,
		 XmNspacing,0,
		 XmNrowColumnType,XmMENU_OPTION,
		 XmNmenuHistory,np->paper_type_pbs[Paper_Type],
		 NULL);

	label = XmOptionLabelGadget(optmenu);
	if(label)
		XtUnmanageChild(label);

        w[wc++] = np->resolution_text = XtCreateWidget
		("resolutionTextF",xmTextFieldWidgetClass,parent,arg,0);

	XtVaSetValues
		(np->orient_optmenu,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,top_widget,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNleftOffset,max_width,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNbottomAttachment,XmATTACH_NONE,
		 NULL);

	XtVaSetValues
		(orientation_lbl,
		 XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		 XmNtopWidget, np->orient_optmenu,
		 XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		 XmNbottomWidget, np->orient_optmenu,
		 XmNleftAttachment, XmATTACH_FORM, 
		 XmNleftOffset, max_width - orientation_width,
		 XmNrightAttachment, XmATTACH_NONE,
		 NULL);



	XtVaSetValues
		(np->paper_type_optmenu,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,np->orient_optmenu,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNleftOffset,max_width,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNbottomAttachment,XmATTACH_NONE,
		 NULL);

	XtVaSetValues
		(paper_size_lbl,
		 XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		 XmNtopWidget,np->paper_type_optmenu,
		 XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		 XmNbottomWidget,np->paper_type_optmenu,
		 XmNleftAttachment, XmATTACH_FORM, 
		 XmNleftOffset,max_width - paper_size_width,
		 XmNrightAttachment, XmATTACH_NONE,
		 NULL);

        XtVaSetValues
		(np->maximize_bb_tgl,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,np->paper_type_optmenu,
		 XmNbottomAttachment,XmATTACH_NONE,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNleftOffset,max_width,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNorientation,XmHORIZONTAL,
		 XmNindicatorType,XmONE_OF_MANY,
		 NULL);

        XtVaSetValues
		(np->full_viewspace_tgl,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,np->paper_type_optmenu,
		 XmNbottomAttachment,XmATTACH_NONE,
		 XmNleftAttachment,XmATTACH_WIDGET,
		 XmNleftWidget,np->maximize_bb_tgl,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNorientation,XmHORIZONTAL,
		 XmNindicatorType,XmONE_OF_MANY,
		 NULL);

	XtVaSetValues
		(plot_bounds_lbl,
		 XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		 XmNtopWidget,np->maximize_bb_tgl,
		 XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		 XmNbottomWidget,np->maximize_bb_tgl,
		 XmNleftAttachment, XmATTACH_FORM, 
		 XmNleftOffset, max_width - plot_bounds_width,
		 XmNrightAttachment, XmATTACH_NONE,
		 NULL);

	sprintf(res_text,"%d",Resolution);
	XtVaSetValues
		(np->resolution_text,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,np->maximize_bb_tgl,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNleftOffset,max_width,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNbottomAttachment,XmATTACH_NONE,
		 XmNvalue,res_text,
		 NULL);

	XtVaSetValues
		(resolution_lbl,
		 XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		 XmNtopWidget,np->resolution_text,
		 XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		 XmNbottomWidget,np->resolution_text,
		 XmNleftAttachment, XmATTACH_FORM, 
		 XmNleftOffset,max_width - resolution_width,
		 XmNrightAttachment, XmATTACH_NONE,
		 NULL);

	UpdateLayoutDialog((NgPrint)go);
	XtManageChildren(w,wc);

	label = XmOptionLabelGadget(np->orient_optmenu);
	if(label)
	  	XtUnmanageChild(label);

	label = XmOptionLabelGadget(np->paper_type_optmenu);
	if(label)
	  	XtUnmanageChild(label);

	sep = XtVaCreateManagedWidget
		("sep",xmSeparatorGadgetClass,parent,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,np->resolution_text,
		 XmNbottomAttachment,XmATTACH_NONE,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_FORM,
		 NULL);
	*bottom_widget = sep;

	return;
}

static void 
CreateDestinationDialog(
	NgGO		go,
	Widget		parent,
	Widget  	*bottom_widget,
	Dimension	*max_label_width
)
{
	NgPrintPart	*np = &((NgPrint)go)->print;
	NhlErrorTypes   ret = NhlNOERROR;
	Arg		arg[30];
	int		ac = 0;
	int		wc = 0;
	Widget		w[30];
        Widget		label,printer,file;
        Widget		print_destination_lbl,print_command_lbl,
	  		filename_lbl,filetype_lbl;
	Widget		print_command_text,filename_text;
	Widget		pb,selected_pb;
        Widget		menush,menu,optmenu;
        Widget		overwrite,maximize,sep,folder;
        int		i;
	Dimension	print_destination_width,print_command_width,
			filename_width,filetype_width,max_width;

/* 
 * The destination part of the dialog 
 */
        w[wc++] = print_destination_lbl = XtCreateWidget
		("printDestinationLbl",xmLabelGadgetClass,parent,arg,0);
        w[wc++] = printer = np->print_tgl = XtCreateWidget
		("printerTgl",xmToggleButtonGadgetClass,parent,arg,0);
        w[wc++] = file = np->file_tgl = XtCreateWidget
		("fileTgl",xmToggleButtonGadgetClass,parent,arg,0);
        w[wc++] = np->print_text_lbl = print_command_lbl = XtCreateWidget
		("printCommandLbl",xmLabelGadgetClass,parent,arg,0);
        w[wc++] = np->print_text = print_command_text = XtCreateWidget
		("printCommandTextF",xmTextFieldWidgetClass,parent,arg,0);
        w[wc++] = np->file_text_lbl = filename_lbl = XtCreateWidget
		("fileNameLbl",xmLabelGadgetClass,parent,arg,0);
        w[wc++] = np->file_text = filename_text = XtCreateWidget
		("fileNameTextF",xmTextFieldWidgetClass,parent,arg,0);
        w[wc++] = np->file_type_lbl = filetype_lbl = XtCreateWidget
		("fileTypeLbl",xmLabelGadgetClass,parent,arg,0);

	menush = XtVaCreatePopupShell("orientMenush",xmMenuShellWidgetClass,
									 parent,
			XmNwidth,		5,
			XmNheight,		5,
			XmNallowShellResize,	True,
			XmNoverrideRedirect,	True,
			XmNdepth,		XcbGetDepth(go->go.xcb),
			XmNcolormap,		XcbGetColormap(go->go.xcb),
			XmNvisual,		XcbGetVisual(go->go.xcb),
			NULL);
	
	menu = XtVaCreateWidget
		("oftypeMenu",xmRowColumnWidgetClass,menush,
		 XmNrowColumnType,XmMENU_PULLDOWN,
		 NULL);
	np->file_type_pbs = NhlMalloc(NhlNumber(FileTypes) * sizeof(Widget));
        for (i = 0; i < NhlNumber(FileTypes); i++) {
                pb = np->file_type_pbs[i] = XtVaCreateManagedWidget
			(FileTypes[i].name,
			 xmPushButtonGadgetClass,menu,
			 XmNalignment,XmALIGNMENT_CENTER,
			 XmNuserData,i,
			 NULL);
                XtAddCallback
			(pb,XmNactivateCallback,SelectFileTypeCB,go);
        }
        w[wc++] = optmenu = np->file_type_optmenu = XtVaCreateWidget
		("oftypeoptMenu",xmRowColumnWidgetClass,parent,
		 XmNsubMenuId,menu,
		 XmNspacing,0,
		 XmNrowColumnType,XmMENU_OPTION,
		 XmNmenuHistory,np->file_type_pbs[File_Type],
		 NULL);

	
	XtVaGetValues
		(print_destination_lbl,XmNwidth,&print_destination_width,NULL);
	max_width = print_destination_width;
	XtVaGetValues(print_command_lbl,XmNwidth,&print_command_width,NULL);
	max_width = MAX(print_command_width,max_width);
	XtVaGetValues(filename_lbl,XmNwidth,&filename_width,NULL);
	max_width = MAX(filename_width,max_width);
	XtVaGetValues(filetype_lbl,XmNwidth,&filetype_width,NULL);
	max_width = MAX(filetype_width,max_width);
	*max_label_width = max_width;

	XtVaSetValues
		(print_destination_lbl,
		 XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		 XmNtopWidget, printer,
		 XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		 XmNbottomWidget, printer,
		 XmNleftAttachment, XmATTACH_FORM, 
		 XmNleftOffset, max_width - print_destination_width,
		 XmNrightAttachment, XmATTACH_NONE,
		 NULL);

        XtVaSetValues(printer,
		      XmNtopAttachment,XmATTACH_FORM,
		      XmNbottomAttachment,XmATTACH_NONE,
		      XmNleftAttachment,XmATTACH_FORM,
		      XmNleftOffset,max_width,
		      XmNrightAttachment,XmATTACH_NONE,
		      XmNorientation,XmHORIZONTAL,
		      XmNuserData,prPRINTER,
		      XmNset,To_Printer ? True : False,
		      XmNindicatorType,XmONE_OF_MANY,
		      NULL);
	XtAddCallback(printer,XmNvalueChangedCallback,
		      ChooseOutputDestinationCB,go); 
        XtVaSetValues(file,
		      XmNtopAttachment,XmATTACH_FORM,
		      XmNbottomAttachment,XmATTACH_NONE,
		      XmNleftAttachment,XmATTACH_WIDGET,
		      XmNleftWidget,printer,
		      XmNrightAttachment,XmATTACH_NONE,
		      XmNuserData,prPRINTER,
		      XmNset,To_Printer ? False : True,
		      XmNindicatorType,XmONE_OF_MANY,
		      NULL);
	XtAddCallback(file,XmNvalueChangedCallback,
		      ChooseOutputDestinationCB,go); 

	XtVaSetValues(print_command_lbl,
		      XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNtopWidget, print_command_text,
		      XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNbottomWidget, print_command_text,
		      XmNleftAttachment, XmATTACH_FORM, 
		      XmNleftOffset, max_width - print_command_width,
		      XmNrightAttachment, XmATTACH_NONE,
		      XmNsensitive,To_Printer ? True : False,
		      NULL);

        Print_Command = XtMalloc(strlen(Default_Print_Command));
        strcpy(Print_Command,Default_Print_Command);
        
	XtVaSetValues(print_command_text,
		      XmNtopAttachment,XmATTACH_WIDGET,
		      XmNtopWidget,printer,
		      XmNleftAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNleftWidget, printer,
		      XmNrightAttachment,XmATTACH_FORM,
		      XmNbottomAttachment,XmATTACH_NONE,
		      XmNvalue,Print_Command,
		      XmNsensitive,To_Printer ? True : False,
		      NULL);

	XtVaSetValues(filename_lbl,
		      XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNtopWidget, filename_text,
		      XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNbottomWidget, filename_text,
		      XmNleftAttachment, XmATTACH_FORM, 
		      XmNleftOffset, max_width - filename_width,
		      XmNrightAttachment, XmATTACH_NONE,
		      XmNsensitive,To_Printer ? False : True,
		      NULL);

	XtVaSetValues(filename_text,
		      XmNtopAttachment,XmATTACH_WIDGET,
		      XmNtopWidget,print_command_text,
		      XmNleftAttachment,XmATTACH_FORM,
		      XmNleftOffset,max_width,
		      XmNrightAttachment,XmATTACH_FORM,
		      XmNbottomAttachment,XmATTACH_NONE,
		      XmNvalue,FileTypes[File_Type].output_pathname,
		      XmNsensitive,To_Printer ? False : True,
		      NULL);

	XtVaSetValues(optmenu,
		      XmNtopAttachment,XmATTACH_WIDGET,
		      XmNtopWidget,filename_text,
		      XmNleftAttachment,XmATTACH_OPPOSITE_WIDGET,
		      XmNleftWidget,filename_text,
		      XmNrightAttachment,XmATTACH_NONE,
		      XmNbottomAttachment,XmATTACH_NONE,
		      XmNsensitive,To_Printer ? False : True,
		      NULL);
	XtVaSetValues(XmOptionButtonGadget(optmenu),
		      XmNsensitive,To_Printer ? False : True,
		      NULL);

	XtVaSetValues(filetype_lbl,
		      XmNtopAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNtopWidget,optmenu,
		      XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
		      XmNbottomWidget, optmenu,
		      XmNleftAttachment, XmATTACH_FORM, 
		      XmNleftOffset, max_width - filetype_width,
		      XmNrightAttachment, XmATTACH_NONE,
		      XmNsensitive,To_Printer ? False : True,
		      NULL);

	XtManageChildren(w,wc);

	label = XmOptionLabelGadget(optmenu);
	if(label)
	  	XtUnmanageChild(label);

        overwrite =  np->overwrite_tgl = XtVaCreateManagedWidget
                ("overwriteTgl",xmToggleButtonGadgetClass,
                 parent,
                 XmNtopAttachment,XmATTACH_OPPOSITE_WIDGET,
                 XmNtopWidget,optmenu,
                 XmNbottomAttachment,XmATTACH_OPPOSITE_WIDGET,
                 XmNbottomWidget,optmenu,
                 XmNleftAttachment,XmATTACH_WIDGET,
                 XmNleftWidget,optmenu,
                 XmNrightAttachment,XmATTACH_NONE,
                 XmNset,Overwrite_Warning ? True : False,
		 XmNsensitive,To_Printer ? False : True,
                 NULL);
	XtAddCallback(overwrite,XmNvalueChangedCallback,
		      OverwriteCB,go); 
        
	sep = XtVaCreateManagedWidget
		("sep",xmSeparatorGadgetClass,parent,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,optmenu,
		 XmNbottomAttachment,XmATTACH_NONE,
		 XmNleftAttachment,XmATTACH_FORM,
		 XmNrightAttachment,XmATTACH_FORM,
		 NULL);
	*bottom_widget = sep;

	return;
}

static NhlBoolean
PrintCreateWin
(
	NgGO	go
)
{
	char		func[]="PrintCreateWin";
	NgPrintPart	*np = &((NgPrint)go)->print;
	NhlErrorTypes   ret = NhlNOERROR;
        Widget		form;
        Widget		folder;
	Dimension	max_label_width;
	Widget		bottom_widget;

	XtVaSetValues(go->go.manager,
		      XmNresizePolicy,	XmRESIZE_GROW,
		      NULL);

	XtUnmanageChild(XmMessageBoxGetChild(go->go.manager,
                                               XmDIALOG_HELP_BUTTON));
	XtAddCallback(go->go.manager,XmNokCallback,PrintScriptOkCB,
                      (XtPointer)go);
	XtAddCallback(go->go.manager,XmNcancelCallback,CancelCB,
                      (XtPointer)go);
        XtAddEventHandler(go->go.manager,VisibilityChangeMask,
                          False,VisibilityEH,(XtPointer)go);
#if 0
        form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,go->go.manager,NULL);

	folder = XtVaCreateManagedWidget
                ("Folder",xmlFolderWidgetClass,form,
                 XmNbottomAttachment,XmATTACH_FORM,
                 XmNrightAttachment,XmATTACH_FORM,
                 XmNleftAttachment,XmATTACH_FORM,
                 XmNtopAttachment,XmATTACH_FORM,
                 NULL);
#endif
	
        form = XtVaCreateWidget("form",xmFormWidgetClass,go->go.manager,
				NULL);
        np->form_parent = form;

	CreateDestinationDialog(go,form,&bottom_widget,&max_label_width);

	CreateLayoutDialog(go,form,bottom_widget,
			   &bottom_widget,&max_label_width);


 	UpdateSourceDialog(go,bottom_widget,max_label_width);

	XtManageChild(form);

	return True;
}

