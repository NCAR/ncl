/*
 *      $Id: Error.c,v 1.7 1994-02-08 20:15:32 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Error.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 2 14:01:23 MDT 1992
 *
 *	Description:	This file contains the error message functions
 *			used by the hlu's.  It has functionality simular
 *			to the ESprintf procedures, but an error object
 *			is formed to hide the implimentation from the
 *			user as well as to allow the user to configure
 *			the error system in the same way they configure
 *			plots.  (ie. by setting resources)
 *			It also incompases some added functionality not
 *			addressed by ESprintf - by allowing the user to send
 *			error messages to a specific file descriptor. And by
 *			default the error messages will be sent to stderr.
 *			Many of the functions in this file are based on
 *			the ESprintf functions written by John Clyne.
 */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <ncarg/hlu/ErrorP.h>
#include <ncarg/hlu/VarArg.h>
#include <ncarg/hlu/Converters.h>

#define	MAXERRMSGLEN	1024
#define	TABLELISTINC	10
#define	ERRLISTINC	32

/************************************************************************
*									*
*	Error Class definition's and declarations			*
*									*
************************************************************************/

/* Resources */
#define Oset(field)	NhlOffset(NhlErrorLayerRec,error.field)
static NhlResource resources[] = {
	{NhlNerrBuffer,NhlCerrBuffer,NhlTBoolean,sizeof(NhlBoolean),
		Oset(buffer_errors),NhlTImmediate,False},
	{NhlNerrLevel,NhlCerrLevel,NhlTErrorTypes,sizeof(NhlErrorTypes),
		Oset(error_level),NhlTImmediate,(NhlPointer)NhlWARNING},
	{NhlNerrPrint,NhlCerrPrint,NhlTBoolean,sizeof(NhlBoolean),
		Oset(print_errors),NhlTImmediate,(NhlPointer)True},
	{NhlNerrFileName,NhlCerrFileName,NhlTString,sizeof(NhlString),
		Oset(error_file),NhlTImmediate,(NhlPointer)"stderr"},
};
#undef Oset

/* Methode declarations	*/

static NhlErrorTypes ErrorClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes ErrorInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
#endif
);

static NhlErrorTypes ErrorSetValues(
#if	NhlNeedProto
	NhlLayer	old,		/* old		*/
	NhlLayer	req,		/* requested	*/
	NhlLayer	new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
#endif
);

static NhlErrorTypes ErrorDestroy(
#if	NhlNeedProto
	NhlLayer	l	/* layer to destroy	*/
#endif
);

/* Class definition	*/

NhlErrorLayerClassRec NhlerrorLayerClassRec = {
	/* BaseClassPart */
	{
/* class_name			*/	"Error",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlErrorLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlobjLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	ErrorClassInitialize,
/* layer_initialize		*/	ErrorInitialize,
/* layer_set_values		*/	ErrorSetValues,
/* layer_set_values_hook	*/	ErrorSetValues,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	ErrorDestroy
	},
	/* ErrorClassPart */
	{
/* num_error_instances		*/	0
	}
};

NhlLayerClass NhlerrorLayerClass = (NhlLayerClass)&NhlerrorLayerClassRec;

/************************************************************************
* New type converters - needed for new type's defined for this class	*
* Added to the converter table by ErrorClassInitialize			*
************************************************************************/

/************************************************************************
*									*
*	Error Class Methode definitions					*
*									*
************************************************************************/
/*
 * Function:	ErrorClassInitialize
 *
 * Description:	This function initializes the Error Class structure
 *
 * In Args:	none
 *
 * Out Args:	none
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
ErrorClassInitialize
#if	__STDC__
(
	void
)
#else
()
#endif
{
	NhlConvertArg	errtypes[] = {
				{NhlSTRENUM,	NhlFATAL,	"fatal"},
				{NhlSTRENUM,	NhlWARNING,	"warning"},
				{NhlSTRENUM,	NhlINFO,	"info"},
				{NhlSTRENUM,	NhlNOERROR,	"noerror"}
				};
	NhlConvertArg	interrtypes[] = {
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)NhlFATAL},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)NhlWARNING},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)NhlINFO},
			{NhlIMMEDIATE,	sizeof(int),	(NhlPointer)NhlNOERROR}
				};

	/*
	 *	Install converters to the NhlErrorTypes enum.
	 */
	NhlRegisterConverter(NhlTString,NhlTErrorTypes,NhlCvtStringToEnum,
				errtypes,NhlNumber(errtypes),False,NULL);
	NhlRegisterConverter(NhlTInteger,NhlTErrorTypes,NhlCvtIntToEnum,
				interrtypes,NhlNumber(interrtypes),False,NULL);
	NhlRegisterConverter(NhlTFloat,NhlTErrorTypes,NhlCvtFloatToEnum,
				interrtypes,NhlNumber(interrtypes),False,NULL);

	return NhlNOERROR;
}

/*
 * Function:	ErrorInitialize
 *
 * Description:	This function initializes an instance of an Error class
 *
 * In Args:	
 *	NhlLayerClass	lc,	class
 *	NhlLayer	req,	requested
 *	NhlLayer	new,	new
 *	_NhlArgList	args,	args
 *	int		nargs	nargs
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
ErrorInitialize
#if	__STDC__
(
	NhlLayerClass	lc,	/* class	*/
	NhlLayer	req,	/* requested	*/
	NhlLayer	new,	/* new		*/
	_NhlArgList	args,	/* args		*/
	int		nargs	/* nargs	*/
)
#else
(lc,req,new,args,nargs)
	NhlLayerClass	lc;	/* class	*/
	NhlLayer	req;	/* requested	*/
	NhlLayer	new;	/* new		*/
	_NhlArgList	args;	/* args		*/
	int		nargs;	/* nargs	*/
#endif
{
	NhlErrorLayerClass	elc = (NhlErrorLayerClass)lc;
	NhlErrorLayer		enew = (NhlErrorLayer)new;
	Const char		*tfname = NULL;
	NhlErrorTypes		ret = NhlNOERROR;

	if(elc->error_class.num_error_instances > 0){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"Only one instance of ErrorClass is supported"));
		return NhlFATAL;
	}
	elc->error_class.num_error_instances = 1;

	tfname = enew->error.error_file;
	if(strcmp(tfname,"stderr") == 0)
		enew->error.error_fp = stderr;
	else if(strcmp(tfname,"stdout") == 0)
		enew->error.error_fp = stdout;
	else{
		tfname = _NhlResolvePath(enew->error.error_file);
		enew->error.error_fp = fopen(tfname,"w");
		if(enew->error.error_fp == NULL){
			NHLPERROR((NhlWARNING,errno,
			"Unable to open error file:%s - Using stderr",tfname));
			ret = MIN(ret,NhlWARNING);
			enew->error.error_fp = stderr;
			tfname = "stderr";
		}
	}

	enew->error.error_file = (char *)NhlMalloc(strlen(tfname) + 1);
	strcpy(enew->error.error_file,tfname);

	enew->error.num_emsgs = 0;
	enew->error.emsgs = NULL;
	enew->error.len_emsgs = 0;

	enew->error.num_etables = 0;
	enew->error.etables = NULL;
	enew->error.len_etables = 0;

	return(ret);
}

/*
 * Function:	ErrorSetValues
 *
 * Description:	This is the setvalues methode for the errorclass hlu.
 *
 * In Args:	
 *		NhlLayer		old,		old
 *		NhlLayer		req,		requested
 *		_NhlArgList	args,		args to set
 *		int		nargs		nargs
 *
 * Out Args:	
 *		NhlLayer		new,		new
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
ErrorSetValues
#if	__STDC__
(
	NhlLayer		old,		/* old		*/
	NhlLayer		req,		/* requested	*/
	NhlLayer		new,		/* new		*/
	_NhlArgList	args,		/* args to set	*/
	int		nargs		/* nargs	*/
)
#else
(old,req,new,args,nargs)
	NhlLayer		old;		/* old		*/
	NhlLayer		req;		/* requested	*/
	NhlLayer		new;		/* new		*/
	_NhlArgList	args;		/* args to set	*/
	int		nargs;		/* nargs	*/
#endif
{
	NhlErrorLayer eold = (NhlErrorLayer)old;
	NhlErrorLayer ereq = (NhlErrorLayer)req;
	NhlErrorLayer enew = (NhlErrorLayer)new;
	char *tfname = NULL;
	FILE *tfptr = NULL;
	NhlErrorTypes ret = NhlNOERROR;


	if(eold->error.error_file != ereq->error.error_file){
		tfname = ereq->error.error_file;
		if(tfname == NULL){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"Unable to open file NULL, Using stderr");
			ret = MIN(ret,NhlWARNING);
			tfname = "stderr";
		}

		if(strcmp(tfname,"stderr") == 0)
			tfptr = stderr;
		else if(strcmp(tfname,"stdout") == 0)
			tfptr = stdout;
		else{
			tfptr = fopen(_NhlResolvePath(tfname),"w");
			if(tfptr == NULL){
				NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				"Unable to open %s,Using stderr",tfname));
				ret = MIN(ret,NhlWARNING);
				tfptr = stderr;
				tfname = "stderr";
			}
		}

		if((eold->error.error_fp != stderr) &&
					(eold->error.error_fp != stdout)){
			if(fclose(eold->error.error_fp) != 0){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
						"Error Closing File %s",
						eold->error.error_file);
				ret = MIN(ret,NhlWARNING);
			}
		}

		(void)NhlFree(eold->error.error_file);
		eold->error.error_file = NULL;
		enew->error.error_file = (char *)NhlMalloc(strlen(tfname) + 1);
		strcpy(enew->error.error_file,tfname);
		enew->error.error_fp = tfptr;
	}

	return ret;
}

/*
 * Function:	ErrorDestroy
 *
 * Description:	This function free's any memory that has been alocated
 *		on behalf of the ErrorClass instance.
 *
 * In Args:	NhlLayer	l	The layer to destroy
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
ErrorDestroy
#if	__STDC__
(
	NhlLayer l		/* layer to ready for destruction	*/
)
#else
(l)
	NhlLayer l;	/* layer to ready for destruction	*/
#endif
{
	NhlErrorLayer	el = (NhlErrorLayer)l;
	NhlErrorTypes	ret = NhlNOERROR;
	int		i;

	if ((el->error.error_fp != stdout) && (el->error.error_fp != stderr)){
		if(fclose(el->error.error_fp) != 0){
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				"error closing file:%s",el->error.error_file));
			ret = MIN(ret,NhlWARNING);
		}
	}

	(void)NhlFree(el->error.error_file);
	for(i=0;i < el->error.num_emsgs;i++){
		(void)NhlFree(el->error.emsgs[i].msg);
		(void)NhlFree(el->error.emsgs[i].fname);
	}
	(void)NhlFree(el->error.emsgs);
	(void)NhlFree(el->error.etables);

	return ret;
}

/************************************************************************
*									*
*	Private API for Error handling					*
*									*
************************************************************************/

static int Error_inited = False;
static NhlErrorLayer errorLayer = NULL;

/*
 * Function:	_NhlInitError
 *
 * Description:	This function initializes the error handler for the hlu
 *		library.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	
 * Side Effect:	
 */
void
_NhlInitError
#if	__STDC__
(
	void
)
#else
()
#endif
{
	extern int	sys_nerr;
	extern char	*sys_errlist[];

	if(!Error_inited){
		int tmp;
		NhlErrorTypes ret = NhlNOERROR;

		ret = NhlVACreate(&tmp,"error",NhlerrorLayerClass,NhlNOPARENT,
									NULL);
		if(ret < NhlWARNING){
			NHLPERROR((ret,NhlEUNKNOWN,
					"Error Creating ErrorClass object"));
			return;
		}

		errorLayer = (NhlErrorLayer)_NhlGetLayer(tmp);

		Error_inited = True;


		ret = NhlErrAddTable(0,sys_nerr,(Const char **)sys_errlist);
		if (ret != NhlNOERROR){
			NHLPERROR((ret,NhlEUNKNOWN,
					"Error loading System Error Table"));
		}
	}

	return;
}

/*
 * Function:	_NhlCloseError
 *
 * Description:	This function is used to destroy and free any memory associated
 *		with the error reporting module of the hlu library.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	void
 * Side Effect:	
 */
void
_NhlCloseError
#if	__STDC__
(
	void
)
#else
()
#endif
{
	NhlDestroy(errorLayer->base.id);
	errorLayer = NULL;
}

/*
 * Function:	RetrieveSysError
 *
 * Description:	This function retrieves a pointer to the error message
 *		indicated by the specified error number. Or NULL if
 *		that error number doesn't exist.
 *
 * In Args:	
 *		int	errnum	error number to retrieve message for
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	Const char *
 * Side Effect:	
 */
static Const char *
RetrieveSysError
#if	__STDC__
(
	int	errnum	/* error number to retrieve message for	*/
)
#else
(errnum)
	int	errnum;	/* error number to retrieve message for	*/
#endif
{
	int i, error;

	if(Error_inited && (errorLayer != NULL)){
		for(i=0;i < errorLayer->error.num_etables;i++){
			error = errnum - errorLayer->error.etables[i].start;
			if((error >= 0) &&
				(error < errorLayer->error.etables[i].len)){
				return errorLayer->error.etables[i].errs[error];
			}
		}
	}

	return NULL;
}

/*
 * Function:	BufferMsg
 *
 * Description:	This function copies a static message structure into the
 *		error instance's internal data.
 *
 * In Args:	
 *		Const NhlErrMsg	*msg	message to buffer
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
static void
BufferMsg
#if	__STDC__
(
	Const NhlErrMsg	*msg	/* message to buffer	*/
)
#else
(msg)
	Const NhlErrMsg	*msg;	/* message to buffer	*/
#endif
{
	/* increase size of errmsg table if neccessary */
	if(errorLayer->error.len_emsgs < errorLayer->error.num_emsgs + 1){
		errorLayer->error.emsgs = (NhlErrMsgList)
				NhlRealloc(errorLayer->error.emsgs,
				((errorLayer->error.len_etables + ERRLISTINC) *
							sizeof(NhlErrMsg)));
		if(errorLayer->error.emsgs == NULL){
			errorLayer->error.num_emsgs = 0;
			errorLayer->error.len_emsgs = 0;
			errorLayer->error.buffer_errors = False;
			if(!errorLayer->error.print_errors){
				errorLayer->error.print_errors = True;
				errorLayer->error.error_fp = stderr;
			}
			NHLPERROR((NhlWARNING,12,"Unable to buffer e-msgs"));
		}
		else
			errorLayer->error.len_emsgs += ERRLISTINC;
	}

	/*
	 * copy errmsg into buffer
	 */
	errorLayer->error.emsgs[errorLayer->error.num_emsgs] = *msg;
	errorLayer->error.emsgs[errorLayer->error.num_emsgs].msg =
				(char *)NhlMalloc(strlen(msg->msg) + 1);
	strcpy(errorLayer->error.emsgs[errorLayer->error.num_emsgs].msg,
								msg->msg);
	errorLayer->error.num_emsgs++;
}

/*
 * Function:	AddErrMsg
 *
 * Description:	This function uses the internal state of the error instance
 *		to determine which error messages to print out and which
 *		ones to buffer etc...
 *
 * In Args:	
 *		Const NhlErrMsg	*msg	message to add
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	Const char *
 * Side Effect:	
 */
static Const char *
AddErrMsg
#if	__STDC__
(
	Const NhlErrMsg	*msg	/* message to add	*/
)
#else
(msg)
	Const NhlErrMsg	*msg;	/* message to add	*/
#endif
{
	static char buffer[MAXERRMSGLEN];

	/* if error instance not init'ed - print to sterr */
	if(!Error_inited || (errorLayer == NULL))
		return NhlErrFPrintMsg(stderr,msg);

	/* if msg severity > level requested return NULL */
	if(msg->severity > errorLayer->error.error_level){
		return NULL;
	}

	/* buffer message */
	if(errorLayer->error.buffer_errors){
		BufferMsg(msg);
	}

	/* print out message */
	if(errorLayer->error.print_errors &&
		(errorLayer->error.error_fp != NULL)){

		return NhlErrFPrintMsg(errorLayer->error.error_fp,msg);
	}
	else
		return NhlErrSPrintMsg(buffer,msg);
}

/************************************************************************
*									*
*	Public API for Error handling					*
*									*
************************************************************************/
/*
 * These var's are used to support the VarArg macro hack.
 */
static HackUsed = False;
static struct {
	int	line;
	char	fname[_NhlMAXFNAMELEN];
} HackInfo;

/*
 * Function:	_NhlPErrorHack
 *
 * Description:	This function saves line and file information for NhlPerror
 *		to retrieve when it is called.  It is a gross ugly hack
 *		to allow a vararg macro call.
 *
 * In Args:	int	line;		line number
 *		char	*fname;		file name
 *
 * Out Args:	
 *
 * Scope:	Global Private -	It should only be called by the
 *					NHLPERROR macro.
 * Returns:	
 * Side Effect:	
 */
void
_NhlPErrorHack
#if	__STDC__
(
	int		line,	/* line number	*/
	Const char	*fname	/* file name	*/
)
#else
(line,fname)
	int		line;	/* line number	*/
	Const char	*fname;	/* file name	*/
#endif
{
	HackUsed = True;

	HackInfo.line = line;
	strcpy(HackInfo.fname,fname);

	return;
}

/*
 * Function:	NhlPError
 *
 * Description:	This is the function used to report an error.  It takes
 *		a fmt string that works identical to printf.
 *
 * In Args:	
 *		NhlErrorTypes	severity,	error severity
 *		int		errnum,		errornum in table
 *		char		*fmt,		fmt string
 *		...				args for fmt string
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	Const char * - the string it will print or buffer
 * Side Effect:	
 */
Const char *NhlPError
#if     NeedVarArgProto
(
	NhlErrorTypes	severity,	/* error severity	*/
	int		errnum,		/* errornum in table	*/
	char		*fmt,		/* fmt string		*/
	...				/* args for fmt string	*/
)
#else
(severity,errnum,fmt,va_alist)
	NhlErrorTypes	severity;	/* error severity	*/
	int		errnum;		/* errornum in table	*/
	char		*fmt;		/* fmt string		*/
	va_dcl				/* args for fmt string	*/
#endif
{
        va_list         ap;
	char		tbuf[MAXERRMSGLEN];
	NhlErrMsg	tmp;

	tmp.severity = severity;
	tmp.errorno = errnum;

	if(errnum == NhlEUNKNOWN)
		tmp.sysmsg = NULL;
	else
		tmp.sysmsg = RetrieveSysError(errnum);

	if(fmt != NULL){

		VA_START(ap,fmt);
		(void)vsprintf(tbuf, fmt, ap);
		va_end(ap);

		tmp.msg = tbuf;
	}
	else
		tmp.msg = NULL;

	if(HackUsed){
		tmp.line = HackInfo.line;
		tmp.fname = NhlMalloc(strlen(HackInfo.fname) + 1);
		strcpy(tmp.fname,HackInfo.fname);
		HackUsed = False;
	}
	else{
		tmp.line = -1;
		tmp.fname = NULL;
	}

	return AddErrMsg(&tmp);
}


/*
 * Function:	NhlErrGetID
 *
 * Description:	This function returns the pid associated with the errorLayer
 *		object.  This is needed so the application programmer can
 *		set resources in the error object if they want to.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	The PID associated with errorLayer - Or NhlErrorTypes
 *		if errorLayer is unavailable.
 * Side Effect:	
 */
int
NhlErrGetID
#if	__STDC__
(
	void
)
#else
()
#endif
{
	if(Error_inited && (errorLayer != NULL))
		return errorLayer->base.id;

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Can't find Error Object"));
	return NhlFATAL;
}

/*
 * Function:	NhlErrNumMsgs
 *
 * Description:	This function returns the number of error messages that
 *		are currently buffered.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	int number of messages - or NhlErrorTypes on error
 * Side Effect:	
 */
int
NhlErrNumMsgs
#if	__STDC__
(
	void
)
#else
()
#endif
{
	if(Error_inited && (errorLayer != NULL))
		return errorLayer->error.num_emsgs;

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Can't find Error Object"));
	return NhlFATAL;
}

/*
 * Function:	NhlErrGetMsg
 *
 * Description:	This function returns a single error message specified
 *		by the number passed to it.
 *
 * In Args:	
 *		int		msgnum	msg num to retrieve
 *
 * Out Args:	
 *		NhlErrMsg	**msg	return msg ptr
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlErrGetMsg
#if	__STDC__
(
	int		msgnum,	/* msg num to retrieve	*/
	Const NhlErrMsg	**msg	/* return msg		*/
)
#else
(msgnum,msg)
	int		msgnum;	/* msg num to retrieve	*/
	Const NhlErrMsg	**msg;	/* return msg		*/
#endif
{
	if(Error_inited && (errorLayer != NULL)){
		if(((msgnum-1) < errorLayer->error.num_emsgs) && (msgnum > 0)){
			*msg = &errorLayer->error.emsgs[msgnum-1];
			return NhlNOERROR;
		}

		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"message number %d doesn't exist",
								msgnum));
		return NhlFATAL;
	}

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Can't find Error Object"));
	return NhlFATAL;
}

/*
 * Function:	NhlErrClearMsgs
 *
 * Description:	This function clears the internal error buffer that is managed
 *		by the error object.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlErrClearMsgs
#if	__STDC__
(
	void
)
#else
()
#endif
{
	int	i;

	if(Error_inited && (errorLayer != NULL)){
		for(i=0;i < errorLayer->error.num_emsgs;i++){
			(void)NhlFree(errorLayer->error.emsgs[i].msg);
			(void)NhlFree(errorLayer->error.emsgs[i].fname);
		}
		(void)NhlFree(errorLayer->error.emsgs);
		errorLayer->error.emsgs = NULL;
		errorLayer->error.num_emsgs = 0;
		errorLayer->error.len_emsgs = 0;

		return NhlNOERROR;
	}

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Can't find Error Object"));
	return NhlFATAL;
}

/*
 * Function:	NhlErrAddTable
 *
 * Description:	This function adds an error msg table to the error object.
 *		This is the messages that get printed out with respect to
 *		the errno reported to NhlPError function.
 *
 * In Args:	
 *		int	start,		starting number
 *		int	tlen,		table length
 *		char	**etable	table of err messages
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlErrAddTable
#if	__STDC__
(
	int		start,		/* starting number		*/
	int		tlen,		/* table length			*/
	Const char	**etable	/* table of err messages	*/
)
#else
(start,tlen,etable)
	int		start;		/* starting number		*/
	int		tlen;		/* table length			*/
	Const char	**etable;	/* table of err messages	*/
#endif
{
	int i;
	unsigned end;
	unsigned tstart, tend;

	if(Error_inited && (errorLayer != NULL)){

		/*
		 * make sure new table has a valid range
		 */
		for(i=0;i < errorLayer->error.num_etables;i++){
			end = start + tlen;
			tstart = errorLayer->error.etables[i].start;
			tend = tstart + errorLayer->error.etables[i].len;

			if(((start > tstart) && (start < tend)) ||
			   ((end > tstart) && (end < tend))){

				NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"Table being added overlaps a previous table"));
				return NhlFATAL;
			}
		}

		/*
		 * Increase size of table if needed
		 */
		if(errorLayer->error.len_etables <
					errorLayer->error.num_etables + 1){
			errorLayer->error.etables =
				(NhlETable *)NhlRealloc(errorLayer->error.etables,
				(unsigned)((errorLayer->error.len_etables +
					TABLELISTINC) * sizeof(NhlETable)));
			if(errorLayer->error.etables == NULL){
				errorLayer->error.len_etables = 0;
				errorLayer->error.num_etables = 0;

				NHLPERROR((NhlFATAL,errno,
				"Unable to allocate memory for error table"));
				return NhlFATAL;
			}
			else
				errorLayer->error.len_etables += TABLELISTINC;
		}

		/*
		 * Add table into list
		 */
		errorLayer->error.etables[errorLayer->error.num_etables].start =
									start;
		errorLayer->error.etables[errorLayer->error.num_etables].len =
									tlen;
		errorLayer->error.etables[errorLayer->error.num_etables].errs =
									etable;
		errorLayer->error.num_etables++;

		return NhlNOERROR;
	}

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Can't find Error Object"));
	return NhlFATAL;
}

/*
 * Function:	NhlErrSPrintMsg
 *
 * Description:	This function takes an error msg and formats it for printing
 *		It places the resulting string in the buffer provided.
 *
 * In Args:	
 *		Const NhlErrMsg	*msg		message to print
 *
 * Out Args:	
 *		char		*buffer,	buffer to print message to
 *
 * Scope:	Global Public
 * Returns:	Const char *
 * Side Effect:	
 */
char *
NhlErrSPrintMsg
#if	__STDC__
(
	char		*buffer,	/* buffer to print message to	*/
	Const NhlErrMsg	*msg		/* message to print		*/
)
#else
(buffer,msg)
	char		*buffer;	/* buffer to print message to	*/
	Const NhlErrMsg	*msg;		/* message to print		*/
#endif
{
	char tbuf[MAXERRMSGLEN];

	if(msg->severity == NhlNOERROR)
		strcpy(buffer,"NhlNOERROR");
	else if(msg->severity == NhlINFO)
		strcpy(buffer,"NhlINFO");
	else if(msg->severity == NhlWARNING)
		strcpy(buffer,"NhlWARNING");
	else
		strcpy(buffer,"NhlFATAL");
	
	if((msg->line > 0) && (msg->fname != NULL)){
		sprintf(tbuf,":[\"%s\":%d]",msg->fname,msg->line);
		strcat(buffer,tbuf);
	}

	if(msg->msg != NULL){
		strcat(buffer,":");
		strcat(buffer,msg->msg);
	}

	if(msg->errorno != NhlEUNKNOWN){
		sprintf(tbuf,":[errno=%d]",msg->errorno);
		strcat(buffer,tbuf);
	}

	if(msg->sysmsg != NULL){
		strcat(buffer,":");
		strcat(buffer,msg->sysmsg);
	}

	return buffer;
}

/*
 * Function:	NhlErrFPrintMsg
 *
 * Description:	This function takes the given error msg and formats it for
 *		printing using NhlErrSPrintMsg.  And then prints it to
 *		the FILE* provided.
 *
 * In Args:	
 *		FILE		*fp;	file to print to
 *		Const NhlErrMsg	*msg	message to print
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	Const char *
 * Side Effect:	
 */
Const char *
NhlErrFPrintMsg
#if	__STDC__
(
	FILE		*fp,	/* file to print to	*/
	Const NhlErrMsg	*msg	/* message to print	*/
)
#else
(fp,msg)
	FILE		*fp;	/* file to print to	*/
	Const NhlErrMsg	*msg;	/* message to print	*/
#endif
{
	static char tbuf[MAXERRMSGLEN];

	fprintf(fp,"%s\n\r",NhlErrSPrintMsg(tbuf,msg));
	fflush(fp);

	return tbuf;
}
