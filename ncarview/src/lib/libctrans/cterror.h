/*
 *	$Id: cterror.h,v 1.4 1991-03-12 17:35:28 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#ifndef	_cterror_
#define	_cterror_
typedef	enum {
	ALL, TERMINAL, NON_TERM
	} Report;

typedef	enum {
	ACT, NO_ACT
	} Action;

typedef	enum {
	DIE = -1, OK = 0, SICK, EOM
	} Ct_err;

#ifdef	_cerror_
	/* 
 	 *	pre_err is a global var used to propagate error types
	 *	up through nested procedure calls
	 */
	Ct_err	pre_err = OK;
#else
	extern	Ct_err	pre_err;
#endif

#define	BEG_NT_GEN	0
#define	BEG_NT_INPUT	10
#define	BEG_NT_GCAP	20
#define	BEG_NT_INTER	30
#define	BEG_NT_X11	50
#define	END_NT		100
#define	BEG_T_INPUT	110
#define	BEG_T_CONT	120
#define	BEG_T_X11	130
typedef enum {

	/*
	 *	The following error messages prefixed with 'NT'
	 *	do NOT cause termination of ctrans. When report
	 *	is set to ALL or NON_TERM via set_report_err these
	 *	errors cause messages to be output. Report is defaulted
	 *	to ALL
	 */

	/* 
	 *	non terminal general error messages
	 */
	NT_MALLOC = BEG_NT_GEN,		/* memory allocation error	*/
	NT_NULL,			/* null error message		*/

	/* 
	 *	non terminal input error messages	
	 */
	NT_NNCGM = BEG_NT_INPUT,/* non NCAR CGM record format	*/
	NT_IOUE,		/* illegal or unsupported element	*/
	NT_UCGMDF,		/* unsupported CGM data format		*/
	
	/*
	 *	non terminal graphcap error messages
	 */
	NT_GFEE = BEG_NT_GCAP,	/* graphcap format or encoding error	*/

	/*
	 *	non terminal interpretation error messages
	 */
	NT_UFONT = BEG_NT_INTER,/* unknown font				*/
	NT_CAE,			/* color allocation error		*/
	NT_ICE,			/* icon creation error			*/
	NT_ILSM,		/* invalid line specification mode	*/
	NT_IIP,			/* invalid integer precision		*/
	NT_IFP,			/* invalid floating point precision	*/
	NT_UPMT,		/* unsupported polymarker type		*/
	NT_UPFS,		/* unsupported polygon fill style	*/
	NT_UPLS,		/* unsupported polyline style		*/
	NT_CAFE,		/* cell array format error		*/
	NT_ICTI,		/* invalid color table index		*/
	NT_ITA,			/* invalid text attribute path		*/
	NT_FLTB,		/* font list to big			*/
	NT_PBTS,		/* point buffer too small		*/

	/*
 	 *	non terminal X11 specific errors
	 */
	NT_X11CRME = END_NT,/* X11 color resource manager error	*/

	/*
	 *	The following errors cause termination when
	 *	action is set to ACT via set_action_err. 
	 *	This is also the default
	 */

	/*
	 *	general terminal errors
	 */
	T_CNOD,			/* can not open device			*/
	T_MALLOC,		/* memmory allocation error		*/


	/*
	 *	terminal input errors
	 */
	T_FOE = BEG_T_INPUT,	/* file open error			*/
	T_FRE,			/* file read error			*/
	T_FSE,			/* file seek error			*/

	/*
	 *	terminal control error
	 */
	T_MM = BEG_T_CONT,	/* missing metafile			*/
	T_MD,			/* missing device			*/
	T_MF,			/* missing font				*/
	T_MR,			/* missing record 			*/
	T_NSO,			/* no such option			*/

	T_NULL,			/* null error message			*/

	/*
	 *	terminal X11 specific
	 */
	T_X11DEVNS = BEG_T_X11	/* X11 display env. var. not set	*/
	} Ct_error;

#endif	_cterror_
