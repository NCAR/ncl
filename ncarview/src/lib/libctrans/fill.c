/*
 *	$Id: fill.c,v 1.6 1991-08-16 10:50:22 clyne Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#include	<stdio.h>
#include	<cgm_tools.h>
#include	<ncarv.h>
#include	<cterror.h>
#include	"fill.h"
#include	"cgmc.h"
#include	"in.h"
#include	"default.h"
#include	"ctrandef.h"


/*
 *	fill.c:
 *	author		John Clyne
 *			4/28/88
 *
 *
 *		This file contains the routines which extract parameters
 *	of the CGM commands. 
 *
 *
 * 		The following routines are used to extract parameters of a 
 * 	specific p_type from the CGM_buffer and loading them into
 *	there specific fields in the cgmc. Each routine is capable
 *	of extracting one type of p_type only. 
 *
 *	on entry:
 *		cgmc	: points to a cgmc
 *		*instr	: contains raw CGM instruction
 *	 	p_len	: if >= 0 then p_len is the number of parameters 
 *			  of p_type to be extracted. Else the number of 
 *			  parameters is calculated from instr->data_length.
 *
 *	on exit:
 *		cgmc->xx : contains the parameters, where xx is the p_type
 *		cgmc->xxnum : contains the number of parameters actually read
 *	return = number of bytes of data in instr->data left un-processed.
 */
		
extern	char	*realloc();
extern	long	GetInt();
extern	double	GetReal();
extern	boolean Moreparm;

/* 
 *	fill_E:
 *		This routine extracts parameters from the CGM_Buf of 
 *		type E.
 *
 */
fill_E(cgmc,instr,p_len)
	CGMC  	*cgmc;
	Instr	*instr;
	int	p_len;		/* expected number of parameters of type E*/

{
	int 	prec,		/* number bits making up parameter E	*/
	stepsize,		/* size of parameter of type E in bytes */
	i;
	unsigned numparm;	/* calculated number of parameters of type E*/

	prec = E_PREC;		/* precision is fixed at 16 bits for Etype*/
	stepsize = prec / BYTESIZE; 

	numparm = (p_len != N) ? p_len : instr->data_length / stepsize;

  
	if (cgmc->Espace < numparm) {
		if (cgmc->e != (Etype *) NULL)
			cfree((char *) cgmc->e); 
		cgmc->Espace = numparm;
		cgmc->e= (Etype *) icMalloc (numparm * sizeof(Etype));
	}

	cgmc->Enum = numparm;	
  
	/* extract parameters */
	for(i=0;i<numparm;i++) {
    
		cgmc->e[i] = GetInt(instr->data, prec,FALSE);
		instr->data += (stepsize); 
	}

	/* return number of bytes of data left in parameter feild*/
	return((i = instr->data_length - (numparm * stepsize)) ? i : 0);
}
/* 
 *	fill_CD:
 *		This routine extracts parameters from the CGM_Buf of 
 *		type CD.
 *
 */
fill_CD(cgmc,instr,p_len)
	CGMC  	*cgmc;
	Instr	*instr;
	int		p_len;		/* expected number of parameters 
					 * of type CD
					 */

{
	int 	prec,		/* number bits making up parameter CD	*/
		stepsize,	/* size of parameter of type CD in bytes */
		i;

	unsigned numparm;	/* calculated parameter length of type CD*/

	/* determine precision of parameters	*/
	prec = DCP;		/* direct colour precision		*/

	stepsize = prec / BYTESIZE; 

	/* calculate number of parameters if unspecified	*/
	numparm = (p_len != N) ? p_len : instr->data_length / stepsize / 3;
  
	if (cgmc->CDspace < numparm) {
		if (cgmc->cd != (CDtype *) NULL)
			cfree((char *) cgmc->cd); 
		cgmc->CDspace = numparm; 
		cgmc->cd= (CDtype *) icMalloc (numparm * sizeof(CDtype)); 
	}

	cgmc->CDnum = numparm;	
  
	/* extract parameters */
	for(i=0;i<numparm;i++) {

		cgmc->cd[i].red = GetInt(instr->data, prec,FALSE);
		instr->data += (stepsize);

		cgmc->cd[i].green = GetInt(instr->data, prec,FALSE);
		instr->data += (stepsize);

		cgmc->cd[i].blue = GetInt(instr->data, prec,FALSE);
		instr->data += (stepsize);
	}
	/* return number of bytes of data left in parameter feild*/
	return((i = instr->data_length - (numparm * (stepsize*3))) ? i : 0);
}
/* 
 *	fill_CI:
 *		This routine extracts parameters from the CGM_Buf of 
 *		type CI.
 *
 */
fill_CI(cgmc,instr,p_len)
	CGMC  	*cgmc;
	Instr	*instr;
	int		p_len;		/* expected number of parameters 
					 * of type CI
					 */

{
	int 	prec,		/* number of bits making up parameter CI*/
		stepsize,	/* size of parameter of type CI in bytes */
		i;

	unsigned numparm;	/* calculated parameter length of type CI*/

	/* determine precision of parameters	*/
	prec = CIP;		/* get colour index precision*/
	stepsize = prec / BYTESIZE; 

	numparm = (p_len != N) ? p_len : instr->data_length / stepsize;

	if (cgmc->CIspace < numparm) {
		if (cgmc->ci != (CItype *) NULL)
			cfree((char *) cgmc->ci); 
		cgmc->CIspace = numparm;
		cgmc->ci = (CItype *) icMalloc (numparm * sizeof(CItype));
	}

	cgmc->CInum = numparm;	
  
	/* extract parameters */
	for(i=0;i<numparm;i++) {

		cgmc->ci[i] = GetInt(instr->data, prec,FALSE);
		instr->data += stepsize;
	}

	/* return number of bytes of data left in parameter feild*/
	return(instr->data_length - (numparm * stepsize));
}

/* 
 *	fill_CO:
 *		This routine extracts parameters from the CGM_Buf of 
 *		type CO.
 *
 */
fill_CO(cgmc,instr,p_len)
	CGMC  	*cgmc;
	Instr	*instr;
	int		p_len;		/* expected number of parameters 
					 * of type CO
					 */

{
	int 	prec,		/* precision of parameter CO		*/
		stepsize,	/* size of component parameter of type CO 
				 *in bytes 
				 */
		i;

	unsigned numparm;	/* calculated parameter length of type CI*/

	boolean	direct = CSM;	/* direct / indexed colour selection mode*/


	if (direct) 		/* if get colour selection mode	*/
		prec = DCP;		/* get colour precision*/
	else
		prec = CIP;		/* get colour index precision*/

	stepsize = prec / BYTESIZE; 

	if (direct)
		numparm = (p_len != N) ? 
				p_len : instr->data_length / stepsize / 3;
	else
		numparm = (p_len != N) ? 
				p_len : instr->data_length / stepsize;

	if (direct) {
		if (cgmc->CDspace < numparm) {
			if (cgmc->cd != (CDtype *) NULL)
				cfree((char *) cgmc->cd); 

			cgmc->CDspace = numparm;
			cgmc->cd = (CDtype *) icMalloc (numparm * sizeof(CDtype));
		}
	}
	else { 
		if (cgmc->CIspace < numparm) {
			if (cgmc->ci != (CItype *) NULL)
				cfree((char *) cgmc->ci); 

			cgmc->CIspace = numparm;
			cgmc->ci = (CItype *) icMalloc (numparm * sizeof(CItype));
		}
	}
  
      
  
	/* extract parameters */
	if (direct) {		/* if direct colour mode	*/
		cgmc->CDnum = numparm;	
		for(i=0;i<numparm;i++) {

			cgmc->cd[i].red = GetInt(instr->data, prec,FALSE);
			instr->data += stepsize;

			cgmc->cd[i].green = GetInt(instr->data+stepsize,
							prec,FALSE);

			instr->data += stepsize;

			cgmc->cd[i].blue = 
				GetInt(instr->data+(2*stepsize),prec,FALSE);
				instr->data += stepsize;
		}		/* for loop*/

	} 
	else {		/* else indexed colour mode	*/
		cgmc->CInum = numparm;
		for(i=0;i<numparm;i++) {

			cgmc->ci[i] = GetInt(instr->data, prec,FALSE);
			instr->data += stepsize;
		}
	}  /* else*/

	/* return number of bytes of data left in parameter feild*/
	return((direct)?((i = instr->data_length-(numparm*stepsize*3)) ? i : 0):
		((i = instr->data_length-(numparm*stepsize) ? i : 0)));
}

/* 
 *	fill_I:
 *		This routine extracts parameters from the CGM_Buf of 
 *		Dtype .
 *
 */
fill_D(cgmc,instr,p_len)
	CGMC  	*cgmc;
	Instr	*instr;
	int		p_len;		/* expected number of parameters 
					 * of type CO
					 */

{
	int	stepsize,	/* size of parameter of type CO in bytes */
		i;

	unsigned numparm;	/* calculated parameter length of type CO*/


	stepsize = 1; 

	numparm = (p_len != N) ? p_len : instr->data_length / stepsize;
  
	if (cgmc->Dspace < numparm) {
		if (cgmc->d != (Dtype *) NULL)
			cfree((char *) cgmc->d); 

		cgmc->Dspace = numparm;
		cgmc->d = (Dtype *) icMalloc (numparm * sizeof(Dtype));
	}

	cgmc->Dnum = numparm;	
  
	/* extract parameters */
	bcopy((char *) instr->data, (char *) cgmc->d, (int) numparm);

	/* return number of bytes of data left in parameter feild*/
	return((i = instr->data_length - (numparm * stepsize)) ? i : 0);
}
/* 
 *	fill_I:
 *		This routine extracts parameters from the CGM_Buf of 
 *		Itype .
 *
 */
fill_I(cgmc,instr,p_len)
	CGMC  	*cgmc;
	Instr	*instr;
	int		p_len;		/* expected number of parameters 
					 * of type CO
					 */

{
	int 	prec,		/* number of bytes making up parameter CO*/
		stepsize,	/* size of parameter of type CO in bytes */
		i;

	unsigned numparm;	/* calculated parameter length of type CO*/

	/* determine precision of parameters		*/
	prec = IP;		/* get integer precision		*/

	stepsize = prec / BYTESIZE; 

	numparm = (p_len != N) ? p_len : instr->data_length / stepsize;
  
	if (cgmc->Ispace < numparm) {
		if (cgmc->i != (Itype *) NULL)
			cfree((char *) cgmc->i); 

		cgmc->Ispace = numparm;
		cgmc->i = (Itype *) icMalloc (numparm * sizeof(Itype));
	}

	cgmc->Inum = numparm;	
  
	/* extract parameters */
	for(i=0;i<numparm;i++) {

		cgmc->i[i] = GetInt(instr->data, prec,TRUE);
		instr->data += (stepsize);
	}

	/* return number of bytes of data left in parameter feild*/
	return((i = instr->data_length - (numparm * stepsize)) ? i : 0);
}

/* 
 *	fill_IX:
 *		This routine extracts parameters from the CGM_Buf of 
 *		IXtype .
 *
 */
fill_IX(cgmc,instr,p_len)
	CGMC  	*cgmc;
	Instr	*instr;
	int		p_len;		/* expected number of parameters 
					 * of type IX
					 */

{
	int 	prec,		/* number of bits making up parameter IX*/
		stepsize,	/* size of parameter of type IX in bytes */
		i;

	unsigned numparm;	/* calculated parameter length of type IX*/

	/* determine precision of parameters		*/
	prec = IXP;		/* get index precision		*/

	stepsize = prec / BYTESIZE; 

	numparm = (p_len != N) ? p_len : instr->data_length / stepsize;
  
	if (cgmc->IXspace < numparm) {
		if (cgmc->ix != (IXtype *) NULL)
			cfree((char *) cgmc->ix); 

		cgmc->IXspace = numparm;
		cgmc->ix = (IXtype *) icMalloc (numparm * sizeof(IXtype));
	}

	cgmc->IXnum = numparm;	
  
	/* extract parameters */
	for(i=0;i<numparm;i++) {

		cgmc->ix[i] = GetInt(instr->data, prec,TRUE);
		instr->data += (stepsize);
	}

	/* return number of bytes of data left in parameter feild*/
	return((i = instr->data_length - (numparm * stepsize)) ? i : 0);
}

/* 
 *	fill_P:
 *		This routine extracts parameters from the CGM_Buf of 
 *		Ptype .
 *
 */
fill_P(cgmc,instr,p_len)
	CGMC  	*cgmc;
	Instr	*instr;
	int		p_len;		/* expected number of parameters 
					 * of type P
					 */

{
	int 	prec,		/* number of bits making up parameter P	*/
		expon_prec,	/* exponent precision for floating point*/
		man_prec,	/* mantissa precision for floating point*/
		stepsize,	/* size of component parameter of type P 
				 * in bytes 
				 */
		i;

	unsigned numparm;	/* calculated parameter length of type P*/

	/* determine precision of parameters		*/
	if (!VDC_TYPE) { 	/* if get VDC type		*/
		prec = VDC_INT; 	/* get VDC integer precision*/
	}

	else { 		/* get VDC real prec	*/ 
	/* NCAR CGM presently does not support real VDC values. Should VDC reals
	 * be added in the future, all that need to be done for the 
	 * entire Ctrans program is to convert them to VDC integers here.
	 */
		expon_prec = VDC_REAL_EXP;
		man_prec =   VDC_REAL_MAN;
		prec = expon_prec + man_prec;
	}

	stepsize = prec / BYTESIZE; 

	numparm = (p_len != N) ? p_len : instr->data_length / stepsize / 2;

	if (cgmc->Pspace < numparm) {
		if (cgmc->p != (Ptype *) NULL)
			cfree((char *) cgmc->p); 

		cgmc->Pspace = numparm;
		cgmc->p = (Ptype *) icMalloc (numparm * sizeof(Ptype));
	}

	cgmc->Pnum = numparm;	
  
	/* extract parameters */
	for(i=0;i<numparm;i++) {

		if (!VDC_TYPE) {	/* if vdc type is integer	*/ 
			cgmc->p[i].x = GetInt(instr->data, prec,TRUE);
			instr->data += (stepsize);

			cgmc->p[i].y = GetInt(instr->data, prec,TRUE);
		}
		else {		/* else vdc type is real		*/
			ct_error(NT_UCGMDF, "(VDC real)");

			instr->data += (stepsize);

		}

		instr->data += (stepsize);
	}
	/* return number of bytes of data left in parameter feild*/
	return((i = instr->data_length - (numparm * (stepsize*2))) ? i : 0);
}
/* 
 *	fill_R:
 *		This routine extracts parameters from the CGM_Buf of 
 *		Rtype .
 *
 */
fill_R(cgmc,instr,p_len)
	CGMC  	*cgmc;
	Instr	*instr;
	int		p_len;		/* expected number of parameters 
					 * of type R
					 */

{
	int 	prec,		/* number of bits making up parameter R	*/
		expon_prec,	/* exponent precision for floating point*/
		man_prec,	/* mantissa precision for floating point*/
		stepsize,	/* size of parameter of type R in bytes */
		i;
	unsigned numparm;	/* calculated parameter length of type R*/

	boolean	r_mode;	/* fixed / floating point mode	*/

	/* determine precision of parameters		*/
	r_mode = REAL_MODE; 
	expon_prec = REAL_EXP;
	man_prec =   REAL_MAN;
	prec = expon_prec + man_prec;

	stepsize = prec / BYTESIZE; 

	numparm = (p_len != N) ? p_len : instr->data_length / stepsize;

	if (cgmc->Rspace < numparm) {
		if (cgmc->r != (Rtype *) NULL)
			cfree((char *) cgmc->r); 

		cgmc->Rspace = numparm;
		cgmc->r = (Rtype *) icMalloc (numparm * sizeof(Rtype));
	}

	cgmc->Rnum = numparm;	
  
	/* extract parameters */
	for(i=0;i<numparm;i++) {

		cgmc->r[i] = GetReal(instr->data, r_mode, expon_prec, man_prec); 
		instr->data += (stepsize);
	}
	/* return number of bytes of data left in parameter feild*/
	return((i = instr->data_length - (numparm * stepsize)) ? i : 0);
}

/* 
 *	fill_S:
 *		This routine extracts parameters from the CGM_Buf of 
 *		Stype .
 *
 */
fill_S(cgmc,instr)
	CGMC  	*cgmc;
	Instr	*instr;
{
	int 	prec,		/* number of bytes making up parameter S*/
		stepsize,	/* size of parameter of type S in bytes */
		i,
		s_command;

	unsigned int charcount;	/* number of bytes of character data	*/
	unsigned int numparm;	/* num bytes data not yet processed	*/


	/* determine precision of parameters		*/
	prec = BYTESIZE;	/* precision is constant for string	*/
	stepsize = prec / BYTESIZE; 

	numparm = instr->data_length / stepsize;

	cgmc->Snum = 0;
	while (numparm > 0) {
		/*
		 *	make sure have enough memory. Can't do this before
		 *	hand because we don't know how many strings.
		 */
		if (cgmc->Snum >= cgmc->Sspace) {
			cgmc->s->string = (char **) 
				realloc ((char *) cgmc->s->string,
				(cgmc->Snum + 10) * sizeof (char *));

			cgmc->s->string_space = (int *) 
				realloc ((char *) cgmc->s->string_space, 
				(cgmc->Snum + 10) * sizeof (int));


			for (i=cgmc->Sspace; i < cgmc->Snum + 10; i++) {
				cgmc->s->string_space[i] = 0;
				cgmc->s->string[i] = NULL;
			}
			cgmc->Sspace = cgmc->Snum + 10;
		}
			

		/* test for long form of string data type*/
		if ((charcount = 
			GetInt(instr->data,BYTESIZE,FALSE)) == STRINGSIZE) {

			instr->data++; /* increment past character count*/
			numparm--;
			s_command = GetInt(instr->data,16,FALSE);
			charcount = GETBITS(s_command,C_PARM_POSS, C_PARM_BITS);

			instr->data += COMM_SIZE; 
			numparm -= COMM_SIZE;
		}
		else {
			instr->data++; 
			numparm--;
		}

		if (charcount > numparm) {
			ct_error(T_EE, "invalid CGM");
			return(-1);
		}

		/*
		 * make sure enough memory for individual string
		 */
		if (cgmc->s->string_space[cgmc->Snum] < (charcount + 1)) {
			if (cgmc->s->string[cgmc->Snum] != (char *) NULL)
				cfree((char *) cgmc->s->string[cgmc->Snum]);

			cgmc->s->string[cgmc->Snum] = (char *) icMalloc 
				(charcount+1 * sizeof(char));
			cgmc->s->string_space[cgmc->Snum] = charcount + 1;
		}

		/*
		 * copy in string and null terminate
		 */
		bcopy((char *) instr->data, 
				cgmc->s->string[cgmc->Snum], 
				(int) charcount);

		cgmc->s->string[cgmc->Snum][charcount] = '\0';

		numparm -= charcount;
		instr->data += charcount;
			
		cgmc->Snum++;
	}

	/* return number of bytes of data left in parameter feild*/
	return(numparm);
}

/* 
 *	fill_VDC:
 *		This routine extracts parameters from the CGM_Buf of 
 *		VDCtype .
 *
 */
fill_VDC(cgmc,instr,p_len)
	CGMC  	*cgmc;
	Instr	*instr;
	int		p_len;		/* expected number of parameters 
					 * of type VDC
					 */

{
	int 	prec,		/* number of bytes making up parameter VDC*/
		expon_prec,	/* exponent precision for floating point*/
		man_prec,	/* mantissa precision for floating point*/
		stepsize,	/* size of parameter of type VDC in bytes */
		i;

	unsigned numparm;	/* calculated parameter length of type VDC*/

	/* determine precision of parameters		*/
	if (!VDC_TYPE ) { 	/*  if get VDC type		*/
		prec = VDC_INT; 	/* get VDC integer precision*/
	}

	else { 		/* get VDC real prec	*/ 
		expon_prec = VDC_REAL_EXP; 
		man_prec = VDC_REAL_MAN; 
		prec = expon_prec + man_prec;
	}

	stepsize = prec / BYTESIZE; 

	numparm = (p_len != N) ? p_len : instr->data_length / stepsize;

	if (cgmc->VDCspace < numparm) {
		if (cgmc->vdc != (VDCtype *) NULL)
			cfree((char *) cgmc->vdc); 

		cgmc->VDCspace = numparm;
		cgmc->vdc = (VDCtype *) icMalloc (numparm * sizeof(VDCtype));
	}

	cgmc->VDCnum = numparm;	
  
	/* extract parameters */
	for(i=0;i<numparm;i++) {

		if (!VDC_TYPE)	/* if vdc type is integer	*/ 
			cgmc->vdc[i] = GetInt(instr->data, prec,TRUE);
		else {		/* if vdc type is real		*/
			
			ct_error(NT_UCGMDF, "(VDC real)");

		/* NCAR CGM presently does not support real VDC 
		 * values. Should VDC reals be added in the future, all 
		 * that need to be done for the entire Ctrans program is 
		 * to convert them to VDC integers here.
		 */
		}
	instr->data += stepsize;
	}

	/* return number of bytes of data left in parameter feild*/
	return((i = instr->data_length - (numparm * stepsize)) ? i : 0);
}




/* 	fill_special:
 *		cgm commands with more one type of parameter must be handled
 *		with this function. Each command is dealt with uniquely as
 *		required.
 *
 */

fill_special(cgmc,instr,class,id)
	CGMC 	*cgmc;
	Instr	*instr;
	int 	class, id;	/* cgm command class and id*/
{
	switch (class) {
	case 1 :
		switch(id) {
		case M_E_L : 	/* metafile element list*/
			instr->data_length = fill_I(cgmc,instr,1);
			return(fill_IX(cgmc,instr,N));
			
		default :
			ct_error(NT_IOUE,"");
			return (0);
		}
	case 4 :
		switch(id) {
		case TEXT :	/* text*/
			if (!Moreparm) {
				instr->data_length = fill_P(cgmc, instr,1);

				instr->data_length = fill_E(cgmc, instr,1);
			}

			return(fill_S(cgmc,instr));

		case CELL :	/* cell array*/
			/* note: cell arrays require a special 
			 * routine
			 */
			if (!Moreparm) { 
				instr->data_length = fill_P(cgmc, instr,3);

				instr->data_length = fill_I(cgmc, instr,3);

				instr->data_length = fill_E(cgmc, instr,1);
			}

			return(fill_Cellarray(cgmc,instr));

		case G_D_P :	/* generalized draw primitive*/
			if (!Moreparm) {
				instr->data_length = fill_I(cgmc, instr,2);

				instr->data_length = fill_I(cgmc, instr,
							(int) cgmc->i[1]);
			}
			
			return(fill_S(cgmc,instr));

		default : 
			ct_error(NT_IOUE,"");
			return (0);
		}

	case 5 :
		switch(id) {
		case TEXT_A :	/* text alignment*/
			instr->data_length = fill_E(cgmc,instr, 2);
			return(fill_R(cgmc,instr,2));

		case PATTERN :	/* pattern table*/
			instr->data_length = fill_IX(cgmc,instr,1);
			instr->data_length = fill_I(cgmc,instr,3);
			/* this code needs to be adjusted 
			 * for colour prec
			 */
			return(fill_CO(cgmc,instr,N));

		case COLOUR_T :	/* colour table*/
			instr->data_length = fill_CI(cgmc,instr,1);
			return(fill_CD(cgmc,instr,N));

		default :
			ct_error(NT_IOUE,"");
			return (0);
		}
	case 6 :
		switch(id) {
		case ESCAPE :	/* escape*/
			instr->data_length = fill_I(cgmc,instr,1);
			return(fill_S(cgmc,instr));

		default:
			ct_error(NT_IOUE,"");
			return (0);
		}
	default : 
		ct_error(NT_IOUE,"");
		return (0);
	}
}



/* 
 *	fill_Cellarray:
 *		This routine extracts parameters from the CGM_Buf 
 *		that contain colour values for a cell array.
 *		note: this routine has only been tested for packed encoding
 *		of cell arrays with indexed colour at 8 bit precision.
 *		See section 7.6 in ANSI X3.122 on CGM. ( found in the NCAR
 *		Graphics installers guide)
 *
 */
fill_Cellarray(cgmc,instr)
	CGMC  	*cgmc;
	Instr	*instr;

{
	int 	i_prec,		/* precision of parameter I 		*/
		c_prec,		/* precision of cells colour		*/
		i_stepsize,	/* size of component parameter of type 
				 * I in bytes	
				 */

		c_stepsize,	/* size of component parameter of type CO 
				 * in bytes
				 */
		i,k;

static	int	count;		/* number of cells processed in a row of data*/

	int	nx = cgmc->i[0];	/* number of columns in the cell array*/
	int	ny = cgmc->i[1]; 	/* number of rows in the cell array */
	boolean	odd;			/* true if nx is odd		*/

	unsigned numparm;		/* calculated parameter length 	*/

	boolean	direct = CSM;	/* direct / indexed colour selection mode*/




	c_prec = (direct ? DCP : CIP);	/* get colour precision*/



	/* only support one colour precision currently		*/
	if ((cgmc->i[2] != 8) && (cgmc->i[2] != 0 || c_prec != 8)) {
		ct_error(NT_CAFE,"(cell array must have 8 bit precison)");
		return(0);
	}



	if (cgmc->e[0]) {	/* packed length encoding	*/

		numparm = instr->data_length;

		/*	see if enough room for data	*/
		if (numparm > cgmc->Cnum) {
			if (cgmc->c != (Ctype *) NULL)
				cfree((char *) cgmc->c);
			cgmc->c = (Ctype *) icMalloc (numparm * sizeof(Ctype));
			cgmc->Cspace = numparm;
		}


		i = 0;
		cgmc->Cnum = 0;		/* clear datacount for cgmc	*/

		/* if this is the first partition of data */
		if (!Moreparm)	
			count = nx;	/* number of cells per row	*/

		odd = nx % 2;		/* see if odd number of columns	*/

		/*	try to extract data by rows	*/
		while(numparm) {
			/* k is the amount of data we extract in an iteration */
			k = MIN(count, numparm);

			/* 
			 * can directly copy data since we're using 8-bit 
			 * precision
			 */
			bcopy((char *) instr->data,(char *) cgmc->c+i, k);

			count -= k;
			numparm -= k;

			cgmc->Cnum += k;
			i += k;
			instr->data += k;

			/* if we just finnished reading a row	*/
			if (!count) {

				/* 
				 * see if have odd length. rows end on
				 * word boundries.
				 */
				if (numparm && odd) {
					numparm--;
					instr->data++;
				}

				count = nx;	/* reset count	*/
			}
		}
		return(0);
	}

	else {

		/*	
		 *	run length encoding	
		 */

		c_stepsize = c_prec / BYTESIZE; 
		i_prec = IP;			/* get index precision	*/
		i_stepsize = i_prec / BYTESIZE; 

		/* calculate number of parameter of type CO and I	*/
		numparm = (direct ? (instr->data_length / (i_stepsize + (c_stepsize * 3)))
			 : (instr->data_length / (i_stepsize + c_stepsize)));

		/*check space for CDtype  or CItype parameters	*/
		if (direct) {	/* if direct colour	*/
			if (cgmc->CDspace < numparm) {
				if (cgmc->cd != (CDtype *) NULL)
					cfree((char *) cgmc->cd); 
				cgmc->CDspace = numparm;
				cgmc->cd = (CDtype *) icMalloc 
					(numparm * sizeof(CDtype));
				cgmc->CDnum = numparm;	
			}
		}
		else { 		/*indexed colour	*/
			if (cgmc->CIspace < numparm) {
				if (cgmc->ci != (CItype *) NULL)
					cfree((char *) cgmc->ci); 
				cgmc->CIspace = numparm;
	
				cgmc->ci = (CItype *) icMalloc 
					(numparm * sizeof(CItype));
				cgmc->CInum = numparm;	
			}
		}
      
		/*check space for Itype parameters	*/
		if (cgmc->Ispace < numparm) {
			if (cgmc->ci != (CItype *) NULL)
				cfree((char *) cgmc->ci); 
			cgmc->Ispace = numparm;
			cgmc->i = (Itype *) icMalloc (numparm * sizeof(Itype));
			cgmc->Inum = numparm;	
		}
  
		/* extract parameters */
		if (direct) {		/* if direct colour mode	*/
			for(i=0;i<ny;i++) {
				count = k = 0;
				while (count < nx) {

					/* extract cell count	*/
					count += cgmc->i[k] = 
						GetInt(instr->data,
							i_prec,FALSE);

					instr->data += i_stepsize; 


					/* extract colours	*/
					cgmc->cd[k].red = 
						GetInt(instr->data,
							 c_prec,FALSE);

					instr->data += c_stepsize; 

					cgmc->cd[k].green = 
						GetInt(instr->data+c_stepsize,
							c_prec,FALSE);

					instr->data += c_stepsize; 

					cgmc->cd[k].blue =
						GetInt(instr->data+(2*c_stepsize),
							 c_prec,FALSE);

					instr->data += c_stepsize; 

					k++;
				}	/*while	*/

				/* rows that don't end on word boundaries 
				 * are paded. test and see if  row ends on 
				 * boundary. Adjust buffer if not
				 */
				if (((k * i_stepsize)+(3 * k * c_stepsize)) / 2) {
					instr->data++;
				}

			}		/* for loop*/

		} 
		else {		/* else indexed colour mode	*/
			cgmc->CInum = numparm;
			for(i=0;i<ny;i++) {
				count = k = 0;
				while (count < nx) {
					count += cgmc->i[k] = 
						GetInt(instr->data,
							i_prec,FALSE);

					instr->data += i_stepsize; 

					cgmc->ci[i] = 
						GetInt(instr->data, 
							c_prec,FALSE);

					instr->data += c_stepsize; 

					k++;
				}

				if (((k * i_stepsize) + (k * c_stepsize)) / 2) {
					instr->data++; 
				}
			}
		}  /* else*/

		/* return number of bytes of data left in parameter feild*/
		return((direct) ? ((i = instr->data_length-
				(numparm*(c_stepsize*3)+i_stepsize)) ? i : 0) 
			: ((i = instr->data_length-
				(numparm*(c_stepsize+i_stepsize))) ? 
			i : 0));
	}


}
