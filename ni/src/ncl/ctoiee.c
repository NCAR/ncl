/* Copyright 1994 University Corporation for Atmospheric Research (UCAR).
**	All rights reserved
** 
** Permission to use, copy, and modify this software and its documentation 
** for any non-commercial purpose is hereby granted without fee, provided 
** that the above copyright notice appear in all copies and that both that 
** copyright notice and this permission notice appear in supporting 
** documentation. UCAR makes no representations about the suitability of 
** this software for any purpose.   It is provided "as is" without express
** or implied warranty.
*/

/* 
**  ctodpf() and dctoie3i():  routines to convert Cray Fortran single
**      precision reals and integers (64 bits each) to IEEE  DP format;
**	it is assumed that char == 8 bits on all machines.
**
**  Written April 1987 by Chuck D'Ambra.
**  Last modified 15 August 1988 by CD.
**  Modified by Dan Anderson for rs6k 22 Feb 1992
**  Modified to unroll loops by PWM 14Nov93
*/

#define CBIAS		040000	/*  Cray f.p. exponent bias  */
#define MBIAS		1022	/*  IEEE f.p. exponent bias  */
#define CSIGNMASK	0200	/*  Mask to get 1st of 8 bits  */
#define LCDIF		sizeof(double) - sizeof(char)

#ifdef ByteSwapped
#define FLCDIF		0
#define BYTE0 3
#define BYTE1 2
#define BYTE2 1
#define BYTE3 0

#define DBYTE0 7
#define DBYTE1 6
#define DBYTE2 5
#define DBYTE3 4
#define DBYTE4 3
#define DBYTE5 2
#define DBYTE6 1
#define DBYTE7 0

#else
#define FLCDIF		(sizeof(long)-sizeof(char))
#define BYTE0 0
#define BYTE1 1
#define BYTE2 2
#define BYTE3 3

#define DBYTE0 0
#define DBYTE1 1
#define DBYTE2 2
#define DBYTE3 3
#define DBYTE4 4
#define DBYTE5 5
#define DBYTE6 6
#define DBYTE7 7
#endif
/*
** Because the SUN Fortran compiler adds an underscore to Fortran entry
** points, we need to define the following wrappers to the user
** entry points.
**
** There probably is a cleaner way to manage this, however we are now
** stuck with this documented (and widely used) interface.
**
**
**
**
**
** By doing this, the interfaces are the same for SUNs and RS6000s
*/

extern void ctodpf();

#if defined(sun) || defined(sgi)
void ctodpf_(in, out, n)
unsigned char   in[], out[];
int             *n;
{
        ctodpf(in, out, n);
        return;
}
#endif


/*------------------------------------------------------------------*/
void ctodpf(in, out, n)
unsigned char in[], out[];
int *n;
{
  	register int 		i, j, joff;
  	unsigned char 		*p, sign, *manp[6];
  	long 			exp;
  	static unsigned char 	maxman = 0xff, minman = 0;


	/*  Cray 64 bit float representation:
	**	bits 0-47:	mantissa
	**	bits 48-62:	exponent
	**	bit 63:		sign
	*/

  	for (i = 0; i < *n; i++) {
    	    p = in+i*8;
    	    joff = i * 8;

    	    sign = *(p) & CSIGNMASK;	/*  sign bit  */

    	    /*
    	    ** Point to mantissa...
    	    */
    	    manp[0] = p+2;
    	    manp[1] = p+3;
    	    manp[2] = p+4;
    	    manp[3] = p+5;
    	    manp[4] = p+6;
    	    manp[5] = p+7;


    	    exp = (*(p+1)) | ((*p & 0x7f) << 8);
    	    if (exp != 0) exp = exp - CBIAS + MBIAS;

	    /*  
	    ** If input is outside range representable on IEEE, set to
    	    **  closest representable number.  
	    */
    	    if (exp > 2046) {
      		exp = 2046;
      	    	/*  Set all bits in mantissa  */
      	    	for (j = 0; j < 6; j++)	manp[j] = &maxman; 
    	    }
    	    else if (exp <= 0) {
      	  	exp = 0;
      		/*  Turn off all bits in mantissa  */
      		for (j = 0; j < 6; j++)	manp[j] = &minman; 
    	    }

	    /*  Pack it into 64 bit IEEE float representation:
	    **	bits 0-51:	mantissa
	    **	bits 52-62:	exponent
	    **	bit 63:		sign
 	    */

     	    /* 
	    ** sign+upper 7 bits of exponent 
	    */
     	    out[joff+DBYTE0] = sign | (exp >> 4); 

    	    /* next joff+1 get lower 4 bits of exponent plus first 5 bits 
     	    ** of cray mantissa....but throw away left normalized bit because 
     	    ** its not in IEEE representation..so it leaves 3bits of mp[0] 
     	    ** for next byte
     	    **/

    	    out[joff+DBYTE1]=  ((exp      & 0xf) <<4) | ((*manp[0] >>3) & 0xf);
    	    out[joff+DBYTE2] = ((*manp[0] & 0x7) <<5) | ((*manp[1] & 0xf8) >>3);
    	    out[joff+DBYTE3] = ((*manp[1] & 0x7) <<5) | ((*manp[2] & 0xf8) >>3);
    	    out[joff+DBYTE4] = ((*manp[2] & 0x7) <<5) | ((*manp[3] & 0xf8) >>3);
    	    out[joff+DBYTE5] = ((*manp[3] & 0x7) <<5) | ((*manp[4] & 0xf8) >>3);
    	    out[joff+DBYTE6] = ((*manp[4] & 0x7) <<5) | ((*manp[5] & 0xf8) >>3);
    	    out[joff+DBYTE7] = ((*manp[5] & 0x7) <<5);
  	}
}




/* Copyright 1994 University Corporation for Atmospheric Research (UCAR).
**	All rights reserved
** 
** Permission to use, copy, and modify this software and its documentation 
** for any non-commercial purpose is hereby granted without fee, provided 
** that the above copyright notice appear in all copies and that both that 
** copyright notice and this permission notice appear in supporting 
** documentation. UCAR makes no representations about the suitability of 
** this software for any purpose.   It is provided "as is" without express
** or implied warranty.
*/

/*  ctospf() and ctospi():  routines to convert Cray Fortran single
**      precision reals and integers (64 bits each) to IEEE (32 bit) format;
**	it is assumed that char == 8 bits on all machines.
**
**  Written April 1987 by Chuck D'Ambra.
**  Last modified 15 August 1988 by CD.
**  Last modified Feb 26,1992 by Dan Anderson
**  Code cleanup Peter Morreale January 94
*/

#define FCBIAS		040000	/*  Cray f.p. exponent bias  */
#define FMBIAS		126	/*  IEEE f.p. exponent bias  */
#define FCSIGNMASK	0200	/*  Mask to get 1st of 8 bits  */


void checkrng();

/*
** Because the SUN Fortran compiler adds an underscore to Fortran entry
** points, we need to define the following wrappers to the user
** entry points.
**
** There probably is a cleaner way to manage this, however we are now
** stuck with this documented (and widely used) interface.
**
** By doing this, the interfaces are the same for Suns, SGIs,  and RS6000s
*/

extern void ctospf();
extern void ctospi();

#if defined(sgi) || defined(sun)
void ctospf_(in, out, n)
unsigned char   in[], out[];
int             *n;
{
        ctospf(in, out, n);
        return;
}

void ctospi_(in, out, n, zpad)
unsigned char   in[], out[];
int             *n;
int             *zpad;
{
        ctospi(in, out, n, zpad);
        return;
}
#endif


/*------------------------------------------------------------------------*/
void ctospf(in, out, n)
unsigned char in[], out[];
int *n;
{
  	register int 		i, j, joff;
  	unsigned char 		*p, sign, *expp, *manp[3];
  	long 			exp;
  	static unsigned char 	maxman = 0xff, minman = 0;

	/*  
	** Cray 64 bit float representation:
 	**	bits 0-47:	mantissa
 	**	bits 48-62:	exponent
 	**	bit 63:		sign
 	*/

  	for (i = 0; i < *n; i++) {
    	    p = in+i*8;
    	    joff = i * 4;

    	    sign = *(p) & FCSIGNMASK;	/*  sign bit  */

    	    /*
	    ** Point at mantissa
	    */
	    manp[0] = p+2;
	    manp[1] = p+3;
	    manp[2] = p+4;
	
	    /* 
	    ** Get the exponet
	    */
	    exp = (*(p+1)) | ((*p & 0x7f) << 8);
	    if (exp != 0) exp = exp - FCBIAS + FMBIAS;
	
	    /*  
	    ** If input is outside range representable on IEEE, set to
	    ** closest representable number.  
	    */
	    if (exp > 254) { /*  Too large  */
	        exp = 254;
	        /*  Set all bits in mantissa  */
	        for (j = 0; j < 3; j++) manp[j] = &maxman;
	    }
	    else if (exp < 0) {	/*  Too small  */
	        exp = 1;
	        /*  Turn off all bits in mantissa  */
	        for (j = 0; j < 3; j++)	manp[j] = &minman;
	    }
	
	    /*  
	    ** Pack it into 32 bit IEEE float representation:
	    **	bits 0-22:	mantissa
	    **	bits 23-30:	exponent
	    **	bit 31:		sign
	    */
	    expp = ((unsigned char *)(&exp)) + FLCDIF;
	    out[joff+BYTE0] = sign | (*expp >> 1);
	    out[joff+BYTE1] = (*manp[0] & (~FCSIGNMASK)) | ((*expp & 1) << 7);
	    out[joff+BYTE2] = *manp[1];
	    out[joff+BYTE3] = *manp[2];
	}
}


/*----------------------------------------------------------------------*/
void ctospi(in, out, n, zpad)
unsigned char in[], out[];
int *n; int *zpad;
{
	register int i, joff;
	register unsigned char *pin, sign;
	unsigned char *p[4];
	
	for (i = 0; i < *n; i++) {
	    pin = in+i*8;
	    joff = i * (*zpad+1)*4;
	
	    sign = *(pin) & CSIGNMASK;	/*  sign bit  */
	
	    p[BYTE0] = pin+4;	
	    p[BYTE1] = pin+5;	
	    p[BYTE2] = pin+6;	
	    p[BYTE3] = pin+7;	
	
	    checkrng(sign,pin,p);	/*  check if int w/i IEEE limits  */
	
	    /*  
	    ** Pack it into 4 byte IEEE integer representation  
	    */
	
	    out[joff+BYTE0] = sign | (*p[BYTE0] & (~CSIGNMASK));
	    out[joff+BYTE1] = *p[BYTE1];
	    out[joff+BYTE2] = *p[BYTE2];
	    out[joff+BYTE3] = *p[BYTE3];

	    /* 
	    ** 4 bytes of zeros if requested 
	    */
	    if (*zpad) {
	        out[joff+4]=out[joff+5]=out[joff+6]=out[joff+7]=0;
	    }
	}
}

/*----------------------------------------------------------------------*/
void checkrng(sign,cp,p)
unsigned char sign, *cp, *p[4];
{
	static unsigned char maxint = 0xff, minint = 0;
	
	/*
	** int >= 0; 1st 33 bits must be 0. 
	*/
	if (sign == 0)	{
	    if ((*(cp+4) & CSIGNMASK) != 0) { 
	        p[BYTE0] = p[BYTE1] = p[BYTE2] = p[BYTE3] = &maxint;
	    } else if( *cp != 0 ) {
	        p[BYTE0] = p[BYTE1] = p[BYTE2] = p[BYTE3] = &maxint;
	    } else if( *(cp+1) != 0) {
	        p[BYTE0] = p[BYTE1] = p[BYTE2] = p[BYTE3] = &maxint;
	    } else if( *(cp+2) != 0) {
	        p[BYTE0] = p[BYTE1] = p[BYTE2] = p[BYTE3] = &maxint;
	    } else if( *(cp+3) != 0) {
	        p[BYTE0] = p[BYTE1] = p[BYTE2] = p[BYTE3] = &maxint;
	    }
	}
	else {		
	    /*  
	    ** int < 0; 1st 33 bits must be 1. 
	    */
	    if ((*(cp+4) & CSIGNMASK) < 0200 || *cp != 0377 || *(cp+1) != 0377
	                               || *(cp+2) != 0377  ||  *(cp+3) != 0377)
	      p[BYTE0] = p[BYTE1] = p[BYTE2] = p[BYTE3] = &minint;
	}
}
	

