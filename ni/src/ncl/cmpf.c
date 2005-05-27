/*
 *      $Id: cmpf.c,v
 */
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <float.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hlu.h>

float _NhlRndIt
#if	NhlNeedProto
(
	float a,
	int sig_digit
)
#else
(a,sig_digit)
	float a;
	int sig_digit;
#endif
{
	char	func[] = "_NhlRndIt";
        float	tmp;
	char	tstr[20];
	char	*end = NULL;

/*
* if its equal to zero just return
*/
        if(a == 0.0)
                return a;
/*
* floats are only accurate to 7 decimal places when 32bit IEEE used
*/
        if(sig_digit>7)
                sig_digit = 7;

	/*
 	 * Let sprintf round the number for us.
	 */
	sprintf(tstr,"%*.*e",1,sig_digit-1,a);
	tmp = (float)strtod(tstr,&end);

	if(!tmp && (tstr == end)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Rounding error!",func);
		return a;
	}

	return tmp;
}

float    _NhlCmpF
#if	NhlNeedProto
(float a, NhlCompareDat *b)
#else
(a,b)
        float a;
        NhlCompareDat *b;
#endif
{
        float   a_final,b_final;
        long a_int,b_int;
        int signa;

	if((a==0.0)&&(!b->is_zero)&&(b->lg_abs <= 0.0)){
		a_int = 0;
		b_final = b->orig_val * (float)pow(10.0,b->sig_dig);
		b_int = (long)b_final;
		return((float)a_int-b_int);
	} else if((a!=0.0)&&(b->is_zero)&&(log10(fabs(a)) <= 0.0)){
		b_int = 0;
		a_final = a * (float)pow(10.0,(double)b->sig_dig);
		a_int = (long)a_final;
		return((float)a_int - b_int);
	} else if((a==0.0)&&(b->is_zero)){
		return(0.0);
	}

	if((a==0.0)||(b->is_zero)) {
		return(a - b->orig_val);
	}
/*
* store sign info and make sure both numbers are positive so log10 can be
* used.
*/
	if(a < 0.0)
		signa = -1;
	else
		signa = 1;
	
        a_final = fabs(a);
/*
* Now divide through by the exponent determined above
*/
        a_final = a_final/(float)pow(10.0,(double)b->exp);

/*
* Since a and possibly b are now shifted to the immediate left of the decimal,
* multipling by pow(10.0,sig_dig), rounding , setting appropriate sign  and
* truncating the decimal produces two integers that can be compared.
*/
        a_final = a_final * pow(10.0,(double)b->sig_dig);
        a_final = _NhlRndIt(a_final,b->sig_dig);
        a_final *= signa;
        a_int = (long)a_final;
        b_int = (long)b->b_final;
        return((float)a_int-(float)b_int);
}

NhlCompareDat *_NhlCmpFSetup
#if	NhlNeedProto
(float val, int sig_dig)
#else
(val,sig_dig)
	float val;
	int sig_dig;
#endif
{
	NhlCompareDat *tmp = (NhlCompareDat*)malloc((unsigned)sizeof(NhlCompareDat));
	float dummy;
	int sign;
        
	if(sig_dig > 7)  {
		tmp->sig_dig = 7;
	} else {
		tmp->sig_dig = sig_dig;
	}
        
	if(val == 0.0) {
		tmp->is_zero = 1;
		tmp->orig_val = 0.0;
		return(tmp);
	}

	tmp->is_zero = 0;	
	tmp->orig_val = val;

	if(val < 0.0)
		sign = -1;
	else
		sign = 1;
	tmp->b_final = fabs(val);
	dummy = (float)log10(tmp->b_final);
	tmp->lg_abs = dummy;
	tmp->exp = (long)ceil(log10(tmp->b_final));
	if((float) tmp->exp == dummy) {
		tmp->exp++;
	}
	tmp->b_final = tmp->b_final/(float)pow(10.0,(double)tmp->exp);
	tmp->b_final = tmp->b_final * pow(10.0,(double)tmp->sig_dig);
	tmp->b_final = _NhlRndIt(tmp->b_final,tmp->sig_dig);
	tmp->b_final *= sign;
	return(tmp);
}

/*
 * Function:	_NhlCmpFAny
 *
 * Description: Differs from _NhlCmpF in that there is no need to set up
 *		a structure containing the expected range of the numbers.
 *		
 *
 * In Args:	a	first floating point number
 *		b	second floating point number
 *		sig_dig	<=7 represents number of significant digits to compare.
 *
 * Out Args:	NONE
 *
 * Return Values: 0 if equal, <0 if a<b, and >0 if a>b
 *
 * Side Effects: NONE
 */
float	_NhlCmpFAny
#if	NhlNeedProto
(float a, float b, int sig_dig)
#else
(a,b,sig_dig)
	float a;
	float b;
	int sig_dig;
#endif
{
	float	a_final;
	float	b_final;
	long a_int;
	long b_int;
	int exp;
	int signa;
	int signb;
	float tmp;
/*
* If sig_dig is > 6, a_int and b_int will overflow and cause problems
*/
	if(sig_dig > 7) 
		sig_dig = 7;

/*
* Get ride of easy cases:
* These actually didn't end up being easy since large numbers compared againts
* zero cause a_int and b_int to overflow. So I added the fabs checks to make
* sure that the absolute value of non-zero numbers are at least between 
* 0 and 1.0.
*/
	if((a == 0.0)&&(b!=0.0)&&(log10(fabs(b))<=0.0)) {
		a_int = 0;
		b_final = b * (float)pow(10.0,(double)sig_dig);
		b_int = (long)b_final;
		return((float)(a_int - b_int));
	} else if((a!=0.0)&&(b==0.0)&&(log10(fabs(a))<=0.0)){
		b_int = 0;
		a_final = a * (float)pow(10.0,(double)sig_dig);
		a_int = (long)a_final;
		return((float)(a_int - b_int));
	} else if((a==0.0)&&(b==0.0)){
		return(0.0);
	}
/*
* If I get here and either a or b is zero then than means one of them is
* greater that 1 and one is 0.0
*/
	if((a==0.0)||(b==0.0)) {
		return(a - b);
	}

	
/*
* store sign info and make sure both numbers are positive so log10 can be
* used. 
*/
	if(a < 0.0)
		signa = -1;
	else
		signa = 1;
	if(b < 0.0)
		signb = -1;
	else
		signb = 1;
	a_final = fabs(a);
	b_final = fabs(b);
/*
* Now compute the exponent needed to shift a to the decimal position immediately
* right of the decimal point for the value of a
*/
	if(a_final>b_final){ 
		tmp = (float)log10(a_final);
		exp = (long)ceil(log10(a_final));
		if((float)exp == tmp)
			exp++;
	} else {
		tmp = (float)log10(b_final);
		exp = (long)ceil(log10(b_final));
		if((float)exp == tmp)
			exp++;
	}

/*
* Now divide through by the exponent determined above
*/
	a_final = a_final/(float)pow(10.0,(double)exp);
	b_final = b_final/(float)pow(10.0,(double)exp);

/*
* Since a and possibly b are now shifted to the immediate left of the decimal,
* multipling by pow(10.0,sig_dig), rounding , setting appropriate sign  and 
* truncating the decimal produces two integers that can be compared.
*/
	a_final = a_final * pow(10.0,(double)sig_dig);
	b_final = b_final * pow(10.0,(double)sig_dig);
	a_final = _NhlRndIt(a_final,sig_dig);
	b_final = _NhlRndIt(b_final,sig_dig);
	a_final *= signa;
	b_final *= signb;
	a_int = (long)a_final;
	b_int = (long)b_final;
	return((float)a_int-(float)b_int);
}

double _NhlCmpDAny
#if	NhlNeedProto
(double a, double b, int sig_dig)
#else
(a,b,sig_dig)
	double a;
	double b;
	int sig_dig;
#endif
{
	double a_final;
	double b_final;
	long a_int;
	long b_int;
	int exp;
	int signa;
	int signb;
	double tmp;
/*
* If sig_dig is > 6, a_int and b_int will overflow and cause problems
*/
	if(sig_dig > 7) 
		sig_dig = 7;

/*
* Get ride of easy cases:
* These actually didn't end up being easy since large numbers compared againts
* zero cause a_int and b_int to overflow. So I added the fabs checks to make
* sure that the absolute value of non-zero numbers are at least between 
* 0 and 1.0.
*/
	if((a == 0.0)&&(b!=0.0)&&(log10(fabs(b))<=0.0)) {
		a_int = 0;
		b_final = b * (double)pow(10.0,(double)sig_dig);
		b_int = (long)b_final;
		return((double)(a_int - b_int));
	} else if((a!=0.0)&&(b==0.0)&&(log10(fabs(a))<=0.0)){
		b_int = 0;
		a_final = a * (double)pow(10.0,(double)sig_dig);
		a_int = (long)a_final;
		return((double)(a_int - b_int));
	} else if((a==0.0)&&(b==0.0)){
		return(0.0);
	}
/*
* If I get here and either a or b is zero then than means one of them is
* greater that 1 and one is 0.0
*/
	if((a==0.0)||(b==0.0)) {
		return(a - b);
	}

	
/*
* store sign info and make sure both numbers are positive so log10 can be
* used. 
*/
	if(a < 0.0)
		signa = -1;
	else
		signa = 1;
	if(b < 0.0)
		signb = -1;
	else
		signb = 1;
	a_final = fabs(a);
	b_final = fabs(b);
/*
* Now compute the exponent needed to shift a to the decimal position immediately
* right of the decimal point for the value of a
*/
	if(a_final>b_final){ 
		tmp = (double)log10(a_final);
		exp = (long)ceil(log10(a_final));
		if((double)exp == tmp)
			exp++;
	} else {
		tmp = (double)log10(b_final);
		exp = (long)ceil(log10(b_final));
		if((double)exp == tmp)
			exp++;
	}

/*
* Now divide through by the exponent determined above
*/
	a_final = a_final/(double)pow(10.0,(double)exp);
	b_final = b_final/(double)pow(10.0,(double)exp);

/*
* Since a and possibly b are now shifted to the immediate left of the decimal,
* multipling by pow(10.0,sig_dig), rounding , setting appropriate sign  and 
* truncating the decimal produces two integers that can be compared.
*/
	a_final = a_final * pow(10.0,(double)sig_dig);
	b_final = b_final * pow(10.0,(double)sig_dig);
	a_final = _NhlRndIt(a_final,sig_dig);
	b_final = _NhlRndIt(b_final,sig_dig);
	a_final *= signa;
	b_final *= signb;
	a_int = (long)a_final;
	b_int = (long)b_final;
	return((double)a_int-(double)b_int);
}

#define CMPF_DEBUG 0
#define EXP2TOEXP10 0.3010299956639812

/*
 * Function:	_NhlCmpFAny2
 *
 * Description: New version of the _NhlCmpFAny routine. 
 *		A new parameter allows specification of a minimum nonzero
 *              value.
 *              Also: Now uses frexp to break numbers up without fear of
 *              overflow. Numbers are normalized to the range 
 *              .1 <= fabs(x) < 1.0, using the base 2 exponent to nearly
 *              figure out the base 10 exponent to factor out. Iteration
 *              is then used to get the exact base 10 exponent.
 *		
 *
 * In Args:	a	first floating point number
 *		b	second floating point number
 *		sig_dig	<=7 represents number of significant digits to compare.
 *              min_nonzero the smallest abs value to be treated as nonzero.
 *
 * Out Args:	NONE
 *
 * Return Values: 0 if equal, <0 if a<b, and >0 if a>b
 *
 * Side Effects: NONE
 */
float	_NhlCmpFAny2
#if	NhlNeedProto
(float a, float b, int sig_dig, float min_nonzero)
#else
(a,b,sig_dig,min_nonzero)
	float a;
	float b;
	int sig_dig;
	float min_nonzero;
#endif
{
	double afr,bfr,minfr;
	int    aexp,bexp,minexp;
	long   arnd,brnd;
	int    asign,bsign;
	float factor;
	int   diffexp;
	int   aexp10,bexp10,diffexp10;
	double norm_factor,anorm,bnorm;
	int icount = 0;
	NhlBoolean azero,bzero;


#if CMPF_DEBUG
	fprintf(stderr,"comparing a: %g and b: %g to %d significant digits\n",
		a,b,sig_dig);
#endif
	if(sig_dig > 7) 
		sig_dig = 7;

	afr = frexp(a,&aexp);
	bfr = frexp(b,&bexp);

	minfr = fabs(frexp(min_nonzero,&minexp));
	asign = afr < 0.0 ? -1 : 1;
	bsign = bfr < 0.0 ? -1 : 1;
#if CMPF_DEBUG
	fprintf(stderr,"base 2 decomposition: a: %g %d b: %g %d\n",
		afr,aexp,bfr,bexp);
#endif
	azero = afr == 0.0 || aexp < minexp ||
		(aexp == minexp && afr * asign < minfr);
	bzero = bfr == 0.0 || bexp < minexp ||
		(bexp == minexp && bfr * bsign < minfr);

	if (azero && bzero) {
#if CMPF_DEBUG
		fprintf(stderr,
			"numbers smaller than minimum non-zero value: %f\n",
			min_nonzero);
#endif
		return 0.0;
	}
	if (azero)
		return (float) - bsign;
	if (bzero)
		return (float) asign;
	
	diffexp = abs(aexp - bexp);
	if (diffexp >= 4) {
		/*
		 * difference exceeds range of single digit decimal precision
		 */
#if CMPF_DEBUG
		fprintf(stderr,"difference exceeds factor of 10e1\n");
#endif

		switch (asign) {
		case 1:
			switch (bsign) {
			case 1:
				return (float)aexp - bexp;
			case -1:
				return 1.0;
			}
		case -1:
			switch (bsign) {
			case 1:
				return -1.0;
			case -1:
				return (float)bexp - aexp;
			}
		}
	}

	aexp10 = EXP2TOEXP10 * aexp;

#if CMPF_DEBUG
	printf("a base 2 exp: %d a base 10 exp: %d\n",aexp,aexp10);
#endif
/*
 * normalize the original values such that the greatest absolute value is
 * in the range .1 <= x < 1.0. Two step process: the first step seems to get
 * within 1 digit either way, so perhaps the "while" could be replaced with
 * and "if", but this is not proven.
 */
	norm_factor = pow(10.0,-aexp10); 
	anorm = a * norm_factor;
	while (anorm * asign >= 1.0) {
		icount++;
		aexp10++;
		anorm *= .1;
	}
	while (anorm * asign < .1) {
		icount++;
		aexp10--;
		anorm *= 10;
	}

#if CMPF_DEBUG
	printf("a normalized after %d iterations: %g %d\n",
	       icount,anorm,aexp10);
#endif
	bexp10 = EXP2TOEXP10 * bexp;
#if CMPF_DEBUG
	printf("b base 2 exp: %d b base 10 exp: %d\n",bexp,bexp10);
#endif

	norm_factor = pow(10.0,-bexp10); 
	bnorm = b * norm_factor;
	while (bnorm * bsign >= 1.0) {
		icount++;
		bexp10++;
		bnorm *= .1;
	}
	while (bnorm * bsign < .1) {
		icount++;
		bexp10--;
		bnorm *= 10;
	}

#if CMPF_DEBUG
	printf("b normalized after %d iterations: %g %d\n",
	       icount,bnorm,bexp10);
#endif

/*
 * Convert to longs in the range 10e(sig_digits) <= x < 10e(sig_digits+1)
 * rounded in the units digit.
 */

	factor = pow(10.0,(double)sig_dig);
	arnd = anorm * factor + (asign == 1 ? .5 : -.5);
	brnd = bnorm * factor + (bsign == 1 ? .5 : -.5);

/*
 * ASSERT: 
 * exponents never differ by more than 2
 * (compare .1 and .000999999999 noting that .1/(2^4) = 0.00625) 
 * This allows a switch rather than a while loop here.
 */         
	diffexp10 = aexp10 - bexp10;

        switch (diffexp10) {
        case -2:
		brnd *= 100;
		break;
        case -1:
		brnd *= 10;
		break;
        case 0:
		break;
        case 1:
		arnd *= 10;
		break;
        case 2:
		arnd *= 100;
		break;
        default:
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "Algorithmic error in _NhlCmpFAny2");
		break;
        }

	
#if CMPF_DEBUG
	fprintf(stderr,
	"rounded values converted to longs in range 10e(sig_digits): %d %d\n",
		arnd,brnd);
#endif
/*
 * compare
 */
	return (float) (arnd - brnd);
}

/*
 * Function:	_NhlCmpDAny2
 *
 * Description: New version of the _NhlCmpDAny routine. 
 *		A new parameter allows specification of a minimum nonzero
 *              value.
 *              Also: Now uses frexp to break numbers up without fear of
 *              overflow. Numbers are normalized to the range 
 *              .1 <= fabs(x) < 1.0, using the base 2 exponent to nearly
 *              figure out the base 10 exponent to factor out. Iteration
 *              is then used to get the exact base 10 exponent.
 *		
 *
 * In Args:	a	first floating point number
 *		b	second floating point number
 *		sig_dig	<=7 represents number of significant digits to compare.
 *              min_nonzero the smallest abs value to be treated as nonzero.
 *
 * Out Args:	NONE
 *
 * Return Values: 0 if equal, <0 if a<b, and >0 if a>b
 *
 * Side Effects: NONE
 */
double	_NhlCmpDAny2
#if	NhlNeedProto
(double a, double b, int sig_dig, double min_nonzero)
#else
(a,b,sig_dig,min_nonzero)
	double a;
	double b;
	int sig_dig;
	double min_nonzero;
#endif
{
	double atmp,btmp;
	char tstr[30];


#if CMPF_DEBUG
	fprintf(stderr,"comparing a: %g and b: %g to %d significant digits\n",
		a,b,sig_dig);
#endif
	if(sig_dig >= DBL_DIG) { 
		if (fabs(a-b) < MAX(DBL_EPSILON,min_nonzero))
			return 0.0;
		else if (a < b) 
			return -1.0;
		else 
			return 1.0;
	}

	sprintf(tstr,"%.*g",MIN(sig_dig,DBL_DIG+1),a);
	atmp = strtod(tstr,NULL);
	sprintf(tstr,"%.*g",MIN(sig_dig,DBL_DIG+1),b);
	btmp = strtod(tstr,NULL);
	if (fabs(atmp - btmp) < MAX(DBL_EPSILON,min_nonzero))
		return 0.0;
	else if (atmp < btmp) 
		return -1.0;
	else 
		return 1.0;

}
