/*
 *	$Id: c_agex10.c,v 1.1 1994-10-31 02:14:12 haley Exp $
 */
#include <stdio.h>
#include <math.h>
#include <ncarg/ncargC.h>

#define WSTYPE SED_WSTYPE
#define WKID   1

main()
{

/*
 * Define the data arrays.
 */
    int i;
    float xdra[1201],ydra[1201];
    extern void bndary();
/*
 * initialize gks.
 */
	gopen_gks("stdout",0);
	gopen_ws(WKID, NULL, WSTYPE);
	gactivate_ws(WKID);
/*
 * fill the data arrays.  the independent variable represents
 * time during the year (a hypothetical year with equal-length
 * months) and is set up so that minor ticks can be lengthened
 * to delimit the months; the major ticks, though shortened to
 * invisibility, still determine where the labels go.
 */
    for( i =0; i < 1201; i++ ) {
        xdra[i]=(float)(i-50);
        ydra[i]=cosh((float)(i-600)/202.);
    }
/*
 * change the labels on the bottom and left axes.
 */
    c_anotat("MONTHS OF THE YEAR$","ROMAN NUMERALS$",0,0,0,NULL);
/*
 * fix the minimum and maximum values on both axes and prevent
 * autograph from using rounded values at the ends of the axes.
 */
    c_agsetf("X/MIN.",-50.);
    c_agsetf("X/MAX.",1150.);
    c_agseti("X/NICE.",0);

    c_agsetf("Y/MIN.",1.);
    c_agsetf("Y/MAX.",10.);
    c_agseti("Y/NICE.",0);
/*
 * specify the spacing between major tick marks on all axes.
 * note that the autograph dummy routine agchnl is supplanted
 * (below) by one which supplies dates for the bottom axis and
 * roman numerals for the left axis in place of the numeric
 * labels one would otherwise get.
 */
    c_agseti("  LEFT/MAJOR/TYPE.",1);
    c_agseti(" RIGHT/MAJOR/TYPE.",1);
    c_agseti("BOTTOM/MAJOR/TYPE.",1);
    c_agseti("   TOP/MAJOR/TYPE.",1);

    c_agsetf ("  LEFT/MAJOR/BASE.",  1.);
    c_agsetf(" RIGHT/MAJOR/BASE.",  1.);
    c_agsetf("BOTTOM/MAJOR/BASE.",100.);
    c_agsetf("   TOP/MAJOR/BASE.",100.);
/*
 * suppress minor ticks on the left and right axes.
 */
    c_agseti("  LEFT/MINOR/SPACING.",0);
    c_agseti(" RIGHT/MINOR/SPACING.",0);
/*
 * on the bottom and top axes, put one minor tick between each
 * pair of major ticks, shorten major ticks to invisibility,
 * and lengthen minor ticks.  the net effect is to make the
 * minor ticks delimit the beginning and end of each month,
 * while the major ticks, though invisible, cause the names of
 * the months to be where we want them.
 */
    c_agseti("BOTTOM/MINOR/SPACING.",1);
    c_agseti("   TOP/MINOR/SPACING.",1);

    c_agsetf("BOTTOM/MAJOR/INWARD. ",0.);
    c_agsetf("BOTTOM/MINOR/INWARD. ",.015);
    c_agsetf("   TOP/MAJOR/INWARD. ",0.);
    c_agsetf("   TOP/MINOR/INWARD. ",.015);
/*
 * draw a boundary around the edge of the plotter frame.
 */
    bndary();
/*
 * draw the graph, using c_ezxy.
 */
    c_ezxy(xdra,ydra,1201,"EXAMPLE 10 (MODIFIED NUMERIC LABELS)$");
/*
 * close gks.
 */
	gdeactivate_ws(WKID);
	gclose_ws(WKID);
	gclose_gks();
}

void bndary()
{
/*
 * Routine to draw the plotter-frame edge.
 */
    c_plotit(    0,    0,0);
    c_plotit(32767,    0,1);
    c_plotit(32767,32767,1);
    c_plotit(    0,32767,1);
    c_plotit(    0,    0,1);
}

char *mons[] = {"JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT",
                "NOV","DEC"};

NGCALLF(agchnl,AGCHNL)(iaxs,vils,chrm,mcim,ncim,ipxm,chre,mcie,ncie)
int *iaxs, *ipxm, *mcim, *ncim, *mcie, *ncie;
float *vils;
char *chrm, *chre;
{
    int imon, incim;
/*
 * modify the numeric labels on the left axis.
 */
    if (*iaxs == 1) {
        incim = *ncim;
        (void)agcorn ((int)*vils,chrm,&incim);
        *ncim = incim;
        *ipxm=0;
        *ncie=0;
/*
 * modify the numeric labels on the bottom axis.
 */
    }
    else if (*iaxs == 3) {
        imon=(int)(*vils+.5)/100+1;
        strncpy( chrm, mons[imon-1], 3 );
        *ncim=3;
        *ipxm=0;
        *ncie=0;
    }
    return(1);
}

agcorn (ntgr,bcrn,ncrn)
int ntgr, *ncrn;
char *bcrn;
{
/*
 * this routine receives an integer in ntgr and returns its
 * roman-numeral equivalent in the first ncrn characters of
 * the character variable bcrn.  it only works for integers
 * within a limited range and it does some rather unorthodox
 * things (like using zero and minus).
 *
 * ich1, ich5, and ic10 are character variables used for the
 * single-unit, five-unit, and ten-unit symbols at a given
 * level.
 */
    char ich1,ich5,ic10;
    int i, imod, idiv, intg;
/*
 * treat numbers outside the range (-4000,+4000) as infinites.
 */
    if (abs(ntgr) >= 4000) {
        if (ntgr > 0) {
            *ncrn=5;
            strcpy( bcrn, "(inf)" );
        }
        else {
            *ncrn=6;
            strcpy( bcrn, "(-inf)" );
        }
        return(1);
    }
/*
 * use a '0' for the zero.  the romans never had it so good.
 */
    if (ntgr == 0) {
        *ncrn=1;
        bcrn[0]='0';
        bcrn[1]='\0';
        return(1);
    }
/*
 * zero the character counter.
 */
    *ncrn=0;
/*
 * handle negative integers by prefixing a minus sign.
 */
      if (ntgr < 0) {
          *ncrn = *ncrn + 1;
          bcrn[*ncrn-1] = '-';
          bcrn[*ncrn] = '\0';
      }
/*
 * initialize constants.  we'll check for thousands first.
 */
    imod=10000;
    idiv=1000;
    ich1='m';
/*
 * find out how many thousands (hundreds, tens, units) there
 * are and jump to the proper code block for each case.
 */
one:
    intg = (abs(ntgr)%imod)/idiv;

    switch(intg+1) {
      case 1:
        goto seven;
        break;
      case 2:
      case 3:
      case 4:
        goto four;
        break;
      case 5:
        goto two;
        break;
      case 6:
      case 7:
      case 8:
      case 9:
        goto three;
        break;
      case 10:
        goto six;
        break;
      default:
        break;
    }
/*
 * four - add ich1 followed by ich5.
 */
two:
    *ncrn = *ncrn + 1;
    bcrn[*ncrn-1]=ich1;
/*
 * five through eight - add ich5, followed by intg-5 ich1's.
 */
three:
    *ncrn = *ncrn + 1;
    bcrn[*ncrn-1]=ich5;

    intg=intg-5;
    if (intg <= 0) goto seven;
/*
 * one through three - add that many ich1's.
 */
four:
    for( i = 1; i <= intg; i++ ) {
        *ncrn = *ncrn + 1;
        bcrn[*ncrn-1]=ich1;
    }
five:

    goto seven;
/*
 * nine - add ich1, followed by ic10.
 */
six:
    *ncrn = *ncrn + 1;
    bcrn[*ncrn-1]=ich1;
    *ncrn = *ncrn + 1;
    bcrn[*ncrn-1]=ic10;
/*
 * if we're done, exit.
 */
seven:
    if (idiv == 1) {
        bcrn[*ncrn] = '\0';
        return(1);
    }
/*
 * otherwise, tool up for the next digit and loop back.
 */
    imod=imod/10;
    idiv=idiv/10;
    ic10=ich1;

    if (idiv == 100) {
        ich5='D';
        ich1='C';
    }
    else if (idiv == 10) {
        ich5='L';
        ich1='X';
    }
    else {
        ich5='V';
        ich1='I';
    }
    goto one;
}
