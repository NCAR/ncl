# include "stdio.h"
#include "input.h"
#ifdef __cplusplus
   extern "C" {
     extern int yyreject();
     extern int yywrap();
     extern int yylook();
     extern void main();
     extern int yyback(int *, int);
     extern int yyinput();
     extern void yyoutput(int);
     extern void yyunput(int);
     extern int my_yylex();
   }
#endif	/* __cplusplus */
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX 200
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):my_getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern unsigned char yytext[];
int yymorfg;
extern unsigned char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	int yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
#include <ctype.h>
#include "lex.h"
static	short	have_command = 0;
# define YYNEWLINE 10
my_yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
;
break;
case 2:
		{
			if (! have_command) {
				yyleng = nuke_whitespace(yytext);
				return(FRAME_TYPE);
			} 
			else 
				REJECT;
			}
break;
case 3:
	{
			if (! have_command) {
				yyleng = nuke_whitespace(yytext);
				return(FRAME_LIST_TYPE);
			} 
			else 
				REJECT;
			}
break;
case 4:
	{
			if (!have_command) {
				have_command = 1;
				return(COMMAND_TYPE);
			}
			else
				REJECT;
			}
break;
case 5:
		{
			if (!have_command) {
				REJECT;
			}
			else {
				yyleng = nuke_lead_whitespace(yytext);
				return(DATA_TYPE);
			}
			}
break;
case 6:
		{
			have_command = 0;
			return(END_LINE);
			}
break;
case 7:
		{
			have_command = 0;
			(void) skip_line();
			return(ERROR_TYPE);
			}
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of my_yylex */





nuke_whitespace(s1)
	char	*s1;
{
	char	*s2 = s1;
	int	len = 0;

	while (*s1) {
		if (!isspace(*s1)) {
			*s2++ = *s1++;
			len++;
		}
		else
			s1++;
	}
	*s2 = '\0';
	return (len);
}

nuke_lead_whitespace(s1)
	char	*s1;
{
	char	*s2 = s1;
	int	len = 0;

	while (isspace(*s1))
		s1++;

	while (*s1) {
		*s2++ = *s1++;
		len++;
	}
	*s2 = '\0';
	return (len);
}

#ifdef	DEAD
nuke_quotes(s1, len)
	char	*s1;
	int	len;
{

	len -= 2;
	bcopy(s1 + 1, s1, len);
	s1[len] = '\0';

	return (len);
}
#endif

skip_line()
{
	int	c;

	while (((c = input()) != EOF) && c != '\n')
	;

	if (c == EOF) unput(c);

	return(1);
}

int yyvstop[] = {
0,

1,
0,

1,
0,

7,
0,

6,
0,

1,
5,
7,
0,

4,
5,
7,
0,

5,
7,
0,

2,
5,
7,
0,

7,
0,

5,
7,
0,

2,
5,
7,
0,

4,
5,
7,
0,

1,
5,
0,

5,
0,

5,
0,

5,
0,

5,
0,

2,
0,

2,
5,
0,

2,
5,
0,

4,
5,
0,

3,
5,
0,

5,
0,

3,
5,
0,

5,
0,

5,
0,

3,
0,

3,
5,
0,

3,
0,

3,
0,
0};
# define YYTYPE unsigned char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	1,4,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,5,	1,6,	0,0,	
1,7,	1,8,	1,7,	1,7,	
1,7,	1,7,	1,7,	1,7,	
1,9,	1,7,	1,10,	1,8,	
1,7,	1,11,	0,0,	0,0,	
0,0,	0,0,	0,0,	8,17,	
17,17,	19,27,	27,27,	1,7,	
1,7,	1,7,	1,7,	1,7,	
1,7,	1,7,	1,12,	8,18,	
17,18,	19,28,	27,28,	0,0,	
0,0,	0,0,	2,7,	2,8,	
2,7,	2,7,	2,7,	2,7,	
2,7,	2,7,	2,9,	2,7,	
2,10,	2,8,	2,7,	0,0,	
9,15,	0,0,	0,0,	0,0,	
0,0,	1,7,	0,0,	1,7,	
1,7,	2,7,	2,7,	2,7,	
2,7,	2,7,	2,7,	2,7,	
9,19,	9,19,	9,19,	9,19,	
9,19,	9,19,	9,19,	9,19,	
9,19,	9,19,	0,0,	23,30,	
0,0,	0,0,	29,29,	10,16,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,7,	23,24,	1,7,	
23,25,	16,16,	29,18,	2,7,	
0,0,	2,7,	2,7,	10,20,	
10,20,	10,20,	10,20,	10,20,	
10,20,	10,20,	10,20,	10,20,	
10,20,	16,20,	16,20,	16,20,	
16,20,	16,20,	16,20,	16,20,	
16,20,	16,20,	16,20,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	2,7,	
0,0,	2,7,	5,13,	5,14,	
0,0,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,15,	5,14,	5,16,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	0,0,	5,14,	0,0,	
5,14,	5,14,	0,0,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	5,14,	5,14,	5,14,	
5,14,	0,0,	5,14,	0,0,	
5,14,	6,14,	6,14,	0,0,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
0,0,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
0,0,	6,14,	0,0,	6,14,	
6,14,	0,0,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
6,14,	6,14,	6,14,	6,14,	
11,17,	6,14,	30,30,	6,14,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
11,18,	30,24,	0,0,	30,25,	
11,21,	11,21,	11,21,	11,21,	
11,21,	11,21,	11,21,	11,21,	
11,21,	11,21,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
12,22,	12,22,	12,22,	12,22,	
18,18,	0,0,	0,0,	0,0,	
18,23,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	18,24,	
0,0,	18,25,	18,23,	20,29,	
18,26,	18,26,	18,26,	18,26,	
18,26,	18,26,	18,26,	18,26,	
18,26,	18,26,	0,0,	20,18,	
0,0,	21,17,	0,0,	20,20,	
20,20,	20,20,	20,20,	20,20,	
20,20,	20,20,	20,20,	20,20,	
20,20,	21,18,	0,0,	0,0,	
0,0,	21,21,	21,21,	21,21,	
21,21,	21,21,	21,21,	21,21,	
21,21,	21,21,	21,21,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	22,22,	22,22,	22,22,	
22,22,	24,24,	33,35,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	25,25,	
0,0,	33,24,	0,0,	33,24,	
0,0,	24,31,	24,31,	24,31,	
24,31,	24,31,	24,31,	24,31,	
24,31,	24,31,	24,31,	25,32,	
25,32,	25,32,	25,32,	25,32,	
25,32,	25,32,	25,32,	25,32,	
25,32,	26,30,	31,31,	31,31,	
31,31,	31,31,	31,31,	31,31,	
31,31,	31,31,	31,31,	31,31,	
26,24,	0,0,	26,25,	0,0,	
0,0,	26,26,	26,26,	26,26,	
26,26,	26,26,	26,26,	26,26,	
26,26,	26,26,	26,26,	28,28,	
0,0,	0,0,	0,0,	28,33,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	28,24,	0,0,	
28,24,	28,33,	0,0,	28,34,	
28,34,	28,34,	28,34,	28,34,	
28,34,	28,34,	28,34,	28,34,	
28,34,	32,32,	32,32,	32,32,	
32,32,	32,32,	32,32,	32,32,	
32,32,	32,32,	32,32,	34,35,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	34,24,	0,0,	
34,24,	0,0,	0,0,	34,34,	
34,34,	34,34,	34,34,	34,34,	
34,34,	34,34,	34,34,	34,34,	
34,34,	0,0,	0,0,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
-1,	0,		yyvstop+1,
-39,	yysvec+1,	yyvstop+3,
0,	0,		yyvstop+5,
0,	0,		yyvstop+7,
134,	0,		yyvstop+9,
229,	0,		yyvstop+13,
0,	yysvec+6,	yyvstop+17,
23,	yysvec+5,	yyvstop+20,
56,	0,		yyvstop+24,
87,	yysvec+6,	yyvstop+26,
320,	yysvec+5,	yyvstop+29,
313,	yysvec+6,	yyvstop+33,
0,	yysvec+5,	yyvstop+37,
0,	yysvec+6,	yyvstop+40,
0,	yysvec+9,	0,	
97,	yysvec+6,	yyvstop+42,
24,	yysvec+5,	yyvstop+44,
404,	yysvec+5,	yyvstop+46,
25,	yysvec+9,	yyvstop+48,
419,	yysvec+6,	yyvstop+50,
433,	yysvec+5,	yyvstop+53,
426,	yysvec+6,	yyvstop+56,
83,	yysvec+5,	yyvstop+59,
517,	0,		0,	
527,	yysvec+6,	yyvstop+62,
553,	yysvec+5,	yyvstop+64,
26,	0,		0,	
579,	0,		0,	
86,	yysvec+6,	yyvstop+67,
322,	yysvec+5,	yyvstop+69,
538,	0,		yyvstop+71,
589,	yysvec+6,	yyvstop+73,
518,	0,		yyvstop+76,
615,	0,		yyvstop+78,
0,	yysvec+33,	0,	
0,	0,	0};
struct yywork *yytop = yycrank+672;
struct yysvf *yybgin = yysvec+1;
unsigned char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
040 ,'!' ,01  ,'!' ,'!' ,'!' ,'!' ,'!' ,
'!' ,'!' ,'!' ,01  ,'!' ,'!' ,'!' ,'!' ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,'!' ,'!' ,'!' ,'!' ,'!' ,'!' ,
'!' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,'!' ,01  ,'!' ,'!' ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,'!' ,01  ,'!' ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
0};
unsigned char yyextra[] = {
0,0,0,0,0,0,0,0,
0};
/* @(#) $Revision: 1.6 $      */
int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
/* char yytext[YYLMAX];
 * ***** nls8 ***** */
unsigned char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
/* char yysbuf[YYLMAX];
 * char *yysptr = yysbuf;
 * ***** nls8 ***** */
unsigned char yysbuf[YYLMAX];
unsigned char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
yylook(){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
/*	char *yylastch;
 * ***** nls8 ***** */
	unsigned char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = &yycrank[yystate->yystoff];
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == 0)break;
				}
			*yylastch++ = yych = input();
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt = &yycrank[yystate->yystoff]) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}

# ifdef __cplusplus
yyback(int *p, int m)
# else
yyback(p, m)
	int *p;
# endif
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
yyinput(){
	return(input());
	
	}

#ifdef __cplusplus
void yyoutput(int c)
#else
yyoutput(c)
  int c;
# endif
{
	output(c);
}

#ifdef __cplusplus
void yyunput(int c)
#else
yyunput(c)
   int c;
#endif
{
	unput(c);
}
