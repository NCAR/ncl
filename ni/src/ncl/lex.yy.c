# include "stdio.h"
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
/*#include <stdio.h>*/
#include <string.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclData.h"
#include "Symbol.h"
#include "parser.h"
#ifdef DEBUGER
extern void printtoken(
#ifdef NhlNeedProto
	int /* type */
#endif
);
#else
#define printtoken(n,m)
#endif

#define MAXTMPSPACE 512
char cur_line_text[MAXTMPSPACE];
char *cur_line_text_pos = &(cur_line_text[0]);
int cur_line_length = 0;
int cur_line_number= 1;
int last_line_length = 0;
int ok_to_start_vsblk = 0;
extern int loading;
#include <ctype.h>
#ifdef MAKEAPI
#undef input
#undef unput
char *the_input_buffer = NULL;
char *the_input_buffer_ptr = NULL;
int the_input_buffer_size = 0;
#define input() (( (int)the_input_buffer_ptr >=the_input_buffer_size + (int)the_input_buffer)? 0 : (((int)*the_input_buffer_ptr == 10) ? yylineno++,*the_input_buffer_ptr++:*the_input_buffer_ptr++))

#define unput(c) { yytchar = (c); if(yytchar=='\n')yylineno--;*(--the_input_buffer_ptr) = yytchar; }
#endif /* MAKEAPI */

int rec = 0;
FILE *recfp;
# define AA 2
# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
{
	cur_line_number++;
	BEGIN 0;
	yyless(yyleng-1);
}
break;
case 2:
{
	cur_line_number++;
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	return EOLN;
}
break;
case 3:
{
	BEGIN AA;
}
break;
case 4:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	if(rec == 1) {
		fprintf(recfp,"%s",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	cur_line_number++;
	return EOLN;
}
break;
case 5:
{
	printtoken(EOLN,(char*)NULL);
	cur_line_length += yyleng;
	if(rec == 1) {
		fprintf(recfp,"%s\n",cur_line_text);
	}
	last_line_length = cur_line_length;
	cur_line_length = 0;
	*cur_line_text_pos = '\0';
	cur_line_text_pos = &(cur_line_text[0]);
	BEGIN AA;
	return EOLN;
}
break;
case 6:
{ 
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	cur_line_length += yyleng;
	
}
break;
case 7:
{
		sscanf(yytext,"%lf",&(yylval.real));
		printtoken(REAL,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
		return REAL;
}
break;
case 8:
{
	sscanf(yytext,"%le",&(yylval.real));
	printtoken(REAL,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return REAL;
}
break;
case 9:
{
	sscanf(yytext,"%d",&(yylval.integer));
	printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return INT;
}
break;
case 10:
{
	yyless(yyleng - strlen(".gt."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 11:
{
	yyless(yyleng - strlen(".lt."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 12:
{
	yyless(yyleng - strlen(".le."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 13:
{
	yyless(yyleng - strlen(".eq."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 14:
{
	yyless(yyleng - strlen(".ge."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 15:
{
	yyless(yyleng - strlen(".ne."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 16:
{
	yyless(yyleng - strlen(".and."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 17:
{
	yyless(yyleng - strlen(".or."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 18:
{
	yyless(yyleng - strlen(".xor."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 19:
{
	yyless(yyleng - strlen(".not."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 20:
{
	yyless(yyleng - strlen(".xor."));
	sscanf(yytext,"%d",&(yylval.integer));
        printtoken(INT,yytext);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        return INT;
}
break;
case 21:
{
	printtoken(GT,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return GT;
}
break;
case 22:
{
	printtoken(LT,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return LT;
}
break;
case 23:
{ 
	printtoken(LE,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return LE;
}
break;
case 24:
{ 
	printtoken(EQ,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return EQ;
}
break;
case 25:
{
	printtoken(GE,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return GE;
}
break;
case 26:
{
	printtoken(NOT,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return NOT;
}
break;
case 27:
{
	printtoken(NE,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return NE;
}
break;
case 28:
{
	printtoken(AND,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return AND;
}
break;
case 29:
{
	printtoken(OR,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return OR;
}
break;
case 30:
{
	printtoken(XOR,(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return XOR;
}
break;
case 31:
{

	strcpy(yylval.str,yytext);
	printtoken(FVAR,yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return FVAR;
}
break;
case 32:
{
        NclSymbol *s;

	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
        if(( s = _NclLookUp(yytext)) == NULL) 
		s = _NclAddSym(yytext,UNDEF);
       	yylval.sym = s;
       	printtoken(s->type,yytext);
	if((rec == 1)&&(s->type == QUIT)) {
		fclose(recfp);
	}
       	return s->type;
}
break;
case 33:
{
	strcpy(yylval.str,&(yytext[1]));
	printtoken(DIM_MARKER,yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return DIM_MARKER;
}
break;
case 34:
{
	char *tmp;

	tmp = (char*)yytext + 1;
	strcpy(yylval.str,tmp);
	printtoken(ATTNAME,yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return ATTNAME;

}
break;
case 35:
{
	char *tmp;

	tmp = strchr(yytext,' ');
	if (tmp != NULL) {
		*tmp = '\0';
	} else {
	   tmp = strchr(yytext,'\t');
	   if (tmp != NULL) {
		*tmp = '\0';
	   } else {
	   	tmp = strchr(yytext,'|');;
	   	if (tmp != NULL) {
			*tmp = '\0';
	   	} 
	   }
	}
	strcpy(yylval.str,yytext);
	printtoken(DIM,yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return DIM;
}
break;
case 36:
{

	strcpy(yylval.str,yytext);
	printtoken(COORD,yylval.str);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return COORD;
/* NOTREACHED */
}
break;
case 37:
{
	char *tmp;

	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	tmp = strrchr(yytext,'\"');
	*tmp = '\0';
	tmp = (char*)&(yytext[0]) + 1;
	strncpy(yylval.str,tmp,strlen(tmp)+1);
	printtoken(STRING,yylval.str);
	return STRING;
}
break;
case 38:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(COLON); 
}
break;
case 39:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	printtoken(yytext[0],(char*)NULL); 
	return(LBC); 
}
break;
case 40:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(RBC); 
}
break;
case 41:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(LP); 
}
break;
case 42:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(LPSLSH); 
}
break;
case 43:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(RP); 
}
break;
case 44:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(SLSHRP); 
}
break;
case 45:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(LBK); 
}
break;
case 46:
{ 
	printtoken(yytext[0],(char*)NULL); 
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(RBK); 
}
break;
case 47:
{
	printtoken(yytext[0],(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(MARKER);
}
break;
case 48:
{  
	printtoken(yytext[0],(char*)NULL);
	cur_line_length += yyleng;
	strcpy(cur_line_text_pos,yytext);
	cur_line_text_pos += yyleng;
	return(yytext[0]);
}
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */
int yyvstop[] = {
0,

48,
0,

6,
48,
0,

5,
0,

33,
48,
0,

48,
0,

48,
0,

47,
48,
0,

41,
48,
0,

43,
48,
0,

48,
0,

48,
0,

48,
0,

9,
48,
0,

38,
48,
0,

48,
0,

48,
0,

32,
48,
0,

45,
48,
0,

48,
0,

46,
48,
0,

39,
48,
0,

40,
48,
0,

1,
48,
0,

1,
6,
48,
0,

2,
5,
0,

1,
33,
48,
0,

1,
48,
0,

1,
48,
0,

1,
47,
48,
0,

1,
41,
48,
0,

1,
43,
48,
0,

1,
48,
0,

1,
48,
0,

1,
48,
0,

1,
9,
48,
0,

1,
38,
48,
0,

1,
48,
0,

1,
48,
0,

1,
32,
48,
0,

1,
45,
48,
0,

1,
48,
0,

1,
46,
48,
0,

1,
39,
48,
0,

1,
40,
48,
0,

37,
0,

36,
0,

42,
0,

7,
0,

44,
0,

7,
0,

9,
0,

4,
0,

34,
0,

32,
0,

35,
0,

3,
0,

31,
0,

7,
0,

7,
0,

8,
0,

8,
0,

24,
0,

25,
0,

21,
0,

23,
0,

22,
0,

27,
0,

29,
0,

7,
0,

28,
0,

30,
0,

26,
0,

8,
0,

13,
0,

14,
0,

10,
0,

12,
0,

11,
0,

15,
0,

17,
0,

16,
0,

19,
0,

18,
20,
0,
0};
# define YYTYPE int
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,5,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,6,	1,7,	
0,0,	0,0,	23,79,	31,0,	
49,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,8,	1,9,	
0,0,	0,0,	0,0,	1,10,	
1,11,	1,12,	1,13,	0,0,	
1,5,	16,69,	1,14,	1,15,	
1,16,	1,17,	0,0,	12,52,	
0,0,	76,76,	0,0,	2,11,	
2,12,	2,13,	0,0,	1,18,	
1,19,	2,14,	2,15,	2,16,	
0,0,	1,20,	1,21,	14,53,	
0,0,	0,0,	1,21,	0,0,	
0,0,	59,88,	2,18,	2,19,	
76,76,	0,0,	17,70,	0,0,	
17,71,	17,71,	17,71,	17,71,	
17,71,	17,71,	17,71,	17,71,	
17,71,	17,71,	3,27,	55,82,	
1,22,	1,23,	1,24,	56,83,	
60,89,	61,90,	3,28,	3,29,	
71,116,	17,72,	83,122,	84,123,	
85,124,	57,84,	58,86,	2,22,	
82,121,	2,24,	51,51,	51,51,	
51,51,	51,51,	51,51,	51,51,	
51,51,	51,51,	51,51,	51,51,	
57,85,	58,87,	3,30,	3,31,	
1,25,	1,5,	1,26,	3,32,	
3,33,	3,34,	3,35,	62,91,	
3,27,	17,72,	3,36,	3,37,	
3,38,	3,39,	63,92,	2,25,	
64,93,	2,26,	65,95,	4,33,	
4,34,	4,35,	66,97,	3,40,	
3,41,	4,36,	4,37,	4,38,	
9,49,	3,42,	3,43,	64,94,	
66,98,	65,96,	3,43,	67,99,	
9,49,	9,0,	4,40,	4,41,	
68,100,	86,125,	87,126,	88,127,	
76,78,	89,128,	15,54,	15,54,	
15,54,	15,54,	15,54,	15,54,	
15,54,	15,54,	15,54,	15,54,	
3,44,	3,45,	3,46,	90,129,	
9,49,	9,50,	91,130,	15,55,	
98,131,	9,49,	100,132,	15,56,	
102,134,	15,57,	9,49,	4,44,	
103,135,	4,46,	15,58,	9,49,	
15,59,	15,60,	104,136,	105,138,	
107,142,	108,143,	109,144,	110,145,	
114,152,	111,146,	15,61,	106,140,	
3,47,	3,27,	3,48,	9,49,	
9,49,	104,137,	105,139,	15,62,	
9,49,	106,141,	112,148,	15,63,	
111,147,	15,64,	113,150,	4,47,	
115,153,	4,48,	15,65,	121,157,	
15,66,	15,67,	129,158,	131,159,	
113,151,	112,149,	134,162,	135,163,	
136,164,	137,165,	15,68,	9,49,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	138,166,	139,167,	
140,168,	141,169,	10,51,	9,49,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	10,51,	10,51,	
10,51,	10,51,	19,73,	142,170,	
143,171,	144,172,	151,173,	153,174,	
155,135,	156,145,	19,73,	19,74,	
162,175,	169,176,	171,177,	54,54,	
54,54,	54,54,	54,54,	54,54,	
54,54,	54,54,	54,54,	54,54,	
54,54,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	19,73,	19,73,	
54,81,	0,0,	0,0,	19,73,	
0,0,	0,0,	0,0,	0,0,	
19,73,	72,117,	0,0,	72,117,	
0,0,	19,73,	72,118,	72,118,	
72,118,	72,118,	72,118,	72,118,	
72,118,	72,118,	72,118,	72,118,	
0,0,	0,0,	0,0,	0,0,	
0,0,	19,73,	19,73,	0,0,	
54,81,	0,0,	19,73,	75,75,	
75,75,	75,75,	75,75,	75,75,	
75,75,	75,75,	75,75,	75,75,	
75,75,	117,118,	117,118,	117,118,	
117,118,	117,118,	117,118,	117,118,	
117,118,	117,118,	117,118,	0,0,	
0,0,	19,73,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
0,0,	0,0,	0,0,	0,0,	
20,75,	19,73,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
20,75,	20,75,	20,75,	20,75,	
21,76,	81,119,	0,0,	81,119,	
0,0,	0,0,	81,120,	81,120,	
81,120,	81,120,	81,120,	81,120,	
81,120,	81,120,	81,120,	81,120,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	21,76,	
0,0,	0,0,	0,0,	0,0,	
0,0,	101,101,	101,101,	101,101,	
101,101,	101,101,	101,101,	101,101,	
101,101,	101,101,	101,101,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	0,0,	101,133,	0,0,	
0,0,	0,0,	0,0,	0,0,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	101,133,	0,0,	
0,0,	0,0,	21,77,	0,0,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	21,77,	21,77,	
21,77,	21,77,	0,0,	21,78,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	0,0,	
0,0,	0,0,	0,0,	53,80,	
0,0,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	53,80,	
53,80,	53,80,	53,80,	70,101,	
70,101,	70,101,	70,101,	70,101,	
70,101,	70,101,	70,101,	70,101,	
70,101,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
70,102,	0,0,	0,0,	0,0,	
70,103,	0,0,	70,104,	0,0,	
0,0,	0,0,	0,0,	70,105,	
0,0,	70,106,	70,107,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	70,108,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
70,109,	0,0,	0,0,	0,0,	
70,110,	0,0,	70,111,	0,0,	
0,0,	0,0,	0,0,	70,112,	
0,0,	70,113,	70,114,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	70,115,	
116,154,	116,154,	116,154,	116,154,	
116,154,	116,154,	116,154,	116,154,	
116,154,	116,154,	119,120,	119,120,	
119,120,	119,120,	119,120,	119,120,	
119,120,	119,120,	119,120,	119,120,	
133,160,	116,155,	133,160,	0,0,	
0,0,	133,161,	133,161,	133,161,	
133,161,	133,161,	133,161,	133,161,	
133,161,	133,161,	133,161,	154,154,	
154,154,	154,154,	154,154,	154,154,	
154,154,	154,154,	154,154,	154,154,	
154,154,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	116,156,	160,161,	160,161,	
160,161,	160,161,	160,161,	160,161,	
160,161,	160,161,	160,161,	160,161,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-16,	yysvec+1,	0,	
yycrank+-89,	0,		0,	
yycrank+-104,	yysvec+3,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+6,
yycrank+0,	0,		yyvstop+8,
yycrank+-151,	0,		yyvstop+11,
yycrank+179,	0,		yyvstop+13,
yycrank+0,	0,		yyvstop+15,
yycrank+4,	0,		yyvstop+18,
yycrank+0,	0,		yyvstop+21,
yycrank+5,	0,		yyvstop+24,
yycrank+122,	0,		yyvstop+26,
yycrank+4,	0,		yyvstop+28,
yycrank+32,	0,		yyvstop+30,
yycrank+0,	0,		yyvstop+33,
yycrank+-301,	0,		yyvstop+36,
yycrank+329,	0,		yyvstop+38,
yycrank+443,	0,		yyvstop+40,
yycrank+0,	0,		yyvstop+43,
yycrank+4,	0,		yyvstop+46,
yycrank+0,	0,		yyvstop+48,
yycrank+0,	0,		yyvstop+51,
yycrank+0,	0,		yyvstop+54,
yycrank+0,	0,		yyvstop+57,
yycrank+0,	0,		yyvstop+60,
yycrank+0,	0,		yyvstop+64,
yycrank+0,	0,		yyvstop+67,
yycrank+-5,	yysvec+9,	yyvstop+71,
yycrank+0,	yysvec+10,	yyvstop+74,
yycrank+0,	0,		yyvstop+77,
yycrank+0,	yysvec+12,	yyvstop+81,
yycrank+0,	0,		yyvstop+85,
yycrank+0,	yysvec+14,	yyvstop+89,
yycrank+0,	yysvec+15,	yyvstop+92,
yycrank+0,	yysvec+16,	yyvstop+95,
yycrank+0,	yysvec+17,	yyvstop+98,
yycrank+0,	0,		yyvstop+102,
yycrank+0,	yysvec+19,	yyvstop+106,
yycrank+0,	yysvec+20,	yyvstop+109,
yycrank+0,	yysvec+21,	yyvstop+112,
yycrank+0,	0,		yyvstop+116,
yycrank+0,	yysvec+23,	yyvstop+120,
yycrank+0,	0,		yyvstop+123,
yycrank+0,	0,		yyvstop+127,
yycrank+0,	0,		yyvstop+131,
yycrank+-6,	yysvec+9,	0,	
yycrank+0,	0,		yyvstop+135,
yycrank+62,	yysvec+10,	yyvstop+137,
yycrank+0,	0,		yyvstop+139,
yycrank+520,	0,		0,	
yycrank+267,	0,		yyvstop+141,
yycrank+13,	0,		0,	
yycrank+14,	0,		0,	
yycrank+36,	0,		0,	
yycrank+37,	0,		0,	
yycrank+4,	0,		0,	
yycrank+14,	0,		0,	
yycrank+18,	0,		0,	
yycrank+21,	0,		0,	
yycrank+25,	0,		0,	
yycrank+39,	0,		0,	
yycrank+41,	0,		0,	
yycrank+45,	0,		0,	
yycrank+45,	0,		0,	
yycrank+53,	0,		0,	
yycrank+0,	0,		yyvstop+143,
yycrank+595,	0,		yyvstop+145,
yycrank+54,	yysvec+17,	yyvstop+147,
yycrank+302,	0,		0,	
yycrank+0,	yysvec+19,	0,	
yycrank+0,	0,		yyvstop+149,
yycrank+323,	yysvec+20,	yyvstop+151,
yycrank+44,	0,		0,	
yycrank+0,	yysvec+21,	yyvstop+153,
yycrank+0,	0,		yyvstop+155,
yycrank+0,	0,		yyvstop+157,
yycrank+0,	yysvec+53,	yyvstop+159,
yycrank+410,	0,		0,	
yycrank+40,	0,		0,	
yycrank+56,	0,		0,	
yycrank+57,	0,		0,	
yycrank+58,	0,		0,	
yycrank+119,	0,		0,	
yycrank+120,	0,		0,	
yycrank+121,	0,		0,	
yycrank+123,	0,		0,	
yycrank+101,	0,		0,	
yycrank+86,	0,		0,	
yycrank+0,	yysvec+83,	0,	
yycrank+0,	yysvec+84,	0,	
yycrank+0,	yysvec+85,	0,	
yycrank+0,	yysvec+86,	0,	
yycrank+0,	yysvec+87,	0,	
yycrank+0,	yysvec+88,	0,	
yycrank+72,	0,		0,	
yycrank+0,	yysvec+89,	0,	
yycrank+76,	0,		0,	
yycrank+433,	0,		yyvstop+161,
yycrank+114,	0,		0,	
yycrank+115,	yysvec+72,	0,	
yycrank+133,	0,		0,	
yycrank+134,	0,		0,	
yycrank+142,	0,		0,	
yycrank+122,	0,		0,	
yycrank+126,	0,		0,	
yycrank+96,	0,		0,	
yycrank+94,	yysvec+72,	0,	
yycrank+108,	0,		0,	
yycrank+121,	0,		0,	
yycrank+125,	0,		0,	
yycrank+94,	0,		0,	
yycrank+117,	0,		0,	
yycrank+668,	yysvec+70,	yyvstop+163,
yycrank+333,	0,		0,	
yycrank+0,	yysvec+117,	yyvstop+165,
yycrank+678,	0,		0,	
yycrank+0,	yysvec+119,	yyvstop+167,
yycrank+185,	0,		0,	
yycrank+0,	0,		yyvstop+169,
yycrank+0,	0,		yyvstop+171,
yycrank+0,	0,		yyvstop+173,
yycrank+0,	0,		yyvstop+175,
yycrank+0,	0,		yyvstop+177,
yycrank+0,	0,		yyvstop+179,
yycrank+0,	0,		yyvstop+181,
yycrank+188,	0,		0,	
yycrank+0,	yysvec+121,	0,	
yycrank+189,	0,		0,	
yycrank+0,	yysvec+129,	0,	
yycrank+693,	0,		0,	
yycrank+170,	0,		0,	
yycrank+193,	0,		0,	
yycrank+194,	0,		0,	
yycrank+195,	0,		0,	
yycrank+224,	0,		0,	
yycrank+225,	0,		0,	
yycrank+226,	0,		0,	
yycrank+189,	0,		0,	
yycrank+257,	0,		0,	
yycrank+222,	0,		0,	
yycrank+205,	0,		0,	
yycrank+0,	yysvec+135,	0,	
yycrank+0,	yysvec+136,	0,	
yycrank+0,	yysvec+137,	0,	
yycrank+0,	yysvec+138,	0,	
yycrank+0,	yysvec+139,	0,	
yycrank+0,	yysvec+140,	0,	
yycrank+190,	0,		0,	
yycrank+0,	yysvec+142,	0,	
yycrank+193,	0,		0,	
yycrank+703,	0,		yyvstop+183,
yycrank+227,	0,		0,	
yycrank+196,	0,		0,	
yycrank+0,	0,		yyvstop+185,
yycrank+0,	0,		yyvstop+187,
yycrank+0,	0,		yyvstop+189,
yycrank+722,	0,		0,	
yycrank+0,	yysvec+160,	yyvstop+191,
yycrank+266,	0,		0,	
yycrank+0,	0,		yyvstop+193,
yycrank+0,	0,		yyvstop+195,
yycrank+0,	0,		yyvstop+197,
yycrank+0,	0,		yyvstop+199,
yycrank+0,	0,		yyvstop+201,
yycrank+0,	0,		yyvstop+203,
yycrank+267,	0,		0,	
yycrank+0,	0,		yyvstop+205,
yycrank+268,	0,		0,	
yycrank+0,	yysvec+162,	0,	
yycrank+0,	yysvec+169,	0,	
yycrank+0,	yysvec+171,	0,	
yycrank+0,	0,		yyvstop+207,
yycrank+0,	0,		yyvstop+209,
yycrank+0,	0,		yyvstop+211,
0,	0,	0};
struct yywork *yytop = yycrank+779;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,'!' ,'"' ,01  ,01  ,01  ,'&' ,01  ,
01  ,01  ,01  ,'+' ,01  ,'+' ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
'@' ,'A' ,'A' ,'A' ,'A' ,'E' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,0134,01  ,01  ,'A' ,
01  ,'A' ,'A' ,'A' ,'A' ,'E' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,'|' ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
#ifndef lint
static	char ncform_sccsid[] = "@(#)ncform 1.6 88/02/08 SMI"; /* from S5R2 1.2 */
#endif

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
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
	char *yylastch;
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
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
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
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
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
yyback(p, m)
	int *p;
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
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
