void    Initialize(int, float [], float [], int, int, 
                                  float [], float []);

double  armin(int, float *);
double  armax(int, float *);

extern int    ReadData();
extern float  **MakeGrid(int, int, float *, float *);

extern void   c_nnsetr(char *, float);
extern void   c_nngetr(char *, float *);

extern void   Terminate();
