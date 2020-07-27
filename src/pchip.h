
/**

** The piecewise cubic Hermite interpolant polynom functions **

Rita P. Ribeiro

August 2010
**/


#ifndef FLOAT
#define FLOAT float
#endif

#ifdef MAINHT
#define EXTERN
#else
#define EXTERN extern
#endif

/*

s = (x - xk)

H(s) = a +  bs + cs^2 + ds^3
H'(s) = b + 2cs + 3ds^2
 */

typedef struct {
  int npts;
  double *x;
  double *a;
  double *b;
  double *c;
  double *d;
} hermiteSpl;


hermiteSpl *pchip_set(int n,
		     double *x, double *y, double *m);

double *pchip_slope_monoFC(int n, double *m, double *delta);

void pchip_val(hermiteSpl *H,
	       double xval, int extrapol,
	       double *yval);
