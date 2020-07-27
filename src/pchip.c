
/* ============================================================ */
/*                                                              */
/*    pchip: Piecewice Cubic Hermite Interpolating Polynomial   */
/*                                                              */
/* ============================================================ */
/*


*/
/*
** phi relevance function.
**  - Cubic Hermite Spline
** Rita P. Ribeiro
*/

#include <stdio.h>
#include <math.h>

#include "R.h"
#include <R_ext/Applic.h> // to use findInterval

#include "pchip.h"

/*
** Memory defined with S_alloc is removed automatically
 */
#define ALLOC(a,b)  S_alloc(a,b)

hermiteSpl *pchip_set(int n,
		      double *x, double *y, double *m) {


  int i;
  double *h, *delta, *new_m;
  hermiteSpl *H;

  if((H = (hermiteSpl *)ALLOC(1,sizeof(hermiteSpl))) == NULL) perror("pchip.c: memory allocation error");

  // fill-in the struct -- not sure of malloc

  H->npts = n;

  if((H->x = (double *)ALLOC(n,sizeof(double))) == NULL) perror("pchip.c: memory allocation error");
  if((H->a = (double *)ALLOC(n,sizeof(double))) == NULL) perror("pchip.c: memory allocation error");
  if((H->b = (double *)ALLOC(n,sizeof(double))) == NULL) perror("pchip.c: memory allocation error");
  if((H->c = (double *)ALLOC(n,sizeof(double))) == NULL) perror("pchip.c: memory allocation error");
  if((H->d = (double *)ALLOC(n,sizeof(double))) == NULL) perror("pchip.c: memory allocation error");

  if((h = (double *)ALLOC(n,sizeof(double))) == NULL) perror("pchip.c: memory allocation error");
  if((delta = (double *)ALLOC(n,sizeof(double))) == NULL) perror("pchip.c: memory allocation error");

  //n +1
  memcpy(H->x,x,n*sizeof(double));
  memcpy(H->a,y,n*sizeof(double));

  // auxiliary vectors
  for(i = 0;i < n-1; i++) {
    h[i] = x[i+1] - x[i];
    delta[i] = (y[i+1] - y[i])/ h[i];
  }

  new_m = pchip_slope_monoFC(n, m, delta);

  memcpy(H->b,new_m,n*sizeof(double));

  for(i = 0;i < n-1; i++) {
    H->c[i] = (3 * delta[i] - 2 * new_m[i] - new_m[i+1]) /
      h[i];
    H->d[i] = (new_m[i] - 2 * delta[i] + new_m[i+1]) /
      (h[i] *  h[i]);
  }

  return H;
}

/*
Slopes for shape-preserving Hermite cubic polynomials
 */


/**
 * Modify the slopes  m_k := s'(x_k) using Fritsch & Carlson (1980)'s algorithm
 *
 * @param m  numeric vector of length n, the preliminary desired slopes s'(x_i), i = 1:n
 * @param S the divided differences (y_{i+1} - y_i) / (x_{i+1} - x_i);        i = 1:(n-1)
 * @return m*: the modified m[]'s: Note that m[] is modified in place
 * @author Martin Maechler, Date: 19 Apr 2010
 */
// adapted
double *pchip_slope_monoFC(int n, double *m, double *delta) {

  for(int k = 0; k < n - 1; k++) {
    /* modify both (m[k] & m[k+1]) if needed : */
    double Sk = delta[k];


    int k1 = k + 1;

    if(fabs(Sk) == 0) {
      m[k] = m[k1] = 0.;

    } else {

      double
	alpha = m[k ] / Sk,
	beta  = m[k1] / Sk, a2b3, ab23;

      if(fabs(m[k]) !=0 && alpha < 0) {
	m[k] = -m[k];
	alpha = m[k] / Sk;
      }

      if(fabs(m[k1]) !=0 && beta < 0) {
	m[k1] = -m[k1];
	beta = m[k1] / Sk;
      }

      a2b3 = 2*alpha + beta - 3;
      ab23 = alpha + 2*beta - 3;

      if(a2b3 > 0 && ab23 > 0 &&
	 alpha * (a2b3 + ab23) < a2b3*a2b3) {
	/* we are outside the monotonocity region ==> fix slopes */
	double tauS = 3*Sk / sqrt(alpha*alpha + beta*beta);
	m[k ] = tauS * alpha;
	m[k1] = tauS * beta;

      }
    }
  } /* end for */

  return m;
}


//  Evaluate the cubic polynomial.
//  Find, from the left, the interval that contains or is nearest to xval.
// Check for linear extrapolation
// use cubic Hermite polynomials, even for extrapolation
void  pchip_val(hermiteSpl *H, double xval, int extrapol,
		double *yval) {

  int i = 1, rightmost_closed = 0, all_inside = 0, mfl = 0;
  double s;

  i = findInterval(H->x,H->npts,
		   xval,
		   rightmost_closed,all_inside,i,&mfl);


  // if extrapol is linear
  if(extrapol == 0 && (i == 0 || i == H->npts)) {

    if(i == H->npts) i--;

    *yval = H->a[i] + H->b[i] * (xval - H->x[i]);

    return;
  }


  i--;

  s = (xval - H->x[i]);
  *yval = H->a[i] + s * (H->b[i] +
		     s * (H->c[i] +
			  s * H->d[i]));

}
