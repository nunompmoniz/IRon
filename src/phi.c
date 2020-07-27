/* phi.c May 2010 */
/*
** phi relevance related functions.
**
** Rita P. Ribeiro
*/

#include <R.h>
#include <Rinternals.h>
#include <math.h>
#include <string.h>
#include "phi.h"

/* ============================================================ */
// new_phi
// To be called directly from R
/* ============================================================ */
void r2phi(Sint *n, double *y,
	   double *phiF_args,
	   double *y_phi) {

  r2phi_init(phiF_args);

  r2phi_eval(n, y, y_phi);

}

/**************************************************************/

/* ============================================================ */
// phi_init
// Because I want to leave pchip as indepent functions
/* ============================================================ */
phi_fun *phi_init(double *phiF_args) {

  phi_fun *phiF;

  if((phiF = (phi_fun *) ALLOC(1, sizeof(phi_fun))) == NULL) perror("phi.c: memory allocation error"); ;

  phiF->method = (phimethod) phiF_args[0];

  phiF->H = phiSpl_init(phiF_args);

  phiF->phi_value = phiSpl_value;

  return phiF;
}

/* ============================================================ */
// new_phi
// setting of phiF
// To be called directly from R
/* ============================================================ */
void r2phi_init(double *phiF_args) {

  phiF = phi_init(phiF_args);

}

/* ============================================================ */
// eval_phi
// To be called directly from R
/* ============================================================ */
void r2phi_eval(Sint *n, double *y,
		double *y_phi) {

  int i;
  phi_out y_phiF;

  for(i = 0; i < (int) *n; i++) {
    y_phiF = (*phiF->phi_value)(y[i], phiF);
    y_phi[i] = y_phiF.y_phi;
  }

}



/*
  ----------------------------------------------------------- 
  jointPhi
  -----------------------------------------------------------
*/  
void r2jphi_eval(Sint *n, double *y_phi, double *ypred_phi, 
		 double *p, double *jphi) {
  
  int i;

  for(i = 0; i< (int) *n; i++) 
    jphi[i] = jphi_value(y_phi[i], ypred_phi[i], *p);

}


/* ============================================================ */
// phi_fun_init
// Because I want to leave pchip as indepent functions
/* ============================================================ */
hermiteSpl *phiSpl_init(double *phiF_args) {

  int n, i;
  double *x, *y, *m;
  hermiteSpl *h;

  n = (int) phiF_args[1];

  // because of memcpy
  if((x = (double *) ALLOC(n,sizeof(double))) == NULL) perror("phi.c: memory allocation error"); ;
  if((y = (double *) ALLOC(n,sizeof(double))) == NULL) perror("phi.c: memory allocation error"); ;
  if((m = (double *) ALLOC(n,sizeof(double))) == NULL) perror("phi.c: memory allocation error"); ;

  for(i = 0;i < n; i++) {
    x[i] = phiF_args[3*i + 2];
    y[i] = phiF_args[3*i + 3];
    m[i] = phiF_args[3*i + 4];
  }

  h = pchip_set(n,x,y,m);

  /* cannot free them!
  if (x !=NULL) {free(x); x = NULL;}
  if (y !=NULL) {free(y); y = NULL;}
  if (m !=NULL) {free(m); m = NULL;}
  */

  return h;
}

/* ============================================================ */
// phi_fun_value
//
/* ============================================================ */
phi_out phiSpl_value(double y, phi_fun *phiF) {

  int extrap = 0;//linear
  phi_out y_phiF;

  pchip_val(phiF->H, y, extrap,
	    &y_phiF.y_phi);


  return y_phiF;
}

/*
  -----------------------------------------------------------
  joint phi
  -----------------------------------------------------------
*/
double jphi_value(double y_phi, double ypred_phi, double p) {

  double jphi;

  jphi = p * y_phi + (1 - p) * ypred_phi;
  return jphi;
}


