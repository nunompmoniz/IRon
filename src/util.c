
/* utility.c May 2010 */
/*
 ** Calculate the utility of a set of prediction
 **
 ** Rita P. Ribeiro
 */

#include <stdio.h>
#include <math.h>

#include "util.h"



/************************************************************/
/*                                                          */
/*        INTERFACE FUNCTIONS WITH R                        */
/*                                                          */
/************************************************************/

/* ============================================================ */
// new_util
// interface function with R
/* ============================================================ */
void r2util(SEXP *n,
            double *y,  double *ypred,
            double *phiF_args,
            double *loss_args,
            double *utilF_args,
            double *u) {

  r2util_init(phiF_args, loss_args, utilF_args);

  r2util_eval(n, y, ypred, u);

}

/* ============================================================ */
// set_util
// interface function with R
/* ============================================================ */
void r2util_init(double *phiF_args,
                 double *loss_args,
                 double *utilF_args) {

  phiF = phi_init(phiF_args);

  bumpI = bumps_set(phiF->H, loss_args);

  utilF = util_init(utilF_args);
}

/* ============================================================ */
// eval_util
// interface function with R
/* ============================================================ */
void r2util_eval(SEXP *n,
                 double *y,  double *ypred,
                 double *u) {

  phi_out *y_phiF, *ypred_phiF;

  if((y_phiF = (phi_out *)ALLOC((int) *n, sizeof(phi_out))) == NULL) perror("util.c: memory allocation error");
  if((ypred_phiF = (phi_out *)ALLOC((int) *n, sizeof(phi_out))) == NULL) perror("util.c: memory allocation error");


  util_core((int) *n, y, ypred, y_phiF, ypred_phiF, u);

}

/*
 -----------------------------------------------------------
 Init Util
 -----------------------------------------------------------
 */
util_fun *util_init(double *utilF_args) {

  util_fun *utilF;

  if((utilF = (util_fun *)ALLOC(1, sizeof(util_fun))) == NULL) perror("util.c: memory allocation error");

  utilF->p = utilF_args[0];
  utilF->Bmax = utilF_args[1];

  utilF->event_thr = utilF_args[2];

  return utilF;
}

/* ============================================================ */
// util core function
//
/* ============================================================ */
void util_core(int n, double *y,  double *ypred,
               phi_out *y_phiF, phi_out *ypred_phiF,
               double *u) {
  int i;

  for(i = 0; i < n; i++) {

    y_phiF[i] = phiF->phiSpl_value(y[i], phiF->H);
    ypred_phiF[i] = phiF->phiSpl_value(y[i], phiF->H);

    u[i] =
      util_value(y[i], ypred[i], y_phiF[i], ypred_phiF[i],
                 phiF, bumpI, utilF);

  }

}


/* ============================================================ */
// util core function
//
/* ============================================================ */
double util_value(double y, double ypred,
                  phi_out y_phiF, phi_out ypred_phiF,
                  phi_fun *phiF, phi_bumps *bumpI,
                  util_fun *utilF) {

  double lb, lc, ycphi, l;
  double jphi, benef, cost, uv;

  benefcost_lin(y, ypred,
                ypred_phiF.y_phi,phiF, bumpI,
                &lb, &lc, &ycphi);


  l = fabs(y - ypred);


  if(lb == 0 || l > lb)
    benef = 0;
  else
    benef = (1 - l/lb);
  /* ---------------------------------------------------------------- */

  jphi = jphi_value(y_phiF.y_phi,ycphi,utilF->p);// + 1e-7;

  if(lc == 0 || l > lc)
    cost = 1;
  else
    cost = l/lc;


  /* ---------------------------------------------------------------- */

  uv = y_phiF.y_phi * benef - jphi * cost;

  // July, 2014
  //uv = (y_phiF.y_phi * benef + 1e-7) - jphi * cost;

  return uv;
}


//------------------------------------------------
// Benefits Linearization
void benefcost_lin(double y, double ypred,
                   double ypred_phi,
                   phi_fun *phiF, phi_bumps *bumpI,
                   double *lb, double *lc, double *ycphi) {

  double lossA, yc;
  int i = 1, rightmost_closed = 0, all_inside = 0, mfl = 0;

  if(bumpI->n > 1)    {
    i = findInterval(bumpI->bleft,bumpI->n,
                     y,
                     rightmost_closed,all_inside,i,&mfl);

  }
  if(i > 0) i--; // Re-check this ...

  lossA = INFINITY;
  /*--------------------------------------------------*/
  /* Benefits loss tolerance */

  if(ypred <= y) {

    if(i > 0 && R_FINITE(bumpI->bleft[i]))
      lossA = fabs(y - bumpI->bleft[i]);

  } else {

    if((i+1) < bumpI-> n && R_FINITE(bumpI->bleft[i+1]))
      lossA = fabs(y - bumpI->bleft[i+1]);

  }


  if(lossA < bumpI->bloss[i])
    *lb = lossA;
  else
    *lb = bumpI->bloss[i];



  lossA = INFINITY;
  /*--------------------------------------------------*/
  /* Costs loss tolerance */

  if(ypred <= y) {

    if(i > 0 && R_FINITE(bumpI->bmax[i-1]))
      lossA = fabs(y - bumpI->bmax[i-1]);


  } else {

    if(i+1 < bumpI-> n && R_FINITE(bumpI->bmax[i+1]))
      lossA = fabs(y - bumpI->bmax[i+1]);


  }

  // If the err of committing regarding an action is more serious...
  if(lossA < bumpI->bloss[i])
    *lc = lossA;
  else
    *lc = bumpI->bloss[i];



  if(ypred <= y)
    yc = y - *lc;
  else
    yc = y + *lc;

  *ycphi = ypred_phi;

}

