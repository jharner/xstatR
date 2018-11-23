#ifndef RSTUFF_H
#define RSTUFF_H

#include "xlisp.h"

#define ROBJP                void *

/*
 * The following definations are based on the types definiation in
 * Rinternals.h.
 */
#define R_NILSXP	     0	  /* nil = NULL */
#define R_SYMSXP	     1	  /* symbols */
#define R_LISTSXP	     2	  /* lists of dotted pairs */
#define R_CLOSXP	     3	  /* closures */
#define R_ENVSXP	     4	  /* environments */
#define R_PROMSXP	     5	  /* promises:[un]evaluated closure arguments */
#define R_LANGSXP	     6	  /* language constructs (special lists) */
#define R_SPECIALSXP         7	  /* special forms */
#define R_BUILTINSXP         8	  /* builtin non-special forms */
#define R_CHARSXP	     9	  /* "scalar" string type (internal only)*/
#define R_LGLSXP	    10	  /* logical vectors */
#define R_INTSXP	    13	  /* integer vectors */
#define R_REALSXP	    14	  /* real variables */
#define R_CPLXSXP	    15	  /* complex variables */
#define R_STRSXP	    16	  /* string vectors */
#define R_DOTSXP	    17	  /* dot-dot-dot object */
#define R_ANYSXP	    18	  /* make "any" args work.
				     Used in specifying types for symbol
				     registration to mean anything is okay  */
#define R_VECSXP	    19	  /* generic vectors */
#define R_EXPRSXP	    20	  /* expressions vectors */
#define R_BCODESXP          21    /* byte code */
#define R_EXTPTRSXP         22    /* external pointer */
#define R_WEAKREFSXP        23    /* weak reference */
#define R_RAWSXP            24    /* raw bytes */
#define R_S4SXP             25    /* S4, non-vector */

void initRConnection();
void shutdownRConnection();

LVAL callR(V);
LVAL saveToR(V);

#endif
