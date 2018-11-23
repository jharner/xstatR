#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Parse.h>

#include "RBridge.h"

typedef struct stack{
  int size;
  void *value;
  struct stack *next;
} Stack;

static Stack *protectionStack = NULL;

void push(Stack **pstack, const void *p, size_t size);
void *pop(Stack **pstack);

void push(Stack **pstack, const void *p, size_t size){
  Stack *newNode = (Stack *)malloc(sizeof(Stack));
  newNode->size = size;
  newNode->value = malloc(size);
  memcpy(newNode->value, p, size);
  newNode->next = *pstack;
  *pstack = newNode;
}

void *pop(Stack **pstack){
  if(*pstack == NULL)
    return NULL;

  Stack* tmp = *pstack;
  void *rval = tmp->value;
  *pstack = tmp->next;
  free(tmp);
  return rval;
}

void *peek(Stack **pstack){
  if(*pstack == NULL)
    return NULL;

  return (*pstack)->value;
}

void initR(int argc, char **argv){
  Rf_initEmbeddedR(argc, argv);
}

void endR(void){
  Rf_endEmbeddedR(0);
}

void * parseAndEvaluate(const char *cmd, int *hasError){
  SEXP e, result, tmp;
  ParseStatus pStatus;

  newProtectionSession();
  tmp = mkString(cmd);
  if(tmp == NULL || tmp == RObjNil()){
    *hasError = 1;
    fprintf(stderr, "Failed: mkString, %d\n", tmp);
    return NULL;
  }
  protectRObj(tmp);
  e = R_ParseVector(tmp, 1, &pStatus, R_NilValue);
  if(e == NULL || e == RObjNil()){
    *hasError = 1;
    fprintf(stderr, "Failed: parsing, %d\n", e);
    return NULL;
  }
  protectRObj(e);
  result = R_tryEval(VECTOR_ELT(e, 0), R_GlobalEnv, hasError);
  endProtectionSession();

  return result;
}

void RObjDefVar(const char *name, void *value){
  SEXP n, v;

  newProtectionSession();
  protectRObj(n = install(name));
  protectRObj(v = (SEXP)value);
  defineVar(n, v, R_GlobalEnv);
  endProtectionSession();
}

int RObjLength(void *rObj){
  SEXP sexp = (SEXP)rObj;
  return LENGTH(sexp);
}

int RObjType(void *rObj){
  SEXP sexp = (SEXP)rObj;
  return TYPEOF(sexp);
}

void *RObjAttributes(void *rObj){
  return ATTRIB((SEXP)rObj);
}

void *RObjAttributes2(void *rObj){
  SEXP exp, ret;
  int errorOccurred;

  newProtectionSession();
  protectRObj(exp = lang2(install("attributes"), (SEXP)rObj));
  ret = R_tryEval(exp, R_GlobalEnv, &errorOccurred);
  endProtectionSession();
  if(errorOccurred)
    return RObjNil();
  else
    return ret;
}

void RObjSetAttribute(void *rObj, const char *name, void *v){
  SEXP n = NEW_CHARACTER(1);
  SET_STRING_ELT(n, 0, mkChar(name));
  SET_ATTR((SEXP)rObj, n, (SEXP)v);
}

char *getRObjClass(void *rObj){
  SEXP sexp = (SEXP)rObj;
  SEXP rClass = GET_CLASS(sexp);
  if(rClass != R_NilValue)
    return STRING_VALUE(rClass);
  else
    return NULL;
}

void *RObjNil(){
  return R_NilValue;
}

int *RObjIntPtr(void *rObj){
  SEXP sexp = (SEXP)rObj;
  return INTEGER(sexp);
}

double *RObjRealPtr(void *rObj){
  SEXP sexp = (SEXP)rObj;
  return REAL(sexp);
}

const char *RObjString(void *rObj){
  SEXP sexp = (SEXP)rObj;
  return CHAR(sexp);
}

void *getRObjAtIndex(void *rObj, int index){
  SEXP sexp = (SEXP)rObj;
  if(TYPEOF(sexp) == STRSXP)
    return STRING_ELT(sexp, index);
  else
    return VECTOR_ELT(sexp, index);
}

void setRObjAtIndex(void *rObj, int index, void *value){
  SEXP sexp = (SEXP)rObj;
  SEXP v = (SEXP)value;
  if(TYPEOF(sexp) == STRSXP)
    SET_STRING_ELT(sexp, index, v);
  else
    SET_VECTOR_ELT(sexp, index, v); 
}

void * RListAppend(void *rObj1, void *rObj2){
  SEXP list = AS_LIST((SEXP)rObj1);
  return listAppend(list, (SEXP)rObj2);
}

void *RListCAR(void *rObj){
  SEXP sexp = (SEXP)rObj;
  return CAR(sexp);
}

void *RListCDR(void *rObj){
  SEXP sexp = (SEXP)rObj;
  return CDR(sexp);
}

void *RObjAsList(void *rObj){
  return AS_LIST((SEXP)rObj);
}

void *newRLogicalVector(int size){
  return NEW_LOGICAL(size);
}

void *newRIntVector(int size){
  return NEW_INTEGER(size);
}

void *newRNumericVector(int size){
  return NEW_NUMERIC(size);
}

void *newRCharacterVector(int size){
  return NEW_CHARACTER(size);
}

void *newRList(int size){
  return NEW_LIST(size);
}

void *mkRCharObj(const char *value){
  return mkChar(value);
}

void newProtectionSession(){
  int value = 0;
  push(&protectionStack, &value, sizeof(int));
}

void endProtectionSession(){
  if(protectionStack == NULL)
    return;

  int *v = pop(&protectionStack);
  if(*v > 0)
    UNPROTECT(*v);
  free(v);
}

void protectRObj(void *rObj){
  PROTECT((SEXP)rObj);
  int *nProtected = (int *)peek(&protectionStack);
  (*nProtected)++;
}

