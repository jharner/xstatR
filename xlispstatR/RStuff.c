#include <stdio.h>
#include "RStuff.h"
#include "RBridge.h"

typedef struct MappingRecord{
  int rType;
  char *rClass;
  int lispType;
  LVAL (*toLisp)(ROBJP);
  ROBJP (*toR)(LVAL);
  struct MappingRecord *next;
} MappingRecord;

typedef struct ConvertHint{
  int rType;
  char *rClass;
} ConvertHint;

LVAL toLispObj(ROBJP);
LVAL R2Lisp(ROBJP);

ROBJP toRObj(LVAL);
ROBJP Lisp2R(LVAL);

LVAL convertRObj(ROBJP);

MappingRecord* searchRToLispMapping(int, char *);
MappingRecord* searchLispToRMapping(LVAL, ConvertHint *);
MappingRecord* matchConverter1(int, char *, int *,
				MappingRecord *, MappingRecord *);
MappingRecord* matchConverter2(int, ConvertHint *, int *,
			       MappingRecord *, MappingRecord *);
int emptyMapping(MappingRecord *);

LVAL getRLogicalVector(ROBJP);
LVAL getRIntVector(ROBJP);
LVAL getRRealVector(ROBJP);
LVAL getRStringVector(ROBJP);
LVAL getRGenericVector(ROBJP);
LVAL getRList(ROBJP);

ROBJP mkRLogicalVector(LVAL);
ROBJP mkRIntVector(LVAL);
ROBJP mkRRealVector(LVAL);
ROBJP mkRStringVector(LVAL);
ROBJP mkRVector(LVAL);
int lengthOfCons(LVAL);

ConvertHint* probeForRType(LVAL);

static MappingRecord* registeredMappings = NULL;
static MappingRecord defaultMappings[] = {
  {R_LGLSXP, NULL, VECTOR, getRLogicalVector, mkRLogicalVector, NULL},
  {R_INTSXP, NULL, VECTOR, getRIntVector, mkRIntVector, NULL},
  {R_REALSXP, NULL, VECTOR, getRRealVector, mkRRealVector, NULL},
  {R_STRSXP, NULL, VECTOR, getRStringVector, mkRStringVector, NULL},
  {R_LISTSXP, NULL, CONS, getRGenericVector, NULL, NULL},
  {R_VECSXP, NULL, CONS, getRGenericVector, mkRVector, NULL},
  {0, NULL, 0, NULL, NULL, NULL}
};

static char *defaultArgs[3] = {"R", "--no-save", 0};

void initRConnection(){
  initR(2, defaultArgs);
}

void shutdownRConnection(){
  endR();
}

LVAL callR(V){
  LVAL result = NIL;
  int hasError = 0;

  newProtectionSession();
  LVAL cmd = nextarg();
  ROBJP ret = parseAndEvaluate(getstring(cmd), &hasError);
  if(hasError){
    fprintf(stderr, "Parsing and evaluating error: %s\n", getstring(cmd));
    return NIL;
  }
  else{
    protectRObj(ret);
    result = toLispObj(ret);
  }

  endProtectionSession();
  return result;
}

LVAL saveToR(V){
  LVAL name = nextarg();
  if(ntype(name) != STRING){
    fprintf(stderr, "A string is expected as the first argument\n");
    return NIL;
  }

  newProtectionSession();
  LVAL value = nextarg();
  RObjDefVar(getstring(name), toRObj(value));
  endProtectionSession();
  return NIL;
}

LVAL toLispObj(ROBJP rObj){
  LVAL value = NIL;
  LVAL attributes = NIL;
  ROBJP attrRObj;

  if(rObj == NULL || rObj == RObjNil())
    return NIL;

  value = R2Lisp(rObj);
  if(value == NIL)
    return NIL;
  else{
    attrRObj = RObjAttributes(rObj);
    if(attrRObj == RObjNil()){
      protectRObj(attrRObj = RObjAttributes2(rObj));
      attributes = R2Lisp(attrRObj);
    }
    else{
      protectRObj(attrRObj);
      attributes = toLispObj(attrRObj);
    }

    return cons(value, attributes);
  }
}

LVAL R2Lisp(ROBJP rObj){
  if(rObj == NULL || rObj == RObjNil())
    return NIL;

  int type = RObjType(rObj);
  char *className = getRObjClass(rObj);
  MappingRecord *mp = searchRToLispMapping(type, className);
  if(mp == NULL || mp->toLisp == NULL){
    /* fprintf(stderr, "Don't know how to convert R object, type: %d\n", type); */
    return NIL;
  }
  else
    return (mp->toLisp)(rObj);
}

ROBJP toRObj(LVAL lispObj){
  LVAL partD, partA, partAN;
  ROBJP retval; 
  ROBJP attrs;
  int i;

  if(lispObj == NIL)
    return RObjNil();

  partD = car(lispObj);
  retval = Lisp2R(partD);
  lispObj = cdr(lispObj);
  if(lispObj != NIL){
    partA = car(lispObj);
    partAN = cdr(lispObj); 
    while(partAN != NIL && ntype(partAN) != VECTOR)
      partAN = car(partAN);
    if(ntype(partAN) == VECTOR){
      attrs = Lisp2R(partA);
      if(attrs == RObjNil())
	return retval;

      for(i = 0; i < getsize(partAN); i++){
	char *aName = getstring(getelement(partAN, i));
	RObjSetAttribute(retval, aName, getRObjAtIndex(attrs, i));
      } 
    }
  }
  return retval;
}

ROBJP Lisp2R(LVAL lispObj){
  ROBJP retval;
  ConvertHint * hint = NULL;

  if(lispObj == NIL)
    return RObjNil();

  MappingRecord *mp = searchLispToRMapping(lispObj, hint);
  if(mp == NULL || mp->toR == NULL){
    fprintf(stderr, "Don't know how to convert lisp object, type: %d",
	    ntype(lispObj));
    return RObjNil();
  }
  protectRObj(retval = (*mp->toR)(lispObj));
  return retval;
}

MappingRecord * searchRToLispMapping(int type, char *className){
  int finished = 0;
  MappingRecord *mp = NULL;

  MappingRecord *tmp = registeredMappings;
  while(tmp && !finished){
    mp = matchConverter1(type, className, &finished, tmp, mp);
    tmp = tmp->next;
  }

  tmp = defaultMappings;
  while(!emptyMapping(tmp) && !finished){
    mp = matchConverter1(type, className, &finished, tmp, mp);
    tmp++;
  }

  return mp;
}

MappingRecord * searchLispToRMapping(LVAL lispObj, ConvertHint *hint){
  int finished = 0;
  MappingRecord *mp;

  if(hint == NULL)
    hint = probeForRType(lispObj);

  MappingRecord *tmp = registeredMappings;
  while(tmp && !finished){
    mp = matchConverter2(ntype(lispObj), hint, &finished, tmp, mp);
    tmp = tmp->next;
  }

  tmp = defaultMappings;
  while(!emptyMapping(tmp) && !finished){
    mp = matchConverter2(ntype(lispObj), hint, &finished, tmp, mp);
    tmp++;
  }

  return mp;
}

MappingRecord * matchConverter1(int rType, char *rClass, int *finished, 
				MappingRecord *mapping, 
				MappingRecord *defMapping){
  if(rClass != NULL && mapping->rClass != NULL && 
     strcmp(rClass, mapping->rClass) == 0){
    *finished = 1;
    return mapping;
  }

  if(rType == mapping->rType)
    return mapping;

  return defMapping;
}

MappingRecord* matchConverter2(int lispType, ConvertHint *hint, int *finished,
			       MappingRecord *mapping, 
			       MappingRecord *defMapping){
  if(lispType == mapping->lispType){
    if(hint == NULL)
      return mapping;
    else if(hint->rClass != NULL && mapping->rClass != NULL &&
	    strcmp(hint->rClass, mapping->rClass) == 0){
      *finished = 1;
      return mapping;
    }
    else if(hint->rType == mapping->rType)
      return mapping;
  }

  return defMapping;
}

int emptyMapping(MappingRecord *mp){
  if(mp->rType == 0 && mp->rClass == NULL && mp->lispType == 0)
    return 1;
  else
    return 0;
}

ConvertHint* probeForRType(LVAL arg){
  LVAL tmp;
  ConvertHint *hint;

  switch(ntype(arg)){
  case FIXNUM:
    hint = (ConvertHint *)malloc(sizeof(ConvertHint));
    hint->rType = R_INTSXP;
    return hint;
  case FLONUM:
    hint = (ConvertHint *)malloc(sizeof(ConvertHint));
    hint->rType = R_REALSXP;
    return hint;
  case STRING:
    hint = (ConvertHint *)malloc(sizeof(ConvertHint));
    hint->rType = R_STRSXP;
    return hint;
  case VECTOR:
    tmp = getelement(arg, 0);
    return probeForRType(tmp);
    break;
  default:
    return NULL;
  }
}

LVAL getRLogicalVector(ROBJP rObj){
  return getRIntVector(rObj);
}

LVAL getRIntVector(ROBJP rObj){
  if(!rObj || RObjType(rObj) == R_NILSXP)
    return NIL;

  int len = RObjLength(rObj);
  int *data = RObjIntPtr(rObj);
  LVAL v = newvector(len);
  int i;
  for(i = 0; i < len; i++)
    setelement(v, i, cvfixnum(data[i]));

  return v;
}

LVAL getRRealVector(ROBJP rObj){
  if(!rObj || RObjType(rObj) == R_NILSXP)
    return NIL;

  int len = RObjLength(rObj);

  LVAL v = newvector(len);
  double *data = RObjRealPtr(rObj);
  int i;
  for(i = 0; i < len; i++)
    setelement(v, i, cvflonum(data[i]));

  return v;
}

LVAL getRStringVector(ROBJP rObj){
  if(!rObj || RObjType(rObj) == R_NILSXP)
    return NIL;

  int len = RObjLength(rObj);
  LVAL v = newvector(len);
  int i;
  for(i = 0; i < len; i++)
    setelement(v, i, cvstring(RObjString(getRObjAtIndex(rObj, i))));

  return v;
}

LVAL getRGenericVector(ROBJP rObj){
  int index;
  rObj = RObjAsList(rObj);  
  int len = RObjLength(rObj);
  LVAL v1 = NIL;
  LVAL v2 = NIL;
  for(index = len -1; index >= 0; index--){
    v1 = toLispObj(getRObjAtIndex(rObj, index));
    v2 = cons(v1, v2);
  }

  return v2;
}

ROBJP mkRLogicalVector(LVAL arg){
  int i;
  ROBJP v;

  int size = getsize(arg);
  protectRObj((v = newRLogicalVector(size)));
  int *data = RObjIntPtr(v); 
  for(i = 0; i < size; i++)
    data[i] = getfixnum(getelement(arg, i));

  return v;
}

ROBJP mkRIntVector(LVAL arg){
  int i;
  ROBJP v;

  int size = getsize(arg);
  protectRObj((v = newRIntVector(size)));
  int *data = RObjIntPtr(v); 
  for(i = 0; i < size; i++)
    data[i] = getfixnum(getelement(arg, i));

  return v;
}

ROBJP mkRRealVector(LVAL arg){
  int i;
  ROBJP v;

  int size = getsize(arg);
  protectRObj((v = newRNumericVector(size)));
  double *data = RObjRealPtr(v); 
  for(i = 0; i < size; i++)
    data[i] = getflonum(getelement(arg, i));
  
  return v;
}

ROBJP mkRStringVector(LVAL arg){
  int i;
  ROBJP v;

  int size = getsize(arg);
  protectRObj((v = newRCharacterVector(size)));
  for(i = 0; i < size; i++){
    LVAL element = getelement(arg, i);
    setRObjAtIndex(v, i, mkRCharObj(getstring(element)));
  }

  return v;
}

ROBJP mkRVector(LVAL arg){
  LVAL tmp;
  ROBJP v;
  int i;
  int size = lengthOfCons(arg);

  protectRObj((v = newRList(size)));
  for(i = 0; i < size; i++){
    tmp = car(arg);
    arg = cdr(arg);
    if(tmp != NIL){
      ROBJP el = toRObj(tmp);
      setRObjAtIndex(v, i, el);
    }
  }

  return v;
}

int lengthOfCons(LVAL arg){
  int len = 0;
  LVAL tmp = car(arg);

  while(arg != NIL){
    if(tmp != NIL) len++;

    arg = cdr(arg);
  }

  return len;
}
