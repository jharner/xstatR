#ifndef R_BRIDGE_H
#define R_BRIDGE_H

void initR(int argc, char **argv);
void endR(void);
void * parseAndEvaluate(const char *exp, int *hasError);
void RObjDefVar(const char *, void *);

int RObjType(void *);
int RObjLength(void *);
void *RObjAttributes(void *);
void *RObjAttributes2(void *);
void RObjSetAttribute(void *, const char *, void *);
char *getRObjClass(void *);

void *RObjNil();

int *RObjIntPtr(void *);
double *RObjRealPtr(void *);
const char *RObjString(void *);
void *getRObjAtIndex(void *, int);
void setRObjAtIndex(void *, int, void *);
void * RListAppend(void *, void *);

void *RObjAsList(void *);

void *RListCAR(void *);
void *RListCDR(void *);

void *newRLogicalVector(int);
void *newRIntVector(int);
void *newRNumericVector(int);
void *newRCharacterVector(int);
void *newRList(int);

void *mkRCharObj(const char *);

void newProtectionSession();
void endProtectionSession();
void protectRObj(void *);

#endif
