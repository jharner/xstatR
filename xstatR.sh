#!/bin/bash

export R_HOME="/Library/Frameworks/R.framework/Resources"
XLISPLIB=/Users/jharner/Documents/Dev/xlispstatR export XLISPLIB
if test -f xlisp.wks; then WKS="xlisp.wks";
else WKS="${XLISPLIB}/xlisp.wks";
fi
if test -f xlisp; then XLISP=./xlisp;
else XLISP="${XLISPLIB}/xlisp"
fi
exec ${XLISP} -w${WKS} $*
