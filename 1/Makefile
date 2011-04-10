# $Id: Makefile,v 1.1 2011-03-24 17:23:58-07 - - $

MKFILE   = Makefile
DEPSFILE = ${MKFILE}.deps
CCOPTS   = -D__EXTENSIONS__ -Xc -v -g -xO0
LINTOPTS = -D__EXTENSIONS__ -Xa -fd -m -u -x -errchk=%all,no%longptr64
SOURCES  = main.c hello.c
HEADERS  = hello.h
OBJECTS  = main.o hello.o
CHECKIN  = ${MKFILE} ${SOURCES} ${HEADERS}
EXECBIN  = hello

all : ${EXECBIN}

${EXECBIN} : ${OBJECTS}
	cc ${CCOPTS} ${OBJECTS} -o ${EXECBIN}

%.o : %.c
	cc ${CCOPTS} -c $<

ci : ${CHECKIN}
	cid + ${CHECKIN}

lint : ${SOURCES}
	lint ${LINTOPTS} ${SOURCES}

test : ${EXECBIN}
	./${EXECBIN} ; echo status = $$?

clean : 
	- rm ${OBJECTS}

spotless : clean
	- rm ${EXECBIN} ${DEPSFILE}

deps :
	- rm ${DEPSFILE}
	${MAKE} --no-print-directory ${DEPSFILE}

${DEPSFILE} :
	@ (echo "# File: ${DEPSFILE}" ; \
	   echo "# Make: ${MAKE}" ; \
	   echo "# Date: `LC_TIME=C date`") >${DEPSFILE}
	cc -xM1 ${SOURCES} | sort | uniq >>${DEPSFILE}

include ${DEPSFILE}

