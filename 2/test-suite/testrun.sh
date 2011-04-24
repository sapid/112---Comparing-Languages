#!/bin/sh -x
# $Id: testrun.sh,v 1.1 2011-04-08 20:16:04-07 - - $

checksource *.scm >check.log

./sbi.scm 00-hello-world.sbir  >00-hello-world.log  2>&1
./sbi.scm 01-1to10.sbir        >01-1to10.log        2>&1
./sbi.scm 02-exprs.sbir        >02-exprs.log        2>&1
./sbi.scm 10-exprs.sbir        >10-exprs.log        2>&1
./sbi.scm 11-let.sbir          >11-let.log          2>&1
./sbi.scm 12-let.sbir          >12-let.log          2>&1
./sbi.scm 20-goto.sbir         >20-goto.log         2>&1
./sbi.scm 21-let-if.sbir       >21-let-if.log       2>&1
./sbi.scm 22-fibonacci.sbir    >22-fibonacci.log    2>&1
./sbi.scm 25-pi-e-fns.sbir     >25-pi-e-fns.log     2>&1

echo 0 | \
   ./sbi.scm 30-input-pi.sbir     >30-input-pi.log     2>&1
echo 1 | \
   ./sbi.scm 30-input-pi.sbir     >>30-input-pi.log    2>&1

echo 4269 | \
   ./sbi.scm 31-big-o-.sbir       >31-big-o-.log       2>&1

echo 1 42 69 107 |  \
   ./sbi.scm 32-factorial.sbir    >32-factorial.log    2>&1

echo 1 0 0   1 1 0   2 2 2 | \
    ./sbi.scm 33-quadratic.sbir   >33-quadratic.log    2>&1

echo 5 1 4 2 3 10 1024 0 | \
    ./sbi.scm 40-sort-array.sbir  >40-sort-array.log   2>&1

./sbi.scm 41-eratosthenes.sbir    >41-eratosthenes.log 2>&1

