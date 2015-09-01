#!/bin/bash

LEDGER=trip-duxbury-aug-2015.beancount
PARTICIPANTS="Sheila Caroline Martin"

bean-query $LEDGER  "
  SELECT GREP('\b(Sheila|Caroline|Martin)\b', account) AS participant, SUM(position) as balance
  GROUP BY 1
  ORDER BY 2
  "
echo
echo

for p in $PARTICIPANTS ; do
    echo "================================================================================"
    echo "$p"

    echo
    echo "Expenses by category"
    bean-query $LEDGER  "
      SELECT PARENT(account), SUM(position)
      WHERE account ~ 'Expenses.*$p'
      GROUP BY 1
      ORDER BY 2 DESC
      "

    echo
    echo
    echo "Expenses Detail"
    bean-query $LEDGER  "
      SELECT date, flag, payee, narration, PARENT(account), position, balance
      WHERE account ~ 'Expenses.*$p'
      "

    echo
    echo
    echo "Contributions Detail"
    bean-query $LEDGER  "
      SELECT date, flag, payee, narration, account, position, balance
      WHERE account ~ 'Income.*$p'
      "

    echo
    echo
    echo "Balance"
    bean-query $LEDGER  "
      SELECT SUM(position)
      WHERE account ~ ':$p'
      "

    echo
    echo
    echo
    echo
done



bean-query $LEDGER  " SELECT GREP('\b(Sheila|Caroline|Martin)\b', account), SUM(position) WHERE account ~ ':$p' GROUP BY 1 ORDER BY 2 "
