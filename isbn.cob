*> Rabia Qureshi
*> 1046427
*> March 25, 2022
*> Program to determine the validity of 10-digit ISBNs from a file

identification division.
program-id. isbn.

data division.
working-storage section.
77 input-fname pic x(30).

procedure division.
    perform display-program-header.
    display "Enter the filename: " with no advancing.
    accept input-fname.
    display input-fname.
stop run.

display-program-header.
    display "----------------------".
    display "ISBN-VERIFYING PROGRAM".
    display "This Cobol program determines the validity of 10-digit ISBNs from".
    display "an input text file of your choice.".
    display "----------------------".
