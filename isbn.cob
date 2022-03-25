*> Rabia Qureshi
*> 1046427
*> March 25, 2022
*> Program to determine the validity of 10-digit ISBNs from a file

identification division.
program-id. isbn.

environment division.
input-output section.
file-control.
select input-file assign to dynamic ws-fname
   organization is line sequential.
 
data division.
file section.
fd input-file.
01 isbn-record.
   02 isbn     pic x(10).

working-storage section.
01 ws-fname    pic x(30).
01 feof        pic 9.
01 i           pic 9(2).
01 isbns       occurs 50 times.
    02 ws-isbn pic x(10).

procedure division.
    perform display-program-header.
    perform readISBN.

stop run.

readISBN.
    display "Enter the filename: " with no advancing.
    accept ws-fname.
    *> open and read input file, perform error checking for invalid input filename
    move 1 to i.
    open input input-file.
    perform store-isbns until feof=1.
    close input-file.
    *> checking whether ISBNs are being stored properly
    perform display-isbns 
       varying i from 1 by 1 
       until i > 10.

display-isbns.
    display i.
    display isbns(i).

store-isbns.
    read input-file at end move 1 to feof
        not at end
           move isbn-record to isbns(i)
           add 1 to i
    end-read.
       
display-program-header.
    display "----------------------".
    display "ISBN-VERIFYING PROGRAM".
    display "This Cobol program determines the validity of 10-digit ISBNs from".
    display "an input text file of your choice.".
    display "----------------------".
