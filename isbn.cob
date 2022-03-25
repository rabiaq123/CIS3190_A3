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
   02 isbn                 pic x(10).

working-storage section.
01 ws-fname                pic x(30).
01 feof                    pic 9.
01 i                       pic 9(2).
01 isbns                   occurs 50 times.
    02 ws-isbn             pic x(10).
01 file-info.
    02 file-size           pic x(8) comp-x.
    02 file-date.
        03 f-day           pic x comp-x.
        03 f-month         pic x comp-x.
        03 f-year          pic x(2) comp-x.
    02 file-time.
        03 f-hours         pic x comp-x.
        03 f-minutes       pic x comp-x.
        03 f-seconds       pic x comp-x.
        03 f-hundredths    pic x comp-x.
01 file-status             pic 9.


procedure division.
    perform display-program-header.
    perform readISBN.
stop run.

readISBN.
    *> perform error checking for invalid input filename
    move 1 to file-status.
    display space.
    perform get-filename until file-status=0.
    *> open and read input file
    move 1 to i.
    open input input-file.
    perform store-isbns until feof=1.
    close input-file.
    *> checking whether ISBNs are being stored properly
    display space.
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
       
get-filename.
    display "Enter the filename to read ISBNs from: " with no advancing.
    accept ws-fname.
    perform check-file-exists.

*> inspired by prof's blog post on error handling for non-existant files 
*> (https://craftofcoding.wordpress.com/2021/03/22/coding-cobol-checking-a-file-exists/)
check-file-exists.
    call "CBL_CHECK_FILE_EXIST" using ws-fname file-info.
    move return-code to file-status.
    if return-code not =0 then
        display "Error: File does not exist."
    end-if.

display-program-header.
    display "----------------------".
    display "ISBN-VERIFYING PROGRAM".
    display "This Cobol program determines the validity of 10-digit ISBNs from".
    display "an input text file of your choice.".
    display "----------------------".
