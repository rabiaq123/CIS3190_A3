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
   02 line-read            pic x(10).

working-storage section.
01 ws-fname                pic x(30).
01 feof                    pic 9.
01 i                       pic 9(2). *> iterator for entries
01 j                       pic 9(2). *> iterator for characters within an entry 
01 num-entries             pic 9(2).
01 isbn-list.
    02 isbn-line           occurs 50 times.
        03 isbn-char    pic x occurs 10 times.
*> 01 isbn-converted          pic 9(10). *> temp variable to store int version of ISBN 
01 is-alpha-flag           pic 9 value 0. *> when set to 1, ISBN is automatically incorrect
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

*> RESOURCES USED ---------------------------------------------------------------------------
*> error handling for non-existant input files: inspired by prof's blog post on  
    *> https://craftofcoding.wordpress.com/2021/03/22/coding-cobol-checking-a-file-exists/


procedure division.
    perform displayProgramInfo.
    perform readISBN.
    perform isValid through checkSUM
        varying i from 1 by 1
        until i > num-entries.
    perform displayEndMessage.
stop run.

isValid.
    display space.
    display "isValid".
    *> could have different flags be set for different things
    
    *> check if any of the first nine digits contain anything other than numbers
        *> if yes, then 'incorrect, contains a non-digit'
    *> check if the check digit is a non-digit other than X (X and x are allowed)
        *> if yes, then 'incorrect, contains a non-digit/X in check digit'
    display "in check non digit".
    display "isbn (" i ") is: " isbn-line(i).
    move 0 to is-alpha-flag. *> reset before reading each ISBN
    perform checkNonDigit
        varying j from 1 by 1
        until j = 9.
    
    *> call checkSUM to see if the value of the expected check digit matches the actual check digit value
        *> if not, then 'correct, but not valid (invalid check digit)'

    *> NOTE that incorrect is used when a non-digit value (other than X/x) is used.
    *> X/x can only be used in the check digit's place, to represent 10.
    *> correct is used when there is no invalid usage of a non-digit.
    *> correct does not mean valid - if the check digit is not what it should be, the ISBN is invalid.

checkNonDigit.
    display "char " j " is: " isbn-char(i,j).
    if isbn-char(i,j) is alphabetic then
        move 1 to is-alpha-flag
    end-if.

checkSUM.
    display space.
    display "checkSUM".
    *> NOTE that a remainder of 0 means 0 should be the check digit,
    *> according to https://bisg.org/page/conversionscalculat/Conversion--Calculations-.htm.

readISBN.
    *> perform error checking for invalid input filename
    move 1 to file-status.
    display space.
    perform getFilename until file-status=0.
    *> open and read input file
    move 1 to num-entries.
    open input input-file.
    perform storeISBNs until feof=1.
    close input-file.
    subtract 1 from num-entries.
    *> REMOVE FOR SUBMISSION checking whether ISBNs are being stored properly
    *> display space.
    *> perform displayISBNs 
    *>     varying i from 1 by 1 
    *>     until i > num-entries.

*> REMOVE FOR SUBMISSION
displayISBNs.
    display i.
    display isbn-line(i).

*> store all lines read in into string array/table of ISBNs
storeISBNs.
    read input-file at end move 1 to feof
        not at end
            move isbn-record to isbn-line(num-entries)
            add 1 to num-entries
    end-read.

getFilename.
    display "Enter the filename to read ISBNs from: " with no advancing.
    accept ws-fname.
    perform checkFileExists.

checkFileExists.
    call "CBL_CHECK_FILE_EXIST" using ws-fname file-info.
    move return-code to file-status.
    if return-code not =0 then
        display "Error: File does not exist."
    end-if.

displayEndMessage.
    display space.
    display "------------------------------".
    display "All ISBNs have been evaluated.".
    display "Exiting program...".
    display "------------------------------".

displayProgramInfo.
    display space.
    display "----------------------".
    display "ISBN-VERIFYING PROGRAM".
    display "This Cobol program determines the validity of 10-digit ISBNs from".
    display "an input text file of your choice.".
    display "----------------------".
