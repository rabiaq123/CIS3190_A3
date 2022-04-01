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
01 record-read.
   02 line-read            pic x(10).

working-storage section.
01 ws-fname                pic x(30).
01 feof                    pic 9.
01 i                       pic 9(2). *> iterator for entries
01 j                       pic 9(2). *> iterator for characters within an entry 
01 k                       pic 9(2). *> multiplier for ISBN digits when calculating expected check digit
01 num-entries             pic 9(2).
01 isbn-list.
    02 isbn-line           occurs 50 times.
        03 isbn-char       pic x occurs 10 times.
01 flags.                  *> when set to 1, ISBN is automatically incorrect
    02 has-invalid-alpha   pic 9 occurs 50 times.
    02 has-invalid-check   pic 9 occurs 50 times.
    02 has-leading-zero    pic 9 occurs 50 times.
    02 has-trailing-zero   pic 9 occurs 50 times.
    02 has-trailing-upperX pic 9 occurs 50 times.
    02 has-trailing-lowerX pic 9 occurs 50 times.
01 check-vars.             *> one element allocated for every ISBN
    02 sum-for-check       pic 9(3) occurs 50 times value 0. *> step one of calculating expected check digit value
    02 product-for-check   pic 9(3) value 0. *> used to calculate sum-for-check
    02 mod-for-check       pic 9(2) occurs 50 times value 0. *> step two of calculating expected check digit value
    02 expected-check      pic 9(2) occurs 50 times value 0.
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
*> handling check digit exception for when x mod 11 gives 0: inspired by site shared by prof
    *> https://bisg.org/page/conversionscalculat/Conversion--Calculations-.htm.


procedure division.
    perform displayProgramInfo.
    perform readISBN.
    perform evaluateISBN.
    perform displayEndMessage.
stop run.


evaluateISBN.
    display space.
    *> loop through the following paragraphs for every ISBN
    perform isValid through checkSUM
        varying i from 1 by 1
        until i > num-entries.
    *> display ISBN status based on flag status
    perform displayStatus
        varying i from 1 by 1
        until i > num-entries.


isValid.
    *> reset after reading each ISBN
    move 0 to has-invalid-alpha(i).
    *> set the appropriate flags for any ISBN containing an invalid alphabetic char
    perform checkAlpha
        varying j from 1 by 1
        until j > 10 or has-invalid-alpha(i) = 1.


*> this paragraph doesn't get called explicitly, but 
*> running `perform isValid through checkSUM` in evaluateISBN allows it to be called every iteration of the loop,
*> since any paragraph between (and including) isValid and checkSUM would be executed with that perform statement.
checkLeadingAndTrailingChars.
    *> reset after reading each ISBN
    move 0 to has-leading-zero(i).
    move 0 to has-trailing-zero(i).
    move 0 to has-trailing-upperX(i).
    move 0 to has-trailing-lowerX(i).
    *> set the appropriate flags for any ISBN containing trailing/leading Xs or 0s
    if isbn-char(i,1) = 0 then
        move 1 to has-leading-zero(i)
    end-if.
    if isbn-char(i,10) = 0 then
        move 1 to has-trailing-zero(i)
    end-if.
    if isbn-char(i,10) = 'x' then
        move 1 to has-trailing-lowerX(i)
    end-if.
    if isbn-char(i,10) = 'X' then
        move 1 to has-trailing-upperX(i)
    end-if.


checkSUM.
    perform calculateExpectedCheck.
    *> set invalid check flag to 1 when expected check digit doesn't match check digit in ISBN
    if mod-for-check(i) = 0 and function numval(isbn-char(i,10)) = 0 then
        move 0 to has-invalid-check(i)
    else
        if expected-check(i) = function numval(isbn-char(i,10)) then
            move 0 to has-invalid-check(i)
        else if expected-check(i) = 10 and (isbn-char(i,10) = 'X' or = 'x') then
            move 0 to has-invalid-check(i)
        else
            move 1 to has-invalid-check(i)
        end-if
    end-if.


calculateExpectedCheck.
    *> STEP 1: calculate sum of the products of all digits multiplied by their place
    move 10 to k.
    move 0 to sum-for-check(i).
    perform varying j from 1 by 1 until j > 9
        compute product-for-check = k * function numval(isbn-char(i,j))
        compute sum-for-check(i) = sum-for-check(i) + product-for-check
        subtract 1 from k
    end-perform.
    *> STEP 2: calculate remainder on division of sum by 11
    compute mod-for-check(i) = function mod(sum-for-check(i),11). *> if 0, check digit is expected to be 0 as well
    *> STEP 3: get expected check digit
    compute expected-check(i) = 11 - mod-for-check(i).


checkAlpha.
    if isbn-char(i,j) is alphabetic then
        if j >= 1 and <= 9 then 
            move 1 to has-invalid-alpha(i)
        else if j = 10 *> check digit
            if isbn-char(i,j) not = "X" and not = "x" then
                move 1 to has-invalid-alpha(i)
            end-if
        end-if
    end-if.


displayStatus.
    display isbn-line(i) with no advancing
    if has-invalid-alpha(i) = 1 then
        if isbn-char(i,10) is alphabetic and (isbn-char(i,10) is not = 'X' and not = 'x') then
            display " incorrect, contains a non-digit/X in check digit"
        else
            display " incorrect, contains a non-digit"
        end-if
    else if has-invalid-check(i) = 1 then
        *> 'invalid' is used when there is an unexpected check digit
        display " correct, but not valid (invalid check digit)"
    else
        perform displayCorrectAndValid
    end-if.

*> display message corresponding to the flags set
displayCorrectAndValid.
    *> 'correct' is used when there is no invalid usage of a non-digit.
    display " correct and valid" with no advancing
    if has-leading-zero(i) = 1 or has-trailing-zero(i) = 1 or has-trailing-lowerX(i) = 1 or has-trailing-upperX(i) = 1 then
        display " with" with no advancing
        if has-leading-zero(i) = 1 then
            display " [leading zero]" with no advancing
        end-if
        if has-trailing-zero(i) = 1 then
            display " [trailing zero]" with no advancing
        end-if
        if has-trailing-lowerX(i) = 1 then
            display " [trailing lowercase x]" with no advancing
        end-if
        if has-trailing-upperX(i) = 1 then
            display " [trailing uppercase X]" with no advancing
        end-if
    end-if.
    display space.


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


*> store all lines read in into string array/table of ISBNs
storeISBNs.
    read input-file at end move 1 to feof
        not at end
            move record-read to isbn-line(num-entries)
            add 1 to num-entries
    end-read.


getFilename.
    display "Enter the filename to read ISBNs from: " with no advancing.
    accept ws-fname.
    perform checkFileExists.


checkFileExists.
    call "CBL_CHECK_FILE_EXIST" using ws-fname file-info.
    move return-code to file-status.
    if return-code not = 0 then
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
