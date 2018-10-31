On October 26th, 2018 I purchased the software assets of Full Moon Software.
Full Moon Software used to be known as Crescent Software.  They produced a line
of excellent development libraries for MS-DOS.  The supported environments were
QuickBASIC 4.x, Microsoft Professional Development System v7.x, and Visual 
Basic for DOS.

The idea behind obtaining these products was to release them to the public
domain to ensure that people could still access these things in the future.
While most developers will have no use for these products in a modern 
develoment environment, they still have value as an example of "how it was 
done" back in the heyday of x86 DOS development. 

The software in this repository hasn't been modified from how I received it 
from Ethan Winer, the original author.  While all the source files carry some 
kind of Copyright notice, the software is now in the public domain.

The contents of the installation floppies will be uploaded to the Internet
Archive soon and when the manuals are scanned, they'll be uploaded there
as well.  I'll update this readme file with a link to the manual scan when
it's available.

The original distribution disk files are available here:

http://annex.retroarchive.org/crescent/XREF.ZIP


Gene Buckle, October 27th, 2018

I've attached the text from Full Moon Software's catalog description of 
Don Malin's XREF below.

-------------------------------------------------------------------------------
About Crescent Software:
After 20 years as a professional recording engineer and musician, Ethan
Winer founded Crescent Software in 1986, quickly building it to become the
leading provider of add-on products for use with Microsoft compiled BASIC
for DOS. During that time Ethan wrote numerous articles about DOS BASIC and
assembly language for all of the major programming magazines, and also
served as a contributing editor for PC Magazine. Ethan also received
Microsoft's MVP award every year since 1996 for his assistance in the
Microsoft BASIC programming newsgroups. In 1992 Ethan sold Crescent to his
partner Don Malin, and retired in order to pursue his musical interests.
=============================================================================

DON MALIN'S CROSS REFERENCE PROGRAM (XREF)(tm)
==============================================

The Most Sophisticated Cross-Reference Program Ever Developed
-------------------------------------------------------------

Serious programmers have always understood the value of a good cross-reference 
program, but with today's large, modular applications, much more is often 
necessary. XREF is a sophisticated cross-reference utility program that 
provides comprehensive documentation about an entire application. Unlike other 
cross-reference programs you may have seen that merely list each variable, 
XREF provides a wealth of useful reports that show exactly what is going on in 
your programs. Of course, XREF does a terrific job of listing variables in 
alphabetical order, so you can quickly spot any that are misspelled. But XREF 
goes far beyond that--it is the very best way to completely document your 
entire program. XREF is also ideal when you need to understand and work on 
someone else's programs, or update a program you wrote long ago.
     XREF is extremely easy to use, and all of its operations are handled 
using pull-down menus and dialog boxes. This avoids having to memorize 
confusing commands or option switches. Most aspect of the program, such as 
report and printer settings, are user-definable, and the current setup may be 
saved to individual configuration files or to a system default configuration 
file. Many useful reports can be created, and they may be sent to a printer, a 
disk file, or viewed on-screen in a unique browse mode that lets you scroll 
the report vertically and horizontally. The report browsing feature is 
particularly valuable because the reports are available immediately for 
viewing without tying up your printer or wasting paper.
     XREF examines all of the files that comprise your application, including 
those modules that are external to your main BASIC program file. Further, all 
Include files are read and processed. XREF understands COMMON and SHARED, as 
well as DIM AS, DEFINT, and all the other DEFtype statements. XREF is 
compatible with all versions of Microsoft BASIC for DOS, including GW-BASIC 
and BASICA, so it is ideal when you are converting a large program from those 
earlier dialects. Because XREF processes your program in a single pass, it is 
also extremely fast. XREF reads all of the source code that is associated with 
your program and then parses the individual program objects into tables. 
Object types include key words, DEF FN functions, SUB and FUNCTION procedures, 
line labels, and CONST constants. Other object types include simple numeric, 
string, and TYPE variables, as well as Static and Dynamic numeric, string, and 
TYPE arrays. After all of the source code has been read, XREF creates a 
database of the object tables for further processing. This lets you run other 
reports later on, without having to process the source files again. XREF 
offers many different perspectives on a program, by providing the following 
information:

     * GENERATES SOURCE LISTINGS WITH HEADERS AND PHYSICAL LINE NUMBERS. Line
       numbers can be listed as relative to the beginning of procedures, like
       the BASIC editor, or relative to the start of the source file.

     * COMPLETE CONTROL OVER HOW PROGRAMS ARE LISTED. Each procedure can start
       on a new page if you prefer. XREF also prints a Table of Contents for
       the entire listing showing the starting page number for each procedure.

     * CREATES CALL TREE DIAGRAMS. The XREF Call Tree report shows the
       interrelationships between all of the procedures used in your
       application. The report may be organized either alphabetically, or in
       logical (order of occurrence) order. This helps you to more easily
       visualize the overall structure of your program.

     * GENERATES AN OBJECT SUMMARY REPORT. The Object Summary report lists all
       of your program objects, such as BASIC key words, SUB and FUNCTION
       procedures, simple and TYPE variables, and both Static and Dynamic
       arrays. Objects are grouped by their type and displayed alphabetically.
       Each object is listed showing where it was defined and how many times
       it was used. The BASIC Key Word report shows which statements you have
       used, flagging those that bring in the BASIC floating point library,
       thus increasing the size of your .EXE program file. This report also
       shows which key words are not supported by our P.D.Q. library. The
       Object Summary report also identifies variables referenced only once,
       as well as procedures that are declared or defined but never used.


     * PROVIDES COMPLETE INFORMATION ABOUT EACH VARIABLE. Program elements can
       be listed by procedure along with their type, so you can easily see
       which variables are passed as parameters and which are Static, Dynamic,
       and Shared. If the same element is used in different modules (such as
       COMMON variables), this is listed as well. These features let you know
       immediately which variables are affected by procedures and which are
       private.

     * UNDERSTANDS ALL BASIC SOURCE LISTING METACOMMANDS. Although Microsoft
       no longer documents the various metacommands in their manuals, all
       current versions of the compiler recognize them. The XREF owner's
       manual provides a complete listing of all useful options, and the
       program honors them as it displays the various reports. This lets you
       define titles and subtitles for each page, turn the source listing on
       and off, and so forth.

     * EXTRACTS QUOTED STRINGS FOR SPELL CHECKING. A special utility is
       provided to extract all quoted strings and remarks and write them to a
       file, so you can proof and spell-check them using any word processor. A
       companion utility merges the corrected text back into your program
       files, with the original spacing intact. This feature also helps if you
       need to translate all of a program's text to a foreign language.

     * REMOVES UNUSED LINE NUMBERS FROM OLD PROGRAMS. Many programmers don't
       realize that unnecessary line numbers can slow down the BASIC compiler
       and impinge on its working memory. Line numbers can also make a program
       larger and run more slowly when certain compiler options are used. A
       separate utility is included to remove all line numbers from your
       source files that are not actually needed.

     * SIMPLIFIES MODULARIZATION. A unique report shows all of the variables
       that are referenced both within a given range of source lines and also
       referenced outside that range. If you need to convert a section of code
       (such as a GOSUB routine) into a subprogram or function, this report
       shows you which variables must be passed or shared.

THE FULL MOON PHILOSOPHY

As with all our products, full source code is provided at no additional cost, 
so you can see how the routines were designed and even modify them if you 
want. We genuinely want you to understand how our libraries work and be able 
to learn from them. All of our products are reasonably priced and include free 
technical assistance, but they are licensed for use by only one person using 
one computer at a time. Royalty payments are not required when our routines 
are incorporated into your compiled applications. However, you may not 
distribute our source, object, or library files. If your customers need to 
rebuild your program, they will need their own copy of our product(s).

     "This product can prevent keeping track of variables from becoming a
     nightmare, make debugging easier, help optimize code, and generally
     save you many hours managing your programs...Without question, XREF is
     a valuable tool for all BASIC programmers."--Bill Reilly, BASICPro, 4/93

THE BOTTOM LINE

Don Malin's XREF costs $79 and works with QuickBASIC 4.x, PDS 7.x, and VB/DOS. 
Add $8 for UPS ground shipping to US addresses only (no P.O. boxes); 
Connecticut residents must add 6.0% sales tax or show proof of tax-exempt 
status when ordering. Please call for overnight and foreign shipping costs. We 
accept checks, MasterCard, and VISA. We do accept purchase orders, but they 
must be accompanied by full payment.

Don Malin's XREF(tm) is a trademark of Crescent Software, Inc.