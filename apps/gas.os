!
!  APPS/GAS. Show the alignment and size of a GNU C type.
!
!  Copyright © 2012 James B. Moen.
!
!  This  program is free  software: you  can redistribute  it and/or  modify it
!  under the terms  of the GNU General Public License as  published by the Free
!  Software Foundation,  either version 3 of  the License, or  (at your option)
!  any later version.
!
!  This program is distributed in the  hope that it will be useful, but WITHOUT
!  ANY  WARRANTY;  without even  the  implied  warranty  of MERCHANTABILITY  or
!  FITNESS FOR  A PARTICULAR PURPOSE.  See  the GNU General  Public License for
!  more details.
!
!  You should have received a copy of the GNU General Public License along with
!  this program.  If not, see <http://www.gnu.org/licenses/>.
!

!  The form C'S lets a GCC type appear in an Orson program, but it requires the
!  alignment and size of the type as arguments. The program GAS queries GCC for
!  the alignment and size of such a type. Suppose that T is the name of the GCC
!  type, and the subscripted F's are zero or more GCC files that define T. Then
!  the command:
!
!    gas T F₁ F₂ ... Fⱼ
!
!  shows the alignment and size of T on standard output. For example:
!
!    gas FILE stdio.h
!
!  shows the alignment and size of the type FILE, as defined by <stdio.h>. This
!  is done by making a short GCC program on the file TEMP PATH, compiling it to
!  the file TEMP BINARY, executing TEMP BINARY, then deleting both TEMP files.

(load ''lib.command'')     !  Process command line arguments.
(load ''lib.fail'')        !  Terminate a program with an error message.
(load ''lib.file'')        !  Input and output on file streams.
(load ''lib.C.stdlib:c'')  !  General utilities.

(prog
  var stream temp                      !  Stream to temp file.
  string     tempBinary :− ''a.out''   !  Binary of temp file.
  string     tempPath   :− ''Temp.c''  !  Pathname of temp file.

!  WRITE PROGRAM. Make a short GCC program that displays the alignment and size
!  of the type NAME on standard output.

  writeProgram :−
   (form () void:
    (with var string name :− nil
     do (if ¬ open(temp, tempPath, ''w'')
         then fail(''Cannot open '' & tempPath & '.'))
        writeln(temp, ''#include <stdio.h>'')
        (for char, string value in command(ϵ, '' '')
         do (if name = nil
             then name := value 
             else writeInclude(value)))
        (if name = nil
         then fail(''Type name expected.'')
         else writeMain(name))
        (if ¬ close(temp)
         then fail(''Cannot close '' & tempPath & '.'))))

!  WRITE MAIN. Make the MAIN method of the GCC program.

  writeMain :−
   (form (string term) void:
    (with
      string term :− (past term)
      foj    w    :− writeln
     do w(temp, ''int main()'')
        w(temp, ''{ fprintf(stdout, "align %%i\\n", __alignof__(%s));'': term)
        w(temp, ''  fprintf(stdout, "size  %%i\\n", sizeof(%s)); }'': term)))

!  WRITE INCLUDE. Make an #INCLUDE declaration for the GCC program.

  writeInclude :−
   (form (string file) void:
    (if file ≠ ''stdio.h''
     then writeln(temp, ''#include <%s>'': file)))

!  RUN PROGRAM. Compile the GCC program on TEMP PATH to TEMP BINARY and run it.
!  Delete TEMP BINARY and TEMP PATH when we're done.

  runProgram :−
   (form () void:
     runCommand(''gcc '' & tempPath) ∧ runCommand(''./'' & tempBinary)
     runCommand(''rm -f '' & tempBinary & ' ' & tempPath))

!  RUN COMMAND. Execute a shell COMMAND. Test if it was successful.

  runCommand :−
   (form (string command) bool:
     c's(int, ''system'': command) = 0)

!  MAIN. Main program.

  main :− (writeProgram(); runProgram())
)
