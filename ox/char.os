!
!  OX/CHAR. Read characters from a source file.
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

(prog
  var char   ch                !  Last char from SOURCE.
  var stream source            !  Current source file.
  var int    sourceNumber      !  Current line number in SOURCE.
  var string sourcePath        !  Pathname of SOURCE.
  var int    state             !  Status of NEXT CHAR.
  var int    tableNumber :− 0  !  Current line number in table.

!  NEXT CHAR. Advance CH to the next char in SOURCE, writing chars to OUTPUT as
!  we go. We use a STATE machine to control how chars are grouped into lines.

  nextChar :−
   (proc () void:
    (with

!  NEXT FIRST CHAR. Advance CH past the first char on a line. We also write the
!  line's TABLE NUMBER to OUTPUT.

      nextFirstChar :−
       (form () void:
         ch := read(source)
         (if ch = eol
          then sourceNumber += 1
               tableNumber += 1
               writeln(''%0*i'': digitsPerNumber, tableNumber)
          else if ch = eos
               then state := 2
               else sourceNumber += 1
                    tableNumber += 1
                    write(''%0*i '': digitsPerNumber, tableNumber)
                    (if ¬ accenting ∧ width(ch) = 0
                     then write(' '))
                    (if asciiing ∧ ¬ isAscii(ch)
                     then write('_')
                     else write(ch))
                    state := 1))

!  NEXT LATER CHAR. Advance CH past a later char.

      nextLaterChar :−
       (form () void:
         ch := read(source)
         (if ch = eol
          then writeln()
               state := 0
          else if ch = eos
               then writeln()
                    state := 2
               else (if ¬ accenting ∧ width(ch) = 0
                     then write(' '))
                    (if asciiing ∧ ¬ isAscii(ch)
                     then write('_')
                     else write(ch))))

!  This is NEXT CHAR's body.

     do (case state
         of 0: nextFirstChar()
            1: nextLaterChar()
            2: skip)))
)
