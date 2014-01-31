!
!  OX/MAIN. Main program.
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
  string orsonPrelude  :− ''.op''   !  Suffix of an Orson prelude file.
  string orsonSource   :− ''.os''   !  Suffix of an Orson source file.
  string version       :− ''0.99''  !  Version of this program.

!  MAIN. Main program.

  main :−
   (with

!  READ FILE. Cross-reference an Orson source program whose pathname is PATH.

     readFile :−
      (form (string path) void:
       (for bool ok, string path in canonical(path)
        do (if ok
            then (if ¬ isEnd(path, orsonPrelude) ∧ ¬ isEnd(path, orsonSource)
                  then fail(''Unexpected suffix in '%s'.'': path))
                 (if ¬ open(source, path, ''r'')
                  then fail(''Cannot open "%s".'': path))
                 sourceNumber := 0
                 sourcePath := path
                 state := 0
                 nextChar()
                 nextToken()
                 nextProgram()
                 write(eop)
                 (if ¬ close(source)
                  then fail(''Cannot close '%s'.'': path))
            else fail(''Cannot open '%s'.'': path))))

!  SET HOW MANY. Set NUMBER to DIGITS from the option CH.

     setHowMany :−
      (proc (var int number, char ch, string digits, int min) void:
       (for bool ok, int howMany in convert(int, digits)
        do (if ok
            then (if howMany ≥ min
                  then number := howMany
                  else fail(''-%c must be at least %i.'': min))
            else fail(''-%c must be an integer.'': ch))))

!  SET OPTION. Assert the Boolean option FLAG.

     setOption :−
      (form (var bool flag) void:
        flag := true)

!  WRITE VERSION. Write a short string describing this program.

     writeVersion :−
      (form () void:
        writeln(errput, me & '', version '' & version & '.'))

!  Visit each argument on the command line, and do what it says. Finally, write
!  a cross reference table to OUTPUT.

    do (for char option, string value in command(''avz'', '' cln'')
        do (case option
            of ' ': readFile(value)
               'a': setOption(asciiing)
               'c': setHowMany(columnsPerName,  'c', value, 1)
               'l': setHowMany(digitsPerNumber, 'd', value, 1)
               'n': setHowMany(numbersPerLine,  'n', value, 0)
               'v': writeVersion()
               'z': setOption(accenting)))
       reportNames())
)
