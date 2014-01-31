!
!  APPS/SERIES. Write a finite series of integers.
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

!  SERIES writes a finite series of INTs to standard OUTPUT, under control of a
!  PRINTF-style format string (see "man 3 printf"). It's similar to SEQ, but it
!  uses integers instead of reals (see "man seq"). Here's the SERIES command.
!
!    series F B E S
!
!  Here F is a format string, and B, E, S are integer constants. B is the first
!  integer in the series, E is the last, and S is the increment. For example,
!
!    series "%i" 0 10 2
!
!  prints the integers 0 2 4 6 8 10, one per line. If any arguments are omitted
!  then their defaults take effect, like this.
!
!    series F B E  ≡  series F B E   1
!    series F E    ≡  series F 0 E−1 1
!
!  This is the same way the arguments of Orson's ENUM work. Also like ENUM, any
!  attempt to make a nonterminating series will cause an error. Results will be
!  undefined if we get an arithmetic overflow/underflow, or if F does not match
!  its integer argument.

(load ''lib.convert'')  !  Convert a string to an integer or a real.
(load ''lib.fail'')     !  Terminate a program with an error message.
(load ''lib.file'')     !  Input and output on Unix file streams.

(prog

!  MAIN. Main program. Get BEGIN, END, and STEP from the command line, and fill
!  in their defaults.
  
  main :−
   (form () void:
    (case argc()
     of 3: range( ''0'', arg(2), −1,  ''1'')
        4: range(arg(2), arg(3),  0,  ''1'')
        5: range(arg(2), arg(3),  0, arg(4))
     none: fail(''Wrong number of arguments'')))

!  RANGE. Convert the strings BEGIN, END, and STEP to integers. Assert an error
!  if all three cannot be converted.

  range :−
   (proc (string begin, string end, int Δend, string step) void:
    (with string message :− ''Integer expected instead of %s''
     do (for bool ok, int begin in convert(int, begin)
         do (if ok
             then (for bool ok, int end in convert(int, end)
                   do (if ok
                       then (for bool ok, int step in convert(int, step)
                             do (if ok
                                 then loop(begin, end + Δend, step)
                                 else fail(message: step)))
                       else fail(message: end)))
             else fail(message: begin)))))

!  LOOP. Generate an integer series from BEGIN to END in steps of STEP, writing
!  its values to OUTPUT via FORMAT. If the series would be infinitely long then
!  assert an error instead.

  loop :−
   (form (int begin, int end, int step) void:
    (with 
      var int count   :− begin
      string  format  :− arg(1)
      string  message :− ''Limits out of range''
     do (if step < 0
         then (if begin < end
               then fail(message)
               else (while count ≥ end
                     do writeln(format: count)
                        count += step))
         else if step > 0
              then (if begin > end
                    then fail(message)
                    else (while count ≤ end
                          do writeln(format: count)
                             count += step))
              else fail(message))))

!  ARG. Return the INDEXth argument from the command line.

  arg :−
   (form (int index) string:
     argv()[index])

!  START. Run the main program.

  start :− main()
)
