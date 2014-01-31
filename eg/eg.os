!
!  EG. Write random strings from a context-free grammar.
!
!  Copyright © 2014 James B. Moen.
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

(load ''lib.ascii'')    !  Operations on ASCII characters.
(load ''lib.bitset'')   !  Finite sets of small integers.
(load ''lib.buffer'')   !  Fixed length linear queues.
(load ''lib.command'')  !  Process Unix command line arguments.
(load ''lib.convert'')  !  Convert a string to an integer or a real.
(load ''lib.dynamic'')  !  Dynamic memory allocation with explicit release.
(load ''lib.fail'')     !  Terminate a program with an error message.
(load ''lib.file'')     !  Input and output on file streams.
(load ''lib.krig'')     !  Knuth's Random Int Generator.
(load ''lib.seed'')     !  Initialize a random number generator.
(load ''lib.string'')   !  Operations on strings.
(load ''lib.vlq'')      !  Variable length queues.
(load ''lib.width'')    !  Determine columns needed to write simple objects.

(prog

!  Constants and types.

  type mut sq      :− vlq(string)  !  A queue of strings.
  string   suffix  :− ''.eg''      !  Pathname suffix.
  string   version :− ''0.2''      !  This program's version.

!  Variables.

  var int  howMany   :− 1          !  (-c) How many strings to generate.
  var int  maxDepth  :− 1024       !  (-d) Max generator recursions.
  var int  maxErrs   :− 5          !  (-e) Max error messages written.
  var int  maxLength :− high(int)  !  (-h) Max length of generated strings.
  var int  maxRepeat :− 10         !  (-m) The value of ∞.
  var int  minLength :− 1          !  (-l) Min length of generated strings.
  var sq   paths                   !  A queue of pathnames.
  var int  seed                    !  (-s) Seed for random numbers.
  var bool tracing                 !  (-t) Are we writing a debug trace?
  var int  widening  :− 1          !  (-b) How likely to revisit disjuncts?
)

(load ''char'')      !  Read characters and report errors.
(load ''codon'')     !  Objects that represent grammar rules.
(load ''debug'')     !  Write debugging information.
(load ''token'')     !  Read tokens.
(load ''parse'')     !  Parse grammar rules.
(load ''generate'')  !  Write random strings.

(prog

!  MAIN. Main program. Start by initializing.

  main :−
   (form () void:
     init(paths)
     seed := timeSeed(true)

!  Visit command line options and do what they say.

     (for char option, string value in command(''tv'', '' bcdehlms'')
      do (with

!  BOOL OPTION. Set FLAG to TRUE.

           boolOption :−
            (form (var bool flag) void:
              flag := true)

!  ERROR OPTION. Set the maximum number of error messages for each source.

           errorOption :−
            (form () void:
             (if value = ''all''
              then maxErrs := high(int)
              else (for bool ok, int count in convert(int, value)
                    do (if ok
                        then maxErrs := count
                        else fail(''-w must be an integer or 'all'.'')))))

!  INT OPTION. Convert VALUE to a nonnegative integer, and set NUMBER to it. If
!  VALUE can't be converted, then assert an error about OPTION.

           intOption :−
            (proc (var int number) void:
             (for bool ok, int what in convert(int, value)
              do (if ok
                  then number := what
                  else fail(''Expected integer for -%c.'': option))))

!  PATH OPTION. Add VALUE to the queue PATHS.

           pathOption :−
            (form () void:
             (if isEnd(value, suffix)
              then enqueue(paths, copy(value))
              else fail(''Unexpected suffix in '%s'.'': value)))

!  VERSION OPTION. Write version information to OUTPUT and halt.

           versionOption :−
            (form () void:
              writeln(''Eg (exempli gratia), version %s.'': version)
              exit(0))

!  Dispatch on OPTION to a form call that handles it.

          do (case option
              of ' ': pathOption()
                 'b': intOption(widening)
                 'c': intOption(howMany)
                 'd': intOption(maxDepth)
                 'e': errorOption()
                 'h': intOption(maxLength)
                 'l': intOption(minLength)
                 'm': intOption(maxRepeat)
                 's': intOption(seed)
                 't': boolOption(tracing)
                 'v': versionOption())))

!  Test length bounds.

     (if minLength > maxLength
      then fail(''-l must be less than or equal to -h.''))

!  Parse the grammar, Pass One. We determine which names are defined by rules.

     (for string path in elements(paths)
      do (in pass(path, 1)
          do next1Grammar()))

!  Parse the grammar, Pass Two. We translate rules to CODONs.

     (for string path in elements(paths)
      do (in pass(path, 2)
          do next2Grammar()))

!  If there were no syntax errors, then we generate strings from the translated
!  rules.

     (if ¬ hasErrs
      then generate()))

!  BEGIN. Run it.

  begin :− main()
)
