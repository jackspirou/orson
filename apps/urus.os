!
!  APPS/URUS. Print a cross-referenced list of names in an Orson program.
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

!  This program attempts to use data structures from the Orson library whenever
!  possible. In particular, we implement a binary search tree of integer queues
!  using forms from lib.ubst and lib.vlq.

(load ''lib.ascii'')    !  Operations on ASCII chars.
(load ''lib.buffer'')   !  Fixed-length linear queues.
(load ''lib.command'')  !  Process command line arguments.
(load ''lib.convert'')  !  Convert a string to an integer or a real.
(load ''lib.fail'')     !  Terminate a program with an error message.
(load ''lib.file'')     !  Input and output on file streams.
(load ''lib.list'')     !  Operations on lists.
(load ''lib.plain'')    !  Operations on Orson plain names.
(load ''lib.string'')   !  Operations on strings.
(load ''lib.ubst'')     !  Unbalanced binary search trees.
(load ''lib.vlq'')      !  Variable-length queues.
(load ''lib.width'')    !  Determine columns needed to display a char.

(prog

!  Constants.

  cha    apostropheChar  :− '\''     !  Because it's ugly.
  cha    backslashChar   :− '\\'     !  Because it's ugly.
  inj    digitsPerNumber :− 5        !  Digits in a line number.
  cha    doubleQuoteChar :− '"'      !  Because it's ugly.
  inj    maxNameLength   :− 1024     !  Max number of chars in a name.
  cha    noBreakChar     :− '\#A0'   !  Nonbreaking blank.
  string orsonPrelude    :− ''.op''  !  Suffix of an Orson prelude file.
  string orsonSource     :− ''.os''  !  Suffix of an Orson source file.
  cha    underscoreChar  :− '_'      !  Because it's ugly.
  cha    unknownChar     :− '□'      !  Placeholder for unknown width char.
  string version         :− ''0.9''  !  Version of this program.

!  Options and their default values, which will produce a cross reference table
!  with 79-char lines. Try 22 numbers per line for a double-length table.

  var bool asciiing       :− false  !  (-a) Show names in ASCII.
  var int  columnsPerName :− 32     !  (-c) Show this many columns per name.
  var int  numbersPerLine :− 8      !  (-n) Show this many numbers per line.
  var bool writing        :− false  !  (-p) Should we print source files?

!  Global variables.

  var char                   ch               !  Last char from SOURCE.
  var int                    lineNumber :− 0  !  Current line number.
  var buffer(maxNameLength)  nameBuffer       !  The name being read.
  var ubst(string, vlq(int)) searchTree       !  Map names to numbers.
  var stream                 source           !  Orson source file.

!  HAS SUFFIX. Test if PATH ends with a string in the list SUFFIXES.

  hasSuffix :−
   (form (string path, list suffixes) bool:
     any(
      (form (obj suffix) bool:
       (with int offset :− length(path) − length(suffix)
        do offset > 0 ∧ path + offset = suffix)),
      suffixes))

!  NEXT CHAR. Read the next char CH from SOURCE, and maybe write it.

  nextChar :−
   (form () void:
    (if writing
     then (if ¬ asciiing ∨ isAscii(ch)
           then write(ch)
           else write(underscoreChar)))
    ch := read(source))

!  NEXT COMMENT. Skip a comment.

  nextComment :−
   (form () void:
     nextChar()
     (while ch ≠ eos ∧ ch ≠ eol
      do nextChar()))

!  NEXT NUMBER. Skip a series of chars until we find a non letter or digit.

  nextNumber :−
   (form () void:
    (while
      nextChar()
      isLetterOrDigit(ch)))

!  NEXT HOOK NAME. Read a hook name, and add it to the search tree.

  nextHookName :−
   (form () void:
     nextChar()
     empty(nameBuffer)
     append(nameBuffer, '?')
     (while isPlain(ch)
      do append(nameBuffer, ch)
         nextChar())
     recordName(nameBuffer{string}))

!  NEXT PLAIN OR SECRET NAME. Read a plain name or a secret name, and add it to
!  the search tree.

  nextPlainOrSecretName :−
   (form () void:
     empty(nameBuffer)
     (while isPlain(ch)
      do append(nameBuffer, ch)
         nextChar())
     recordName(nameBuffer{string}))

!  NEXT QUOTED NAME. Read a quoted name, and add it to the search tree.

  nextQuotedName :−
   (form () void:
     empty(nameBuffer)
     append(nameBuffer, doubleQuoteChar)
     nextChar()
     (while ch ≠ doubleQuoteChar ∧ ch ≠ eol ∧ ch ≠ eos
      do append(nameBuffer, ch)
         nextChar())
     (if ch = doubleQuoteChar
      then append(nameBuffer, doubleQuoteChar)
           nextChar())
     recordName(nameBuffer{string}))

!  NEXT CHAR OR STRING. Skip a char constant or a string constant.

  nextCharOrString :−
   (form () void:
    (with

!  NEXT CHARACTER. Skip a char constant, minus its first APOSTROPHE CHAR.

      nextCharacter :−
       (form () void:
        (if ch = backslashChar
         then nextChar()
              (if ch = '#'
               then nextChar()
                    (while isDigit(ch, 16)
                     do nextChar())
               else nextChar())
         else nextChar())
        nextChar())

!  NEXT STRING. Skip a string constant, minus its first APOSTROPHE CHAR.

      nextString :−
       (form () void:
         nextChar()
         (while
          (if ch = apostropheChar
           then nextChar()
                (if ch = apostropheChar
                 then nextChar()
                      (if ch = apostropheChar
                       then nextChar())
                      false
                 else true)
           else (if ch = backslashChar
                 then nextChar()
                      nextChar()
                 else nextChar())
                true)))

!  This is NEXT CHAR OR STRING's body. Two APOSTROPHE CHARs in a row will begin
!  a string constant, and one will begin a char constant.

     do nextChar()
        (if ch = apostropheChar
         then nextString()
         else nextCharacter())))

!  NEXT SOURCE. Read an Orson source file. We add names and line numbers to the
!  search tree as we go.

  nextSource :−
   (form () void:
     ch := read(source)
     (while ch ≠ eos
      do writeLineNumber()
         (while ch ≠ eol ∧ ch ≠ eos
          do (if ch = apostropheChar
              then nextCharOrString()
              else if isDigit(ch) ∨ ch = '#'
                   then nextNumber()
                   else if isPlain(ch)
                        then nextPlainOrSecretName()
                        else if ch = '?'
                             then nextHookName()
                             else if ch = doubleQuoteChar
                                  then nextQuotedName()
                                  else if ch = '!'
                                       then nextComment()
                                       else nextChar()))
         (if ch = eol
          then nextChar())))

!  RECORD NAME. Add NAME and the current LINE NUMBER to the binary search tree.
!  Note that if NAME appears many times on a line, then its line number appears
!  the same number of times in one of the tree's queues.

  recordName :−
   (proc (string name) void:
    (for bool found, var string key, var vlq(int) value
     in got(searchTree, comp, name)
     do (if ¬ found
         then key := copy(name)
              init(value))
        enqueue(value, lineNumber)))

!  WRITE LINE NUMBER. Increment the current line number and maybe write it.

  writeLineNumber :−
   (form () void:
     lineNumber += 1
     (if writing
      then write(''%0*u '': digitsPerNumber, lineNumber)))

!  WRITE SEARCH TREE. Do an inorder traversal of the search tree to write names
!  in lexicographic order. Following each name, write a list of line numbers on
!  which it appears.

  writeSearchTree :−
   (form () void:
    (for string key, vlq(int) value in pairs(searchTree)
     do (with var int count :− 0
         do writeName(key)
            (for int number in elements(value)
             do (if count = numbersPerLine
                 then count := 0
                      writeln()
                      write(''%*s'': columnsPerName, '' ''))
                write('' %0*i'': digitsPerNumber, number)
                count += 1)
            (if count > 0
             then writeln()))))

!  WRITE NAME. Write NAME left justified and blank filled in a field of COLUMNS
!  PER NAME chars. If we are ASCIIING, then write non-ASCII chars as UNDERSCORE
!  CHARs.

  writeName :−
   (form (string name) void:
    (with var int columns :− 0
     do (for breaker() break, char ch in elements(name)
         do (if asciiing
             then (if columns + 1 > columnsPerName
                   then break()
                   else columns += 1
                        (if isAscii(ch)
                         then write(ch)
                         else write(underscoreChar)))
             else (with int temp :− width(ch)
                   do (case temp
                       of −1: (if columns + 1 > columnsPerName
                               then break()
                               else columns += 1
                                    write(unknownChar))
                           0: (if columns + 1 > columnsPerName
                               then break()
                               else columns += 1
                                    write(noBreakChar)
                                    write(ch))
                        none: (if columns + temp > columnsPerName
                               then break()
                               else columns += temp
                                    write(ch))))))
        (in columnsPerName − columns
         do write(' '))))

!  READ FILE. Cross-reference an Orson source file whose pathname is PATH.

  readFile :−
   (form (string path) void:
    (if path↑ = '-'
     then fail(''Illegal pathname "%s".'': path))
    (if ¬ hasSuffix(path: orsonPrelude, orsonSource)
     then fail(''Unexpected suffix in "%s".'': path))
    (if ¬ open(source, path, ''r'')
     then fail(''Cannot open "%s".'': path))
    nextSource()
    (if writing
     then write(eop))
    (if ¬ close(source)
     then fail(''Cannot close "%s".'': path)))

!  SET NAME LENGTH. Tell URUS to display the first DIGITS chars in names.

  setNameLength :−
   (form (string digits) void:
    (for bool success, int temp in convert(int, digits)
     do (if success
         then (if 1 ≤ temp ≤ maxNameLength
               then columnsPerName := temp
               else fail(''-c must be between 1 and %i.'': maxNameLength))
         else fail(''-c must be an integer.''))))

!  SET NUMBERS PER LINE. Tell URUS to display DIGITS line numbers on every line
!  of the cross reference table.

  setNumbersPerLine :−
   (form (string digits) void:
    (for bool success, int temp in convert(int, digits)
     do (if success
         then (if temp > 0
               then numbersPerLine := temp
               else fail(''-n must be greater than 0.''))
         else fail(''-n must be an integer.''))))

!  SET OPTION. Tell URUS to assert the Boolean option FLAG.

  setOption :−
   (form (var bool flag) void:
     flag := true)

!  WRITE VERSION. Write a short string describing this program.

  writeVersion :−
   (form () void:
     writeln(''Orson cross referencer, version '' & version & '.'))

!  MAIN. This is the main program. We visit each option on the command line and
!  do what it says. Then we write the cross reference table.

  main :−
   (init(searchTree)
    (for char option, string value in command(''apv'', '' cn'')
     do (case option
         of ' ': readFile(value)
            'a': setOption(asciiing)
            'c': setNameLength(value)
            'n': setNumbersPerLine(value)
            'p': setOption(writing)
            'v': writeVersion()))
    writeSearchTree())
)
