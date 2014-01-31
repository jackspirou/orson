!
!  APPS/TABU. Write a series of names as a rectangular table.
!
!  Copyright © 2013 James B. Moen.
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

(load ''lib.buffer'')   !  Fixed length linear queues.
(load ''lib.command'')  !  Process Unix command line arguments.
(load ''lib.convert'')  !  Convert a string to an integer or a real.
(load ''lib.dynamic'')  !  Dynamic memory allocation with explicit release.
(load ''lib.fail'')     !  Terminate a program with an error message.
(load ''lib.vlq'')      !  Variable length queues.

(prog
  char eof          :− eos{char}       !  End of file sentinel.
  int maxNameLength :− 1024            !  Max CHAR0s in a name.
  matrix            :− row var string  !  A rectangular matrix of strings.
  queue             :− vlq(string)     !  A queue of strings.

  var int    columnCount  :− −1     !  How many columns to write.
  var int    columnNumber :− 1      !  The column we're writing now.
  var int    columnWidth  :− 0      !  Width of each column.
  var string delimiter    :− '' ''  !  Delimits columns.
  var bool   hasPathnames :− false  !  Any pathnames on command line?
  var int    lastNonblank           !  Index of last nonblank in DELIMITER.
  var int    lineWidth    :− 79     !  Width of lines to be written.
  var int    longWidth    :− 2      !  Width of DELIMITER.
  var queue  names                  !  Queue of names to be formatted.
  var int    nameCount    :− 0      !  How many names in NAMES.
  var bool   righting     :− false  !  Are we right-justifying names?
  var int    rowCount               !  How many rows to write.
  var int    shortWidth             !  Width of DELIMITER w/o trailing blanks.
  var matrix table                  !  A table of transposed names.
  var bool   transposing  :− false  !  Will we transpose rows and columns?

!  MAIN. Main program. Start by processing command line arguments.

  main :−
   (form () void:
     init(names)
     (for char option, string value in command(''rt'', '' cdw'')
      do (with
 
!  FILE OPTION. Read names from a stream whose pathname is VALUE. Add them to a
!  queue NAMES.

           fileOption :−
            (form () void:
             (for bool ok, stream source in opened(value, ''r'')
              do (if ok
                  then hasPathnames := true
                       readNames(source)
                  else fail(''Cannot open '%s'.'': value))))

!  INTEGER OPTION. Convert VALUE to a nonnegative integer and set WHAT to it.

           integerOption :−
            (proc (var int what) void:
             (for bool ok, int number in convert(int, value)
              do (if ok
                  then what := number
                  else fail(''Illegal option -%c.'': option))))

!  Dispatch on each command line option.

          do (case option
              of ' ': fileOption()
                 'c': integerOption(columnCount)
                 'd': delimiter := value
                 'r': righting := true
                 't': transposing := true
                 'w': integerOption(lineWidth))))

!  If there were no pathnames on the command line, then read names from INPUT.

     (if ¬ hasPathnames
      then readNames(input))

!  If we read no names, then why bother?

     (if columnWidth = 0
      then exit(0))

!  Compute LAST NONBLANK, LONG WIDTH, and SHORT WIDTH.

     lastNonblank := length(delimiter) − 1
     longWidth := count(delimiter)
     shortWidth := longWidth
     (while lastNonblank ≥ 0 ∧ delimiter[lastNonblank] = ' '
      do lastNonblank −= 1
         shortWidth −= 1)

!  Compute LINE WIDTH. If COLUMN COUNT is negative then the user didn't specify
!  it, so compute that too.

     (if columnCount < 0
      then columnCount := lineWidth / (columnWidth + longWidth)
           lineWidth   −= columnCount × (columnWidth + longWidth)
           columnCount += (lineWidth ≥ columnWidth + shortWidth)
           (if columnCount < 1
            then fail(''Columns do not fit on line.''))
      else lineWidth −= (columnCount − 1) × (columnWidth + longWidth)
           lineWidth −= columnWidth − shortWidth
           (if lineWidth < 0
            then fail(''Columns do not fit on line.'')))

!  Dequeue the names, writing COLUMN COUNT names per line. Delimiters that come
!  between columns are written with trailing blanks so they're LONG WIDTH chars
!  wide. Delimiters at the ends of lines are written without trailing blanks so
!  they're SHORT WIDTH chars wide. The final line doesn't end with a delimiter.
!
!  If we're TRANSPOSING, then let TABLE be an array of ROW COUNT rows by COLUMN
!  COUNT columns. Dequeue the names into TABLE and write them out. Otherwise we
!  just write the names out as we dequeue them.

     (if transposing
      then rowCount := nameCount / columnCount
           rowCount += nameCount mod columnCount ≠ 0
           table := fromHeap(rowCount × columnCount, var string)
           (for int columnIndex in columnCount
            do (for int rowIndex in rowCount
                do table[rowIndex, columnIndex] :=
                    (if isEmpty(names)
                     then nil
                     else dequeue(names))))
           (for int rowIndex in rowCount
            do (for int columnIndex in columnCount
                do (with string name :− table[rowIndex, columnIndex]
                    do (if name ≠ nil
                        then writeName(name)
                             (if columnIndex < columnCount − 1 ∧
                                 table[rowIndex, columnIndex + 1] ≠ nil
                              then writeLongDelimiter(name)))))
               (if rowIndex < rowCount − 1
                then writeShortDelimiter()))
           writeln()
      else (while ¬ isEmpty(names)
            do (with string name :− dequeue(names)
                do writeName(name)
                   columnNumber += 1
                   (if isEmpty(names)
                    then writeln()
                    else if columnNumber > columnCount
                         then writeShortDelimiter()
                              columnNumber := 1
                         else writeLongDelimiter(name))))))

!  "␣[]". Return the string variable at indexes ROW INDEX and COLUMN INDEX from
!  TABLE.

  " []" :−
   (form (matrix table, int rowIndex, int columnIndex) var string:
     table[columnCount × rowIndex + columnIndex])

!  READ NAMES. Read names from SOURCE separated by blanks and newlines. Add the
!  names to NAMES. Also compute COLUMN WIDTH, the width of the longest name.

  readNames :−
   (proc (stream source) void:
    (with
      var char ch :− read(source)     !  Last char read from SOURCE.
      var buffer(maxNameLength) name  !  Last name read from SOURCE.
      var int nameWidth               !  Width of NAME.

!  IS NAME CHAR. Test if CH can be part of a name.

      isNameChar :−
       (form (char ch) bool:
         ch ≠ ' ' ∧ ch ≠ eol ∧ ch ≠ eof)

!  This is READ NAMES's body.

     do (while ch ≠ eof
         do (while
             (if ch = eof
              then false
              else if isNameChar(ch)
                   then init(name)
                        nameWidth := 0
                        (while
                          nameWidth += 1
                          append(name, ch)
                          ch := read(source)
                          isNameChar(ch))
                        enqueue(names, copy(name{string}))
                        nameCount += 1
                        (if nameWidth > columnWidth
                         then columnWidth := nameWidth)
                        true
                   else ch := read(source)
                        true)))))

!  WRITE INDENT. Write enough blanks to fill out NAME's column.

  writeIndent :−
   (form (string name) void:
    (in columnWidth − count(name)
     do write(' ')))

!  WRITE LONG DELIMITER. Write DELIMITER. If we're left justifying then we also
!  write enough trailing blanks for NAME.

  writeLongDelimiter :−
   (form (string name) void:
     write(delimiter)
     (if ¬ righting
      then writeIndent(name)))

!  WRITE NAME.  If we're right justifying, then write enough leading blanks for
!  NAME. Also write NAME of course.

  writeName :−
   (form (string name) void:
    (if righting
     then writeIndent(name))
    write(name))

!  WRITE SHORT DELIMITER. Write DELIMITER without trailing blanks.

  writeShortDelimiter :−
   (form () void:
    (for int index in 0, lastNonblank
     do write(delimiter[index]))
    writeln())

!  BEGIN. Run it.

  begin :− main()
)
