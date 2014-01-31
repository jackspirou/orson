!
!  APPS/JC. Justify comments with a given prefix.
!
!  Copyright © 2012 James B. Moen.
!
!  This program is free software: you can  redistribute  it  and/or  modify  it
!  under  the  terms of the GNU General Public License as published by the Free
!  Software Foundation, either version 3 of the License, or  (at  your  option)
!  any later version.
!
!  This program is distributed in the hope that it will be useful, but  WITHOUT
!  ANY  WARRANTY;  without  even  the  implied  warranty  of MERCHANTABILITY or
!  FITNESS FOR A PARTICULAR PURPOSE. See the GNU  General  Public  License  for
!  more details.
!
!  You should have received a copy of the GNU General Public License along with
!  this program. If not, see <http://www.gnu.org/licenses/>.
!

!  In this distribution, comments in source files are filled and justified into
!  79-character lines. I typed them in that way, using some tricks I learned in
!  grad school. The tricks involve being a halfway decent writer, and having an
!  anal-retentive personality. If you want to justify your comments, but you're
!  not like that, then you can use JC instead. It justifies every comment block
!  whose lines have a specified prefix in their leftmost column. The JC command
!  looks like this.
!
!    jc -l n -p σ p₁ p₂ ... pⱼ
!
!  Comment lines begin with the prefix string σ, which defaults to "!␣␣" (where
!  ␣ is a blank). Comment blocks are justified to lines of n columns (including
!  the prefix string), which defaults to 79. The subscripted p's are  pathnames
!  of source files to be justified. Each source file is rewritten in place, via
!  a temporary scratch file.

(load ''lib.buffer'')   !  Fixed length linear queues.
(load ''lib.command'')  !  Process Unix command line arguments.
(load ''lib.convert'')  !  Convert a string to an integer or a real.
(load ''lib.dynamic'')  !  Dynamic memory allocation with explicit release.
(load ''lib.fail'')     !  Terminate a program with an error message.
(load ''lib.file'')     !  Input and output on file streams.
(load ''lib.width'')    !  Determine columns needed to write simple objects.

(prog
  inj    defaultJustified :− 79       !  Default justified comment columns.
  string defaultPrefix    :− ''!  ''  !  Default comment prefix.
  inj    maxLineLength    :− 1024     !  Max CHAR1s in a LINE.

!  BUF0, BUF1. Buffers of CHAR0s and CHAR1s respectively.

  buf0 :− buffer(maxLineLength, char0)
  buf1 :− buffer(maxLineLength, char1)

!  WORD. A word from a comment block, in a queue represented by FIRST and LAST.
!  CHARS is a contiguous series of nonblank characters, and WIDTH is the number
!  of columns needed to write CHARS. NEXT is the next WORD in the queue.

  word :−
   (tuple
     var string   chars,
     var int      width,
     var ref word next)

!  Global variables.

  var char     ch           !  Current char from SOURCE.
  var word     first        !  Its NEXT slot is the first WORD in the queue.
  var int      justified    !  Width of a justified comment line.
  var ref word last         !  Last WORD in the queue.
  var buf1     line         !  Current line from SOURCE.
  var buf1     prefix       !  Comment prefix.
  var int      prefixWidth  !  Width of PREFIX in columns.
  var stream   source       !  Source file to be justified.
  var stream   temp         !  A temporary file.

!  MAIN. Main program. Initialize, then handle command line options.

  main :−
   (form () void:
     empty(prefix)
     append(prefix, defaultPrefix)
     prefixWidth := count(defaultPrefix)
     justified := defaultJustified
     (if ¬ open(temp)
      then fail(''Cannot open temporary file.''))
     (for char option, string value in command(''lp'', '' '')
      do (with

!  FILE OPTION. Open a SOURCE file whose pathname is VALUE and a temporary file
!  TEMP. First copy SOURCE to TEMP, justifying its comments as we go, then copy
!  TEMP back to SOURCE. Close both files when we're finished.

           fileOption :−
            (form () void:
             (if ¬ open(source, value, ''r'')
              then fail(''Cannot open '%s'.'': value))
             (if ¬ open(temp)
              then fail(''Cannot open temporary file.''))
             nextSource()
             (if ¬ close(source)
              then fail(''Cannot close '%s'.'': value))
             (if open(source, value, ''w+'')
              then (with var int ch
                    do reset(temp, 0)
                       (while
                         ch := get(temp)
                         ch ≠ eos
                        do put(source, ch))
                       (if ¬ close(source)
                        then fail(''Cannot close '%s'.'': value))
                       (if ¬ close(temp)
                        then fail(''Cannot close temporary file.'')))
              else fail(''Cannot open '%s'.'': value)))

!  LENGTH OPTION. Set the length of a justified line, including PREFIX. Default
!  is DEFAULT WIDTH.

           lengthOption :−
            (form () void:
             (for bool ok, int value in convert(int, value)
              do (if ok ∧ 1 ≤ value ≤ maxLineLength
                  then justified := value
                  else fail(''-l must be between 1 and %i.'': maxLineLength))))

!  PREFIX OPTION. Set a comment prefix. Default is DEFAULT PREFIX.

           prefixOption :−
            (form () void:
              empty(prefix)
              append(prefix, value)
              prefixWidth := length(prefix))

!  This is COMMAND's body. Dispatch to forms that handle options.

          do (case option
              of ' ': fileOption()
                 'l': lengthOption()
                 'p': prefixOption()))))

!  NEXT SOURCE. Read a file from SOURCE, copying it to the temporary file TEMP.
!  If we encounter comments (starting with PREFIX), then justify them before we
!  write them.

  nextSource :−
   (form () void:
    (with

!  NEXT LINE. Read a LINE from SOURCE.

      nextLine :−
       (form () void:
         empty(line)
         (while
          (if ch = eos
           then false
           else if ch = eol
                then ch := read(source)
                     false
                else append(line, ch)
                     ch := read(source)
                     true)))

!  IS COMMENT. Test if LINE starts with PREFIX, followed by a nonblank.

      isComment :−
       (form () bool:
        (with var bool ok
         do restart(line)
            restart(prefix)
            (while
             (if atEnd(line)
              then ok := false
                   false
              else if atEnd(prefix)
                   then ok := (start(line) ≠ ' ')
                        false
                   else if start(line) = start(prefix)
                        then advance(line)
                             advance(prefix)
                             true
                        else ok := false
                             false))
            restart(line)
            restart(prefix)
            ok))

!  This is NEXT SOURCE's body. Read LINEs. If a LINE is a comment, then enqueue
!  its words. Otherwise dequeue and justify WORDs, then write LINE.

     do emptyQueue()
        ch := read(source)
        (while ch ≠ eos
         do nextLine()
            (if isComment()
             then enqueueWords()
             else dequeueWords()
                  writeln(temp, line)))
        dequeueWords()))

!  EMPTY QUEUE. Initialize the queue of WORDs from a comment block.

  emptyQueue :−
   (form () void:
     first.next := nil
     last := first↓{ref word})

!  ENQUEUE WORDS. Add words from LINE into the queue.

  enqueueWords :−
   (form () void:
    (with
      var buf0     chars  !  The chars of the word.
      var ref word this   !  The WORD to be added to the queue.

!  SKIP PREFIX. Skip the PREFIX that LINE starts with.

      skipPrefix :−
       (form () void:
        (in prefixWidth
         do advance(line)))

!  SKIP BLANKS. Skip a series of zero or more blanks in LINE.

      skipBlanks :−
       (form () void:
        (while ¬ atEnd(line) ∧ start(line) = ' '
         do advance(line)))

!  ENQUEUE WORD. Copy a word from LINE into CHARS. Copy CHARS, make a WORD that
!  holds the copy, and add the WORD to the queue.

      enqueueWord :−
       (form () void:
         empty(chars)
         (while
           append(chars, start(line))
           advance(line)
           ¬ atEnd(line) ∧ start(line) ≠ ' ')
         this        := fromHeap(word)
         this↑.chars := copy(chars{string})
         this↑.width := width(chars{string})
         this↑.next  := nil
         last↑.next  := this
         last        := this)

!  This is ENQUEUE WORDS's body.

     do restart(line)
        skipPrefix()
        (while ¬ atEnd(line)
         do skipBlanks()
            (if ¬ atEnd(line)
             then enqueueWord()))))
 
!  DEQUEUE WORDS. Write the queued WORDs in a justified comment block. First we
!  dequeue enough WORDs to fit on a single line. Then we write PREFIX, followed
!  by those WORDs, separated by blanks. We do this in one of three ways.
!
!    ∙ If the line consists of a single word, but longer than the requested
!      line width, then we write that word by itself.
!
!    ∙ If the line is the final line in the comment block then we write its
!      WORDs separated by single blanks.
!
!    ∙ Otherwise we write the line with as many blanks between its WORDs as
!      are needed to achieve the requested width.  We try to separate every
!      pair of WORDs by the same number of blanks. If we can't, then we add
!      an extra blank between some WORDs. We alternate between adding extra
!      blanks from left to right, and from right to left.
!
!  Continue in this way until queue becomes empty. If it's initially empty then
!  we do nothing.

  dequeueWords :−
   (proc () void:
    (with
      var int      count              !  Words in LINE.
      var bool     direction :− true  !  Add extra blanks L-to-R or R-to-L?
      var int      blanks             !  Blanks in LINE.
      var ref word left               !  Leftmost WORD in a justified line.
      var int      longs              !  Long blanks in justified line.
      var int      remaining          !  Columns to be filled by justification.
      var ref word right              !  Rightmost WORD in a justified line.
      var int      shorts             !  Short blanks in a justified line.
      var int      unjustified        !  Columns in unjustified line.
      var int      width              !  Columns in a word, or in a blank.

!  NEXT. Destroy THIS, and return the next WORD in the queue that follows it.

      next :−
       (form (ref word this) ref word:
         toHeap(this↑.chars)
         this↑.next also toHeap(this))

!  This is DEQUEUE WORDS's body.

     do right := first.next
        emptyQueue()
        (while right ≠ nil
         do count := 0
            left := right
            unjustified := 0
            (while
             (if right = nil
              then false
              else if width := right↑.width + (count > 0)
                      unjustified + width ≤ justified − prefixWidth
                   then count += 1
                        unjustified += width
                        right := right↑.next
                        true
                   else false))
            write(temp, prefix)
            (if count = 0
             then write(temp, right↑.chars)
                  right := next(right)
             else if right = nil
                  then write(temp, left↑.chars)
                       left := next(left)
                       (while left ≠ right
                        do write(temp, ' ')
                           write(temp, left↑.chars)
                           left := next(left))
                  else blanks := count − 1
                       remaining := justified − unjustified − prefixWidth
                       width := 1 + remaining / blanks
                       longs := remaining mod blanks
                       shorts := blanks − longs
                       direction := ¬ direction
                       write(temp, left↑.chars)
                       left := next(left)
                       (if direction
                        then (while left ≠ right
                              do write(temp, ' ', width)
                                 (if longs > 0
                                  then write(temp, ' ')
                                       longs −= 1)
                                 write(temp, left↑.chars)
                                 left := next(left))
                        else (while left ≠ right
                              do write(temp, ' ', width)
                                 (if shorts > 0
                                  then shorts −= 1
                                  else write(temp, ' '))
                                 write(temp, left↑.chars)
                                 left := next(left))))
            writeln(temp))))

!  WRITE. Extend WRITE so it writes COUNT characters, or a BUF1, to OUTPUT.

  write :−
   (alt
    (form (stream output, char ch, int count) void:
     (in count
      do write(output, ch))),
    (form (stream output, var buf1 this) void:
     (for char ch in elements(this)
      do write(output, ch))))

!  WRITELN. Extend WRITELN so it writes a BUF1 followed by a newline to OUTPUT.

  writeln :−
   (form (stream output, var buf1 this) void:
     write(output, this)
     writeln(output))

!  BEGIN. Run the main program.

  begin :− main()
)
