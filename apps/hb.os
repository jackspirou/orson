!
!  APPS/HB. Write simple statistics about source files.
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

!  HB (How Big) reads source files and writes statistics about how big they are
!  to OUTPUT. For each file it writes the number of blank lines, comment lines,
!  code lines, total lines, length of shortest nonblank line, length of longest
!  nonblank line, and total pages. It ignores directories. The HB command looks
!  like this.
!
!    hb -c σ p₁ p₂ ... pⱼ
!
!  Comment lines begin with the string σ, which defaults to "!" (Orson comments
!  begin with this). The subscripted p's are pathnames of the files to be read.
!  If no pathnames are given, then HB does nothing. HB demonstrates BUFFERs and
!  iterators, among other things.

(load ''lib.command'')    !  Process command line arguments.
(load ''lib.directory'')  !  Operations on Unix directories.
(load ''lib.dynamic'')    !  Dynamic memory allocation with explicit release.
(load ''lib.fail'')       !  Terminate a program with an error message.
(load ''lib.file'')       !  Input and output on Unix file streams.
(load ''lib.text'')       !  Read characters and lines from a text file.
(load ''lib.width'')      !  Determine columns needed to write simple objects.

(prog
  int       ∞            :− high(int)           !  Largest possible INT.
  string    comment      :− ''!''               !  Default comment prefix.
  inj       lineLength   :− 1024                !  Max CHAR0's per line.
  inj       linesPerPage :− 60                  !  Lines per page. Duh.
  type exe  chars        :− buffer(lineLength)  !  A line or a string.
  var chars prefix                              !  It holds COMMENT.

!  FILE. Hold information about a file whose pathname is PATH. FILEs are linked
!  into a linear chain through their NEXT slots.

  file :−
   (tuple
     var string   path,     !  File pathname.
     var int      blank,    !  Blank lines.
     var int      code,     !  Code lines.
     var int      comment,  !  Comment lines.
     var int      line,     !  Lines.
     var int      long,     !  Columns in longest nonblank line.
     var int      page,     !  Pages.
     var int      short,    !  Columns in shortest nonblank line.
     var ref file next)     !  Next FILE in the chain.

!  MAIN. Main program. (1) Collect statistics about files whose paths appear on
!  the command line. (2) Calculate how to format the statistics. (3) Write them
!  to OUTPUT.

  main :−
   (form () void:
    (with
      var int blankTotal   :− 0  !  Total blank   lines in all files.
      var int codeTotal    :− 0  !  Total code    lines in all files.
      var int commentTotal :− 0  !  Total comment lines in all files.
      var int lineTotal    :− 0  !  Total         lines in all files.
      var int pageTotal    :− 0  !  Total pages         in all files.
      var int pathWidth    :− 0  !  Maximum path width  of all files.
      var int maxLong      :− 0  !  Max line length     in all files.
      var int maxShort     :− 0  !  Min line length     in all files.
     do init(prefix)
        append(prefix, comment)
        (for char option, string value in command(ϵ, '' c'')
         do (case option
             of 'c': (init(prefix); append(prefix, value))
                ' ': readPath(value)))
        (for
          string path, int blank, int code, int comment, int line, int long,
          int short, int page
         in files()
         do pathWidth    := max(pathWidth, width(path))
            blankTotal   += blank
            codeTotal    += code
            commentTotal += comment
            lineTotal    += line
            maxLong      := max(long, maxLong)
            maxShort     := max(short, maxShort)
            pageTotal    += page)
        (for
          string path, int blank, int code, int comment, int line, int long,
          int short, int page
         in files()
         do write(pathWidth,           path)
            write(width(blankTotal),   blank,   ''blank'')
            write(width(codeTotal),    code,    ''code'')
            write(width(commentTotal), comment, ''comment'')
            write(width(lineTotal),    line,    ''line'')
            write(width(maxShort),     short,   ''min'')
            write(width(maxLong),      long,    ''max'')
            write(width(pageTotal),    page,    ''page'')
            writeln())
        (if twoOrMore()
         then write(pathWidth,           ϵ)
              write(width(blankTotal),   blankTotal,   ''blank'')
              write(width(codeTotal),    codeTotal,    ''code'')
              write(width(commentTotal), commentTotal, ''comment'')
              write(width(lineTotal),    lineTotal,    ''line'')
              write(width(maxShort),     maxShort,     ''min'')
              write(width(maxLong),      maxLong,      ''max'')
              write(width(pageTotal),    pageTotal,    ''page'')
              writeln())))

!  FIRST. The head node of a linear linked chain of FILEs. They are sorted into
!  nondecreasing order by their PATH slots.

  ref file first :−
   (with var ref file self :− fromHeap(file)
    do self↑.path := ϵ
       self↑.next := nil
       self)

!  TWO OR MORE. Test if the chain starting with FIRST has two or more files.

  twoOrMore :−
   (form () bool:
     first↑.next ≠ nil ∧ first↑.next↑.next ≠ nil)

!  FILES. Iterator. Visit files in the chain starting with FIRST, and call BODY
!  on their slots.

  files :−
   (form () foj:
    (form (form (string, int, int, int, int, int, int, int) obj body) obj:
     (with var ref file next :− first↑.next
      do (while next ≠ nil
          do (with
               string path    :− next↑.path
               int    blank   :− next↑.blank
               int    code    :− next↑.code
               int    comment :− next↑.comment
               int    line    :− next↑.line
               int    long    :− next↑.long
               int    page    :− next↑.page
               int    short   :− next↑.short
              do body(path, blank, code, comment, line, long, short, page)
                 next := next↑.next)))))

!  READ PATH. If a pathame PATH doesn't specify a directory, then read the file
!  it specifies, and make a FILE that holds information about the file.

  readPath :−
   (form (string path) void:
    (if ¬ isDirectory(path)
     then (with
            var int blank   :− 0
            var int code    :− 0
            var int comment :− 0
            var int long    :− 0
            string  path    :− (past path)
            var int short   :− ∞
            values          :− open(path, ''r'')
           do (if values.success
               then (for var chars line in lines(values.stream, lineLength)
                     do (if isBlank(line)
                         then blank += 1
                         else (with int temp :− count(line)
                               do (if temp > long
                                   then long := temp)
                                  (if temp < short
                                   then short := temp)
                                  (if isComment(prefix, line)
                                   then comment += 1
                                   else code += 1))))
                    recordFile(path, blank, code, comment, long, short)
                    (if ¬ close(values.stream)
                     then fail(''cannot close "%s"'': path))
               else fail(''cannot open "%s"'': path)))))

!  COUNT. Return the number of CHARs in LINE.

  count :−
   (form (var chars line) int:
     restart(line)
     count(line{string}))

!  IS BLANK. Test if LINE contains only blanks.

  isBlank :−
   (form (var chars line) bool:
     restart(line)
     skipBlanks(line)
     atEnd(line))

!  IS COMMENT. Test if LINE starts with zero or more blanks followed by PREFIX.

  isComment :−
   (form (var chars prefix, var chars line) bool:
     restart(prefix)
     skipBlanks(line)
     (while ¬ atEnd(prefix) ∧ ¬ atEnd(line) ∧ start(prefix) = start(line)
      do advance(prefix)
         advance(line))
     atEnd(prefix))

!  SKIP BLANKS. Reset LINE so it starts with its first nonblank character.

  skipBlanks :−
   (form (var chars line) void:
     restart(line)
     (while ¬ atEnd(line) ∧ start(line) = ' '
      do advance(line)))

!  RECORD FILE. Add a new file with PATH, BLANK, CODE, COMMENT, LONG, and SHORT
!  to the chain starting at FIRST. We compute LINE and PAGE from these.

  recordFile :−
   (with type inj i :− int
    do (form (string path, i blank, i code, i comment, i long, i short) void:
        (with
          var ref file last :− first
          ref file     self :− fromHeap(file)
         do self↑.path    := copy(path)
            self↑.blank   := blank
            self↑.code    := code
            self↑.comment := comment
            self↑.line    := self↑.blank + self↑.code + self↑.comment
            self↑.long    := long
            self↑.page    := pages(self↑.line)
            self↑.short   := short
            (while
             (if last↑.next = nil
              then last↑.next := self
                   false
              else if self↑.path > last↑.path
                   then last := last↑.next
                        true
                   else self↑.next := last↑.next
                        last↑.next := self
                        false)))))

!  PAGES. The number of pages needed for a file of LINE lines. Maybe there's a
!  partial page at the end.

  pages :−
   (form (int line) int:
     line / linesPerPage + (line mod linesPerPage > 0))

!  WRITE. Write the string PATH in COLUMNS columns. Write COUNT number of WHATs
!  in COLUMNS columns.

  write :−
   (alt
    (form (int columns, string path) void:
      (in columns − width(path)
       do write(' '))
      write(path)
      write(' ')),
    (form (int columns, int count, string what) void:
      write(''%*i %s '': columns, count, what)))

!  BEGIN. Begin execution here.

  begin :− main()
)
