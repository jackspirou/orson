!
!  APPS/NSURT. Copy a file, inserting the contents of other files.
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

!  NSURT (pronounced as "insert") copies one or more text files to OUTPUT. Each
!  text file can contain symbolic pointers to other text files. Each time NSURT
!  encounters a pointer, it copies the referenced file, then returns to copying
!  the original file. The NSURT command looks like this.
!
!    nsurt -l L -p σ p₁ p₂ ... pⱼ
!
!  NSURT follows pointers at most L deep: this keeps it from being caught in an
!  infinite loop if a file points to itself either directly or indirectly. Each
!  symbolic pointer starts with the nonempty prefix σ, followed by the absolute
!  pathname of the file it references. The subscripted p's are pathnames of the
!  files to be read. If no pathnames are given, then NSURT does nothing.

(load ''lib.buffer'')   !  Fixed length linear queues.
(load ''lib.command'')  !  Process Unix command line arguments.
(load ''lib.convert'')  !  Convert a string to an integer or a real.
(load ''lib.fail'')     !  Terminate a program with an error message.
(load ''lib.file'')     !  Input and output on file streams.
(load ''lib.string'')   !  Operations on strings.
(load ''lib.text'')     !  Read characters and lines from a text file.

(prog
  var int    maxLevel      :− 4      !  Max number of nested files.
  inj        maxLineLength :− 1024   !  Max line length.
  var string prefix        :− ''%''  !  Prefix indicating a pathname.

!  FILE OPTION. Copy lines to OUTPUT from a file with pathname PATH. If we find
!  a line starting with PREFIX, then get a pathname from the line and copy from
!  the file with that pathname instead. When we've copied its lines, go back to
!  copying lines from the original file. We can go MAX LEVEL files deep.

  fileOption :−
   (proc (int level, string path) void:
    (if level > maxLevel
     then fail(''Files nested more than %i levels deep.'': maxLevel)
     else (for bool ok, stream input in opened(path, ''r'')
           do (if ok
               then (for var buffer(maxLineLength) line
                     in lines(input, maxLineLength)
                     do (if isStart(prefix, line{string})
                         then advance(line, length(prefix))
                              (while start(line) = ' '
                               do advance(line))
                              fileOption(level + 1, line{string})
                         else writeln(line{string})))
               else fail(''Cannot open "%s".'': path)))))

!  LEVEL OPTION. Set MAX LEVEL to a non negative integer encoded in VALUE.

  levelOption :−
   (form (string value) void:
    (for bool ok, int level in convert(int, value)
     do (if ok ∧ level ≥ 0
         then maxLevel := level
         else fail(''Expected non negative integer for -l.''))))

!  PREFIX OPTION. Set PREFIX to a nonempty string VALUE.

  prefixOption :−
   (form (string value) void:
    (with string value :− (past value)
     do (if isEmpty(value)
         then fail(''Expected nonempty string for -p.'')
         else prefix := value)))

!  MAIN. Visit command line options and do what they say.
                         
  main :−
   (for char option, string value in command(ϵ, '' lp'')
    do (case option
        of ' ': fileOption(0, value)
           'l': levelOption(value)
           'p': prefixOption(value)))
)
