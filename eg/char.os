!
!  EG/CHAR. Read characters and report errors.
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

(prog

!  Errors.

  foj makeError         :− enum()       !  Return a new error number.
  inj alreadyDefinedErr :− makeError()  !  Name already defined.
  inj arrowExpectedErr  :− makeError()  !  '=' or '→' expected.
  inj badCharErr        :− makeError()  !  Illegal character.
  inj badEscapeErr      :− makeError()  !  This character cannot follow '\'.
  inj badNumberErr      :− makeError()  !  Number out of range.
  inj badTokenErr       :− makeError()  !  Illegal symbol.
  inj charExpectedErr   :− makeError()  !  Character expected.
  inj closeExpectedErr  :− makeError()  !  ')' expected.
  inj dotExpectedErr    :− makeError()  !  '.' expected.
  inj endExpectedErr    :− makeError()  !  End of file expected.
  inj lineTooLongErr    :− makeError()  !  Source line is too long.
  inj nameExpectedErr   :− makeError()  !  Name expected.
  inj numberExpectedErr :− makeError()  !  Nonnegative integer expected.
  inj quoteExpectedErr  :− makeError()  !  '"' expected.
  inj rangeErr          :− makeError()  !  Number out of range.
  inj termExpectedErr   :− makeError()  !  Name or string expected.
  inj undefinedNameErr  :− makeError()  !  Name was never defined.
  inj maxErr            :− makeError()  !  Maximum error number.

!  Types.

  inj      maxLineLength :− 1024                         !  Max chars per line.
  type exe errSet        :− set(maxErr)                  !  Syntax error set.
  type exe lineBuffer    :− buffer(maxLineLength, char)  !  A line from SOURCE.

!  Constants.

  char   eog      :− '\0'              !  End Of Grammar char.
  char   eos      :− (past eos){char}  !  End Of Stream char.
  char   linefeed :− '\N'              !  ASCII linefeed char.
  char   return   :− '\R'              !  ASCII return char.
  errSet ∅        :− makeSet(maxErr:)  !  The empty set of errors.

!  Variables.

  var char       ch                !  Last char from SOURCE.
  var bool       hasErrs :− false  !  Have we seen syntax errs?
  var lineBuffer line              !  Char at each column.
  var stream     source            !  Read grammars from here.
  var int        sourceErrs        !  Errs left to report in SOURCE.
  var int        sourceLine        !  Counts line numbers from SOURCE.
  var int        sourcePass        !  Which pass are we doing?
  var string     sourcePath        !  Pathname of current SOURCE.

!  PASS. Wrapper. Open SOURCE, whose pathname is PATH. Initialize the character
!  reader and error reporter, then call BODY. Write error messages (if any) and
!  finally close SOURCE.

  pass :−
   (form (string path, int pass) foj:
    (form (form () obj body) obj:
     (if ¬ open(source, path, ''r'')
      then fail(''Cannot open '%s'.'': path))
     sourceErrs := maxErrs
     sourceLine := 0
     sourcePass := pass
     sourcePath := path
     nextLine()
     nextChar()
     body()
     writeMessages()
     (if ¬ close(source)
      then fail(''Cannot close '%s'.'': path))))

!  NEXT LINE. Read the next line from SOURCE into LINE.  If L is the ASCII line
!  feed char and R is the ASCII return char then a line may be terminated by L,
!  R, LR, or RL. The last line in SOURCE may be optionally terminated by an EOS
!  instead.

  nextLine :−
   (proc () void:
    (with

!  IS ILLEGAL CHAR. Test if CH is a control char, which isn't allowed to appear
!  in a source file.

      isIllegalChar :−
       (form (char ch) bool:
         '\#00' ≤ ch ≤ '\#1F' ∨
         ch = '\#7F'          ∨
         '\#80' ≤ ch ≤ '\#9F' ∨
         ch = '\#2028'        ∨
         ch = '\#2029')

!  NEXT END FILE. Read an EOG into LINE and stop reading.

      nextEndFile :−
       (form () bool:
        (if atStart(line)
         then append(line, eog))
        false)

!  NEXT LINEFEED. Read a blank into LINE and stop reading.

      nextLinefeed :−
       (form () bool:
         ch := read(source)
         (if ch ≠ eos ∧ ch ≠ return
          then unread(source, ch))
         append(line, ' ')
         false)

!  NEXT RETURN. Read a blank into LINE and stop reading.

      nextReturn :−
       (form () bool:
         ch := read(source)
         (if ch ≠ eos ∧ ch ≠ linefeed
          then unread(source, ch))
         append(line, ' ')
         false)

!  NEXT OTHER. Read an ordinary char into LINE and keep reading.

      nextOther :−
       (form () bool:
        (if isFull(line)
         then syntaxError(lineTooLongErr)
              remove(line, 1)
              append(line, ' ')
         else if isIllegalChar(ch)
              then syntaxError(badCharErr)
                   append(line, ' ')
              else append(line, ch))
        true)

!  This is NEXT LINE's body.

     do empty(line)
        sourceLine += 1
        (while
         (case ch := read(source); ch
          of eos:      nextEndFile()
             linefeed: nextLinefeed()
             return:   nextReturn()
             none:     nextOther()))))

!  NEXT CHAR. Get the next char CH, advancing LINE. If it's already advanced as
!  far as possible, then read a new line first.

  nextChar :−
   (proc () void:
    (if atEnd(line)
     then nextLine())
    ch := start(line)
    advance(line))

!  LINE ERR. Record that the errors in ERRS occurred at line number LINE in the
!  current SOURCE file. LINE ERRs are linked into a linear chain via their NEXT
!  slots, in increasing order of their LINE slots.

  lineErr :−
   (tuple
     int             line,
     var errSet      errs,
     var ref lineErr next)

!  MAKE LINE ERR. Return a LINE ERR that asserts no errors occurred at LINE.

  makeLineErr :−
   (proc (int line) ref lineErr:
    (with ref var lineErr this :− fromHeap(var lineErr)
     do this↑.line := line
        this↑.errs := ∅
        this↑.next := nil
        this{ref lineErr}))

!  SYNTAX ERROR. Assert that ERR occurred at line number SOURCE LINE of SOURCE.
!  FIRST is the dummy head node in a linear chain of LINE ERRs, and LAST is the
!  last such node. We assert errors only during Pass 2.

  ref lineErr     first :− makeLineErr(0)
  var ref lineErr last  :− first

  syntaxError :−
   (proc (int err) void:
    (if sourcePass = 2 ∧ sourceErrs > 0
     then sourceErrs −= 1
          (if last↑.line ≠ sourceLine
           then (with ref lineErr this :− makeLineErr(sourceLine)
                 do last↑.next := this
                    last := this))
          last↑.errs ∪= err
          hasErrs := true))

!  WRITE MESSAGES. Write all error messages in the chain of LINE ERRs, starting
!  after FIRST.

  writeMessages :−
   (proc () void:
    (with var ref lineErr this :− first↑.next

!  MESSAGE. Return the message string that corresponds to ERR.

      message :−
       (form (inj err) string:
        (case err
         of alreadyDefinedErr: ''Name already defined''
             arrowExpectedErr: '''=' or '→' expected''
                   badCharErr: ''Illegal character''
                 badEscapeErr: ''This character cannot follow '\\'''
                 badNumberErr: ''Number out of range''
                  badTokenErr: ''Illegal symbol''
              charExpectedErr: ''Character expected''
             closeExpectedErr: ''')' expected''
               dotExpectedErr: '''.' expected''
               endExpectedErr: ''End of file expected''
               lineTooLongErr: ''Source line is too long''
              nameExpectedErr: ''Name expected''
            numberExpectedErr: ''Nonnegative integer expected''
             quoteExpectedErr: '''"' expected''
                     rangeErr: ''Number out of range''
              termExpectedErr: ''Name or string expected''
             undefinedNameErr: ''Name was never defined''
                         none: ϵ))

!  This is WRITE MESSAGES's body. Visit every LINE ERR, write its messages, and
!  send the LINE ERR back to the heap. Finally restore FIRST and LAST.

     do (while this ≠ nil
         do (for inj err in elements(this↑.errs)
             do writeln(''%s:%i: %s.'': sourcePath, this↑.line, message(err)))
            this := (this↑.next also toHeap(this)))
        first↑.next := nil
        last := first))
)
