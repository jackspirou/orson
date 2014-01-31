!
!  EG/TOKEN. Read tokens.
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

!  Tokens.

  foj makeToken       :− enum()       !  Return a new token number.
  inj arrowToken      :− makeToken()  !  Token for '→' or '='.
  inj barToken        :− makeToken()  !  Token for '|'.
  inj closeParenToken :− makeToken()  !  Token for ')'.
  inj commaToken      :− makeToken()  !  Token for ','.
  inj dotToken        :− makeToken()  !  Token for '.'.
  inj endToken        :− makeToken()  !  Token for the end of a grammar.
  inj nameToken       :− makeToken()  !  Token for a nonterminal.
  inj numberToken     :− makeToken()  !  Token for a number in a RANGE.
  inj openParenToken  :− makeToken()  !  Token for '('.
  inj otherToken      :− makeToken()  !  An irrelevant token (Pass One only).
  inj plusToken       :− makeToken()  !  Token for '+'.
  inj questionToken   :− makeToken()  !  Token for '?'.
  inj starToken       :− makeToken()  !  Token for '*' or '∗'.
  inj stringToken     :− makeToken()  !  Token for a terminal.
  inj maxToken        :− makeToken()  !  Maximum token number.

!  Constants.

  cha "\" :− '\\'  !  A better-looking backslash char.

!  Variables.

  var int                   token        !  Current token.
  var buffer(maxLineLength) tokenChars   !  CHAR0s making up TOKEN.
  var ref codon             tokenName    !  TOKEN CHARS as a NAME CODON.
  var int                   tokenNumber  !  TOKEN CHARS as an INT.

!  NEXT 1 TOKEN. Advance to the next Pass One token from SOURCE. We really care
!  only about NAME TOKENs and ARROW TOKENs here: everything else gets turned to
!  OTHER TOKENs.

  next1Token :−
   (proc () void:
    (with

!  NEXT ARROW. Read an ARROW TOKEN and stop reading.

      nextArrow :−
       (form () bool:
         token := arrowToken
         nextChar()
         false)

!  NEXT BLANK. Skip the current char and keep reading.

      nextBlank :−
       (form () bool:
         nextChar()
         true)

!  NEXT COMMENT. Skip the rest of the current line and keep reading.

      nextComment :−
       (form () bool:
        (while ¬ atEnd(line)
         do nextChar())
        true)

!  NEXT END. Read an END token and stop reading.

      nextEnd :−
       (form () bool:
         token := endToken
         false)

!  NEXT NAME. Read a NAME TOKEN and stop reading. TOKEN CHARS holds the token's
!  chars. TOKEN NAME is the NAME token as a CODON.

      nextName :−
       (proc () bool:
         token := nameToken
         empty(tokenChars)
         (while
           append(tokenChars, ch)
           nextChar()
           isLetter(ch) ∨ isDigit(ch))
         tokenName := makeName(tokenChars{string})
         false)

!  NEXT OTHER. Read a token that's irrelevant to Pass One, and stop reading.

      nextOther :−
       (form () bool:
         token := otherToken
         nextChar()
         false)

!  NEXT STRING. Read a STRING TOKEN, discarding its chars, and stop reading.

      nextString :−
       (form () bool:
         token := otherToken
         nextChar()
         (while
          (if ch = '"'
           then false
           else if ch = "\"
                then nextChar()
                     (if atEnd(line)
                      then false
                      else nextChar()
                           true)
                else if atEnd(line)
                     then false
                     else nextChar()
                          true))
         true)

!  This is NEXT 1 TOKEN's body. We dispatch on the first char of every token to
!  a method call that reads the token. The call returns TRUE if we should go on
!  reading another token, and FALSE if we should stop with this one.

     do (while
         (case ch
          of eog: nextEnd()
             ' ': nextBlank()
             '!': nextComment()
             '"': nextString()
             '=': nextArrow()
             'A': nextName()
             'B': nextName()
             'C': nextName()
             'D': nextName()
             'E': nextName()
             'F': nextName()
             'G': nextName()
             'H': nextName()
             'I': nextName()
             'J': nextName()
             'K': nextName()
             'L': nextName()
             'M': nextName()
             'N': nextName()
             'O': nextName()
             'P': nextName()
             'Q': nextName()
             'R': nextName()
             'S': nextName()
             'T': nextName()
             'U': nextName()
             'V': nextName()
             'W': nextName()
             'X': nextName()
             'Y': nextName()
             'Z': nextName()
             'a': nextName()
             'b': nextName()
             'c': nextName()
             'd': nextName()
             'e': nextName()
             'f': nextName()
             'g': nextName()
             'h': nextName()
             'i': nextName()
             'j': nextName()
             'k': nextName()
             'l': nextName()
             'm': nextName()
             'n': nextName()
             'o': nextName()
             'p': nextName()
             'q': nextName()
             'r': nextName()
             's': nextName()
             't': nextName()
             'u': nextName()
             'v': nextName()
             'w': nextName()
             'x': nextName()
             'y': nextName()
             'z': nextName()
             '→': nextArrow()  !  U+2192 rightwards arrow.
            none: nextOther()))))

!  NEXT 2 TOKEN. Advance to the next Pass Two token from SOURCE. If the current
!  TOKEN is EXPECTED, then advance to the next, otherwise assert ERR and do not
!  advance.

  next2Token :−
   (alt
    (form () void:
      Next2Token()),
    (form (int expected, int err) void:
     (if token = expected
      then Next2Token()
      else syntaxError(err))))

!  The procedure does all the work for the form.

  Next2Token :−
   (proc () void:
    (with

!  NEXT BLANK. Skip the current char and keep reading.

      nextBlank :−
       (form () bool:
         nextChar()
         true)

!  NEXT COMMENT. Skip the rest of the current line and keep reading.

      nextComment :−
       (form () bool:
        (while ¬ atEnd(line)
         do nextChar())
        true)

!  NEXT END. Read an END token and stop reading.

      nextEnd :−
       (form () bool:
         token := endToken
         false)

!  NEXT INFINITY. Read '∞', treat it as a NUMBER token, and stop reading.

      nextInfinity :−
       (form () bool:
         token := numberToken
         tokenNumber := maxRepeat
         nextChar()
         false)

!  NEXT NAME. Read a NAME TOKEN and stop reading. TOKEN CHARS holds the token's
!  chars. TOKEN NAME is the NAME TOKEN as a CODON.

      nextName :−
       (proc () bool:
         token := nameToken
         empty(tokenChars)
         (while
           append(tokenChars, ch)
           nextChar()
           isLetter(ch) ∨ isDigit(ch))
         tokenName := makeName(tokenChars{string})
         false)

!  NEXT NUMBER. Read a NUMBER token and stop reading. TOKEN NUMBER is the token
!  as a nonnegative INT.

      nextNumber :−
       (proc () bool:
         token := numberToken
         empty(tokenChars)
         (while
           append(tokenChars, ch)
           nextChar()
           isDigit(ch))
         (for bool ok, int number in convert(int, tokenChars{string})
          do (if ok
              then tokenNumber := number
              else syntaxError(badNumberErr)))
         false)

!  NEXT SINGLE. Read a single char as a TOKEN' and stop reading.

      nextSingle :−
       (form (int token') bool:
         token := token'
         nextChar()
         false)

!  NEXT STRING. Read a STRING token, and stop reading. TOKEN CHARS contains the
!  token's characters.

      nextString :−
       (form () bool:
        (with

!  NEXT BACKSLASH. Read a special character denoted by a prefix backslash.

          nextBackslash :−
           (form () void:
             nextChar()
             (case ch
              of '"': nextSlashed('"')
                 '#': nextSharped()
                 'A': nextSlashed('\A')
                 'B': nextSlashed('\B')
                 'F': nextSlashed('\F')
                 'N': nextSlashed('\N')
                 'R': nextSlashed('\R')
                 'T': nextSlashed('\T')
                 'V': nextSlashed('\V')
                 "\": nextSlashed("\")
                 'a': nextSlashed('\A')
                 'b': nextSlashed('\B')
                 'f': nextSlashed('\F')
                 'n': nextSlashed('\N')
                 'r': nextSlashed('\R')
                 't': nextSlashed('\T')
                 'v': nextSlashed('\V')
                none: syntaxError(badEscapeErr)))

!  NEXT SLASHED. Add CH to TOKEN CHARS.

          nextSlashed :−
           (proc (char0 ch) void:
             append(tokenChars, ch)
             nextChar())

!  NEXT SHARPED. Read a series of hexadecimal digits into HEXITS and convert it
!  to a character code. Add the character with that code to TOKEN CHARS.

          nextSharped :−
           (form () void:
            (with var buffer(maxLineLength) hexits
             do nextChar()
                empty(hexits)
                (while
                  append(hexits, ch)
                  nextChar()
                  isDigit(ch, 16))
                (for bool ok, int code in convert(int, hexits{string}, 16)
                 do (if ok ∧ code ≥ 0
                     then append(hexits, code{char})
                     else syntaxError(badEscapeErr)))))

!  This is NEXT STRING's body.

         do token := stringToken
            empty(tokenChars)
            nextChar()
            (while
             (if ch = '"'
              then nextChar()
                   false
              else if ch = "\"
                   then nextBackslash()
                        true
                   else if atEnd(line)
                        then syntaxError(quoteExpectedErr)
                             false
                        else append(tokenChars, ch)
                             nextChar()
                             true))
            false))

!  NEXT UNKNOWN. Read a character that doesn't correspond to a token, then keep
!  reading.

      nextUnknown :−
       (form () bool:
         syntaxError(badTokenErr)
         nextChar()
         true)

!  This is NEXT 2 TOKEN's body. We dispatch on the first char of every token to
!  a method call that reads the token. The call returns TRUE if we should go on
!  reading another token, and FALSE if we should stop with this one.

     do (while
         (case ch
          of eog: nextEnd()
             ' ': nextBlank()
             '!': nextComment()
             '"': nextString()
             '(': nextSingle(openParenToken)
             ')': nextSingle(closeParenToken)
             '*': nextSingle(starToken)
             '+': nextSingle(plusToken)
             ',': nextSingle(commaToken)
             '.': nextSingle(dotToken)
             '0': nextNumber()
             '1': nextNumber()
             '2': nextNumber()
             '3': nextNumber()
             '4': nextNumber()
             '5': nextNumber()
             '6': nextNumber()
             '7': nextNumber()
             '8': nextNumber()
             '9': nextNumber()
             '=': nextSingle(arrowToken)
             '?': nextSingle(questionToken)
             'A': nextName()
             'B': nextName()
             'C': nextName()
             'D': nextName()
             'E': nextName()
             'F': nextName()
             'G': nextName()
             'H': nextName()
             'I': nextName()
             'J': nextName()
             'K': nextName()
             'L': nextName()
             'M': nextName()
             'N': nextName()
             'O': nextName()
             'P': nextName()
             'Q': nextName()
             'R': nextName()
             'S': nextName()
             'T': nextName()
             'U': nextName()
             'V': nextName()
             'W': nextName()
             'X': nextName()
             'Y': nextName()
             'Z': nextName()
             'a': nextName()
             'b': nextName()
             'c': nextName()
             'd': nextName()
             'e': nextName()
             'f': nextName()
             'g': nextName()
             'h': nextName()
             'i': nextName()
             'j': nextName()
             'k': nextName()
             'l': nextName()
             'm': nextName()
             'n': nextName()
             'o': nextName()
             'p': nextName()
             'q': nextName()
             'r': nextName()
             's': nextName()
             't': nextName()
             'u': nextName()
             'v': nextName()
             'w': nextName()
             'x': nextName()
             'y': nextName()
             'z': nextName()
             '|': nextSingle(barToken)
             '→': nextSingle(arrowToken)  !  U+2192 rightwards arrow.
             '∗': nextSingle(starToken)   !  U+2217 asterisk operator.
             '∞': nextInfinity()          !  U+221E infinity.
            none: nextUnknown()))))
)
