!
!  APPS/PRESCRIPT. Preprocess Orson source files for GNU Enscript.
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

!  Orson source files may contain Unicode characters that are outside the usual
!  ASCII set, but ENSCRIPT can't print them. PRESCRIPT preprocesses such files,
!  so that many (but not all!) of these characters can be printed. Suppose that
!  φ is a path to an Orson source file. Then you can print the file like this.
!
!    prescript φ | enscript -2rG -e -f Courier6.75
!
!  Read "man enscript" for more than you want to know about ENSCRIPT's options.
!  If no paths are given to PRESCRIPT then it reads an Orson source from INPUT.
!  For example, you can print a cross reference listing of several source files
!  like this, using the Orson cross referencer OX.
!
!    ox φ₁ φ₂ ... φⱼ | prescript | enscript -2rG -e -f Courier6.75
!
!  PRESCRIPT writes messages to ERRPUT about characters that it can't print. To
!  see just the messages about a series of source files, you can do this.
!
!    prescript φ₁ φ₂ ... φⱼ > /dev/null
!
!  PRESCRIPT is a kludge, intended as a temporary stopgap until a better way to
!  print Unicode files comes along.

(load ''lib.dynamic'')  !  Dynamic memory allocation with explicit release.
(load ''lib.command'')  !  Process command line arguments.
(load ''lib.fail'')     !  Terminate a program with an error message.
(load ''lib.text'')     !  Read characters and lines from a text file.

(prog

!  Constants. In many cases we represent a char as an octal code. Hexdecimal is
!  more convenient, but the PostScript documentation uses octal.

  real   ap        :− 4.75       !  Point size for some arrows.
  char   backspace :− '\B'       !  A backspace. Duh.
  real   cp        :− 6.75       !  Point size for Courier chars.
  real   gp        :− 5.75       !  Point size for Greek letter chars.
  int    lower     :− 2          !  Points to lower a subscript char.
  string me        :− argv()[0]  !  Name of this program.
  int    mystery   :− 8#250      !  Unprintable char (in Symbol font).
  char   nul       :− '\0'       !  ENSCRIPT's escape char.
  real   op        :− 4.75       !  Point size for some Symbol operators.
  real   sg        :− 0.25       !  Shade of gray for some Symbol chars.
  real   sp        :− 6.75       !  Point size for Symbol chars.       
  real   ssp       :− cp − 2.0   !  Point size for sub/superscript chars.
  int    upper     :− 2          !  Points to raise a superscript char.

!  NODE. A node in an unbalanced BST of missing chars.  CH is the missing char,
!  LEFT is a subtree whose chars are less than CH, and RIGHT is a subtree whose
!  chars are greater than CH.

  node :−
   (tuple
     char ch,
     var ref node left,
     var ref node right)

!  MAKE NODE. Make a new node for the BST of missing chars.

  makeNode :−
   (proc (char ch) ref node:
    (with ref var node self :− fromHeap(var node)
     do self↑.ch    := ch
        self↑.left  := nil
        self↑.right := nil
        self{ref node}))

!  ROOT. The root of the BST of missing chars. It's a dummy head node.

  ref node root :− makeNode('x')

!  MISSED. Scold the user about chars recorded in the BST of missing chars.

  missed :−
   (form () obj:
    (with

!  MISSED'ING. Do all the work for MISSED.

      missed'ing :−
       (proc (ref node subtree) void:
        (with var ref node subtree :− (past subtree)
         do (while subtree ≠ nil
             do missed'ing(subtree↑.left)
                write(errput, ''%s: Can't write U+%04X "'': me, subtree↑.ch)
                write(errput, subtree↑.ch)
                writeln(errput, ''".'')
                subtree := subtree↑.right)))

!  This is MISSED's body. We do an inorder traversal of ROOT, skipping its head
!  node.

     do missed'ing(root↑.left)
        missed'ing(root↑.right)))

!  MAIN. Main program. If there are no arguments on the command line, then read
!  INPUT. Otherwise read from file paths given as arguments.

  main :−
   (form () void:
    (if argc() = 1
     then transduce(input)
     else (for char, string path in command(ϵ, '' '')
           do (with var stream source
               do (if open(source, path, ''r'')
                   then transduce(source)
                        (if ¬ close(source)
                         then fail(''Cannot close "%s".'': path))
                   else fail(''Cannot open "%s".'': path)))))
    missed())

!  TRANSDUCE. Copy SOURCE to OUTPUT. Substitute fragments of PostScript for non
!  ASCII chars as we go.

  transduce :−
   (proc (stream source) void:
    (for char ch in chars(source)
     do (case hb(ch)
         of #00: transduceLatin(ch)
            #03: transduceGreek(ch)
            #20: transduceSubSup(ch)
            #21: transduceArrow(ch)
            #22: transduceSmallMath(ch)
            #23: transduceBigMath(ch)
            #26: transduceSuit(ch)
            #1D: transduceSub₀(ch)
            #2C: transduceSub₁(ch)
           none: missing(ch))))

!  HB. Return bytes 3 through 1 of CH.

  hb :−
   (form (char ch) int:
     ch → 8)

!  LB. Return byte 0 of CH.

  lb :−
   (form (char ch) int:
     #FF & ch)

!  TRANSDUCE LATIN. Write the Latin char CH to OUTPUT. We can handle chars from
!  ISO-8859 here.

  transduceLatin :−
   (form (char ch) void:
    (case lb(ch)
     of #3C: symbol(8#074)  !  < Less-than sign.
        #3E: symbol(8#076)  !  > Greater-than sign.
        #A1: latin(8#241)   !  ¡ Inverted exclamation mark.
        #A2: latin(8#242)   !  ¢ Cent sign.
        #A3: latin(8#243)   !  £ Pound sign.
        #A4: latin(8#244)   !  ¤ Currency sign.
        #A5: latin(8#245)   !  ¥ Yen sign.
        #A6: latin(8#246)   !  ¦ Broken bar.
        #A7: latin(8#247)   !  § Section sign.
        #A8: latin(8#250)   !  ¨ Diaeresis.
        #A9: latin(8#251)   !  © Copyright sign.
        #AA: latin(8#252)   !  ª Feminine ordinal indicator.
        #AB: latin(8#253)   !  « Left-pointing double angle quotation mark.
        #AC: latin(8#254)   !  ¬ Not sign.
        #AD: missing(ch)    !    Soft hyphen.
        #AE: latin(8#256)   !  ® Registered sign.
        #AF: latin(8#257)   !  ¯ Macron.
        #B0: latin(8#260)   !  ° Degree sign.
        #B1: latin(8#261)   !  ± Plus-minus sign.
        #B2: super('2')     !  ² Superscript two.
        #B3: super('3')     !  ³ Superscript three.
        #B4: latin(8#264)   !  ´ Acute accent.
        #B5: latin(8#265)   !  µ Micro sign.
        #B6: latin(8#266)   !  ¶ Pilcrow sign.
        #B7: latin(8#267)   !  · Middle dot.
        #B8: latin(8#270)   !  ¸ Cedilla.
        #B9: super('1')     !  ¹ Superscript one.
        #BA: latin(8#272)   !  º Masculine ordinal indicator.
        #BB: latin(8#273)   !  » Right-pointing double angle quotation mark.
        #BC: latin(8#274)   !  ¼ Vulgar fraction one quarter.
        #BD: latin(8#275)   !  ½ Vulgar fraction one half.
        #BE: latin(8#276)   !  ¾ Vulgar fraction three quarters.
        #BF: latin(8#277)   !  ¿ Inverted question mark.
        #C0: latin(8#300)   !  À Capital letter A with grave.
        #C1: latin(8#301)   !  Á Capital letter A with acute.
        #C2: latin(8#302)   !  Â Capital letter A with circumflex.
        #C3: latin(8#303)   !  Ã Capital letter A with tilde.
        #C4: latin(8#304)   !  Ä Capital letter A with diaeresis.
        #C5: latin(8#305)   !  Å Capital letter A with ring above.
        #C6: latin(8#306)   !  Æ Latin capital letter AE.
        #C7: latin(8#307)   !  Ç Latin capital letter C with cedilla.
        #C8: latin(8#310)   !  È Latin capital letter E with grave.
        #C9: latin(8#311)   !  É Latin capital letter E with acute.
        #CA: latin(8#312)   !  Ê Latin capital letter E with circumflex.
        #CB: latin(8#313)   !  Ë Latin capital letter E with diaeresis.
        #CC: latin(8#314)   !  Ì Latin capital letter I with grave.
        #CD: latin(8#315)   !  Í Latin capital letter I with acute.
        #CE: latin(8#316)   !  Î Latin capital letter I with circumflex.
        #CF: latin(8#317)   !  Ï Latin capital letter I with diaeresis.
        #D0: latin(8#320)   !  Ð Latin capital letter Eth.
        #D1: latin(8#321)   !  Ñ Latin capital letter N with tilde.
        #D2: latin(8#322)   !  Ò Latin capital letter O with grave.
        #D3: latin(8#323)   !  Ó Latin capital letter O with acute.
        #D4: latin(8#324)   !  Ô Latin capital letter O with circumflex.
        #D5: latin(8#325)   !  Õ Latin capital letter O with tilde.
        #D6: latin(8#326)   !  Ö Latin capital letter O with diaeresis.
        #D7: latin(8#327)   !  × Multiplication sign.
        #D8: latin(8#330)   !  Ø Latin capital letter O with stroke.
        #D9: latin(8#331)   !  Ù Latin capital letter I with grave.
        #DA: latin(8#332)   !  Ú Latin capital letter U with acute.
        #DB: latin(8#333)   !  Û Latin capital letter U with circumflex.
        #DC: latin(8#334)   !  Ü Latin capital letter U with diaeresis.
        #DD: latin(8#335)   !  Ý Latin capital letter Y with acute.
        #DE: latin(8#336)   !  Þ Latin capital letter Thorn.
        #DF: latin(8#337)   !  ß Latin small letter sharp S.
        #E0: latin(8#340)   !  à Latin small letter A with grave.
        #E1: latin(8#341)   !  á Latin small letter A with acute.
        #E2: latin(8#342)   !  â Latin small letter A with circumflex.
        #E3: latin(8#343)   !  ã Latin small letter A with tilde.
        #E4: latin(8#344)   !  ä Latin small letter A with diaeresis.
        #E5: latin(8#345)   !  å Latin small letter a with ring above.
        #E6: latin(8#346)   !  æ Latin small letter AE.
        #E7: latin(8#347)   !  ç Latin small letter C with cedilla.
        #E8: latin(8#350)   !  è Latin small letter E with grave.
        #E9: latin(8#351)   !  é Latin small letter E with acute.
        #EA: latin(8#352)   !  ê Latin small letter E with circumflex.
        #EB: latin(8#353)   !  ë Latin small letter E with diaeresis.
        #EC: latin(8#354)   !  ì Latin small letter I with grave.
        #ED: latin(8#355)   !  í Latin small letter I with acute.
        #EE: latin(8#356)   !  î Latin small letter I with circumflex.
        #EF: latin(8#357)   !  ï Latin small letter I with diaeresis.
        #F0: latin(8#360)   !  ð Latin small letter Eth.
        #F1: latin(8#361)   !  ñ Latin small letter N with tilde.
        #F2: latin(8#362)   !  ò Latin small letter O with grave.
        #F3: latin(8#363)   !  ó Latin small letter O with acute.
        #F4: latin(8#364)   !  ô Latin small letter O with circumflex.
        #F5: latin(8#365)   !  õ Latin small letter O with tilde.
        #F6: latin(8#366)   !  ö Latin small letter O with diaeresis.
        #F7: latin(8#367)   !  ÷ Division sign.
        #F8: latin(8#370)   !  ø Latin small letter O with stroke.
        #F9: latin(8#371)   !  ù Latin small letter U with grave.
        #FA: latin(8#372)   !  ú Latin small letter U with acute.
        #FB: latin(8#373)   !  û Latin small letter U with circumflex.
        #FC: latin(8#374)   !  ü Latin small letter U with diaeresis.
        #FD: latin(8#375)   !  ý Latin small letter Y with acute.
        #FE: latin(8#376)   !  þ Latin small letter Thorn.
        #FF: latin(8#377)   !  ÿ Latin small letter Y with diaeresis.
       none: latin(ch)))

!  TRANSDUCE GREEK. Write the Greek letter CH to OUTPUT. We handle letters that
!  are in the PostScript Symbol font. We print them in gray so they do not seem
!  darker than letters in the ordinary Courier text.

  transduceGreek :−
   (form (char ch) void:
    (case lb(ch)
     of #91: symbol(8#101, gp, sg)  !  Α Greek capital letter alpha.
        #92: symbol(8#102, gp, sg)  !  Β Greek capital letter beta.
        #93: symbol(8#107, gp, sg)  !  Γ Greek capital letter gamma.
        #94: symbol(8#104, gp, sg)  !  Δ Greek capital letter delta.
        #95: symbol(8#105, gp, sg)  !  Ε Greek capital letter epsilon.
        #96: symbol(8#132, gp, sg)  !  Ζ Greek capital letter zeta.
        #97: symbol(8#110, gp, sg)  !  Η Greek capital letter eta.
        #98: symbol(8#121, gp, sg)  !  Θ Greek capital letter theta.
        #99: symbol(8#111, gp, sg)  !  Ι Greek capital letter iota.
        #9A: symbol(8#113, gp, sg)  !  Κ Greek capital letter kappa.
        #9B: symbol(8#114, gp, sg)  !  Λ Greek capital letter lamda [sic].
        #9C: symbol(8#115, gp, sg)  !  Μ Greek capital letter mu.
        #9D: symbol(8#116, gp, sg)  !  Ν Greek capital letter nu.
        #9E: symbol(8#130, gp, sg)  !  Ξ Greek capital letter xi.
        #9F: symbol(8#117, gp, sg)  !  Ο Greek capital letter omicron.
        #A0: symbol(8#120, gp, sg)  !  Π Greek capital letter pi.
        #A1: symbol(8#122, gp, sg)  !  Ρ Greek capital letter rho.
        #A2: missing(ch)            !    Not assigned.
        #A3: symbol(8#123, gp, sg)  !  Σ Greek capital letter sigma.
        #A4: symbol(8#124, gp, sg)  !  Τ Greek capital letter tau.
        #A5: symbol(8#125, gp, sg)  !  Υ Greek capital letter upsilon.
        #A6: symbol(8#106, gp, sg)  !  Φ Greek capital letter phi.
        #A7: symbol(8#103, gp, sg)  !  Χ Greek capital letter chi.
        #A8: symbol(8#131, gp, sg)  !  Ψ Greek capital letter psi.
        #A9: symbol(8#127, gp, sg)  !  Ω Greek capital letter omega.
        #B1: symbol(8#141, gp, sg)  !  α Greek small letter alpha.
        #B2: symbol(8#142, gp, sg)  !  β Greek small letter beta.
        #B3: symbol(8#147, gp, sg)  !  γ Greek small letter gamma.
        #B4: symbol(8#144, gp, sg)  !  δ Greek small letter delta.
        #B5: symbol(8#145, gp, sg)  !  ε Greek small letter epsilon.
        #B6: symbol(8#172, gp, sg)  !  ζ Greek small letter zeta.
        #B7: symbol(8#150, gp, sg)  !  η Greek small letter eta.
        #B8: symbol(8#161, gp, sg)  !  θ Greek small letter theta.
        #B9: symbol(8#151, gp, sg)  !  ι Greek small letter iota.
        #BA: symbol(8#153, gp, sg)  !  κ Greek small letter kappa.
        #BB: symbol(8#154, gp, sg)  !  λ Greek small letter lamda [sic].
        #BC: symbol(8#155, gp, sg)  !  μ Greek small letter mu.
        #BD: symbol(8#156, gp, sg)  !  ν Greek small letter nu.
        #BE: symbol(8#170, gp, sg)  !  ξ Greek small letter xi.
        #BF: symbol(8#157, gp, sg)  !  ο Greek small letter omicron.
        #C0: symbol(8#160, gp, sg)  !  π Greek small letter pi.
        #C1: symbol(8#162, gp, sg)  !  ρ Greek small letter rho.
        #C2: symbol(8#126, gp, sg)  !  ς Greek small letter final sigma.
        #C3: symbol(8#163, gp, sg)  !  σ Greek small letter sigma.
        #C4: symbol(8#164, gp, sg)  !  τ Greek small letter tau.
        #C5: symbol(8#165, gp, sg)  !  υ Greek small letter upsilon.
        #C6: symbol(8#152, gp, sg)  !  φ Greek small letter phi.
        #C7: symbol(8#143, gp, sg)  !  χ Greek small letter chi.
        #C8: symbol(8#171, gp, sg)  !  ψ Greek small letter psi.
        #C9: symbol(8#167, gp, sg)  !  ω Greek small letter omega.
        #D2: symbol(8#241, gp, sg)  !  ϒ Greek upsilon with hook symbol.
        #D5: symbol(8#146, gp, sg)  !  ϕ Greek phi symbol.
        #D6: symbol(8#166, gp, sg)  !  ϖ Greek pi symbol.
        #F6: symbol(8#047, gp, sg)  !  ϶ Greek reversed lunate epsilon symbol.
       none: missing(ch)))

!  TRANSDUCE SUB SUP. Write a superscript or subscript char CH to OUTPUT, along
!  with maybe some other chars. Also see TRANSDUCE SUB₀ and TRANSDUCE SUB₁.

  transduceSubSup :−
   (form (char ch) void:
    (case lb(ch)
     of #13: symbol(8#055) !  − En dash.
        #70: super('0')    !  ⁰ Superscript zero.
        #71: super('i')    !  ⁱ Superscript latin small letter I.
        #72: missing(ch)   !    Not assigned.
        #73: missing(ch)   !    Not assigned.
        #74: super('4')    !  ⁴ Superscript four.
        #75: super('5')    !  ⁵ Superscript five.
        #76: super('6')    !  ⁶ Superscript six.
        #77: super('7')    !  ⁷ Superscript seven.
        #78: super('8')    !  ⁸ Superscript eight.
        #79: super('9')    !  ⁹ Superscript nine.
        #7A: super('+')    !  ⁺ Superscript plus sign.
        #7B: super('-')    !  ⁻ Superscript minus.
        #7C: super('=')    !  ⁼ Superscript equals sign.
        #7D: super(8#050)  !  ⁽ Superscript left parenthesis.
        #7E: super(8#051)  !  ⁾ Superscript right parenthesis.
        #7F: super('n')    !  ⁿ Superscript latin small letter N.
        #80: sub('0')      !  ₀ Subscript zero.
        #81: sub('1')      !  ₁ Subscript one.
        #82: sub('2')      !  ₂ Subscript two.
        #83: sub('3')      !  ₃ Subscript three.
        #84: sub('4')      !  ₄ Subscript four.
        #85: sub('5')      !  ₅ Subscript five.
        #86: sub('6')      !  ₆ Subscript six.
        #87: sub('7')      !  ₇ Subscript seven.
        #88: sub('8')      !  ₈ Subscript eight.
        #89: sub('9')      !  ₉ Subscript nine.
        #8A: sub('+')      !  ₊ Subscript plus sign.
        #8B: sub('-')      !  ₋ Subscript minus.
        #8C: sub('=')      !  ₌ Subscript equals sign.
        #8D: sub(8#050)    !  ₍ Subscript left parenthesis.
        #8E: sub(8#051)    !  ₎ Subscript right parenthesis.
        #8F: missing(ch)   !    Not assigned.
        #90: sub('a')      !  ₐ Subscript small letter A.
        #91: sub('e')      !  ₑ Subscript small letter E.
        #92: sub('o')      !  ₒ Subscript small letter O.
        #93: sub('x')      !  ₓ Subscript small letter X.
       none: missing(ch)))

!  TRANSDUCE ARROW. Write the arrow char CH to OUTPUT. We also write some chars
!  that look like letters, in gray.

  transduceArrow :−
   (form (char ch) void:
    (case lb(ch)
     of #11: symbol(8#301, sp, sg)  !  ℑ Black letter capital I.
        #18: symbol(8#303, sp, sg)  !  ℘ Script capital P.
        #1C: symbol(8#302, sp, sg)  !  ℜ Black letter capital R.
        #22: symbol(8#324, sp, sg)  !  ™ Trade mark sign.
        #35: symbol(8#300, sp, sg)  !  ℵ Alef [sic] symbol.
        #90: symbol(8#254, op)      !  ← Leftwards arrow.
        #91: symbol(8#255, ap)      !  ↑ Upwards arrow.
        #92: symbol(8#256, op)      !  → Rightwards arrow.
        #93: symbol(8#257, ap)      !  ↓ Downwards arrow.
        #94: symbol(8#253, op)      !  ↔ Left right arrow.
        #B2: symbol(8#277, op)      !  ↲ Downwards arrow with tip leftwards.
        #D0: symbol(8#334, op)      !  ⇐ Leftwards double arrow.
        #D1: symbol(8#335, ap)      !  ⇑ Upwards double arrow.
        #D2: symbol(8#336, op)      !  ⇒ Rightwards double arrow.
        #D3: symbol(8#337, ap)      !  ⇓ Downwards double arrow.
        #D4: symbol(8#333, op)      !  ⇔ Left right double arrow.
       none: missing(ch)))

!  TRANSDUCE SMALL MATH. Write the mathematical operator CH to OUTPUT. We write
!  some of them in gray.

  transduceSmallMath :−
   (form (char ch) void:
    (case lb(ch)
     of #00: symbol(8#042)           !  ∀ For all.
        #02: symbol(8#266, sp, sg)   !  ∂ Partial differential.
        #03: symbol(8#044)           !  ∃ There exists.
        #04: symbol(8#044, '/')      !  ∄ There does not exist.
        #05: symbol(8#306, op)       !  ∅ Empty set.
        #07: symbol(8#321, sp, sg)   !  ∇ Nabla.
        #0A: symbol(8#316)           !  ∊ Small element of.
        #0F: symbol(8#325, sp, sg)   !  ∏ N-ary product.
        #11: symbol(8#345, sp, sg)   !  ∑ N-ary summation.
        #12: symbol(8#055)           !  − Minus sign.
        #15: symbol(8#057)           !  ∕ Division slash.
        #17: symbol(8#052)           !  ∗ Asterisk operator.
        #18: ring()                  !  ∘ Ring operator.
        #19: symbol(8#267, sp, sg)   !  ∙ Bullet operator.
        #1A: symbol(8#326)           !  √ Square root.
        #1D: symbol(8#265)           !  ∝ Proportional to.
        #1E: symbol(8#245)           !  ∞ Infinity.
        #20: symbol(8#320, op)       !  ∠ Angle.
        #23: symbol(8#174)           !  ∣ Divides.
        #27: symbol(8#331)           !  ∧ Logical and.
        #28: symbol(8#332)           !  ∨ Logical or.
        #29: symbol(8#307, op)       !  ∩ Intersection.
        #2A: symbol(8#310, op)       !  ∪ Union.
        #2B: symbol(8#362)           !  ∫ Integral.
        #34: symbol(8#134)           !  ∴ Therefore.
        #3C: symbol(8#176)           !  ∼ Tilde operator.
        #45: symbol(8#100)           !  ≅ Approximately equal to.
        #47: symbol(8#100, '/')      !  ≇ Neither approx nor actually equal.
        #48: symbol(8#273)           !  ≈ Almost equal to.
        #49: symbol(8#273, '/')      !  ≉ Not almost equal to.
        #60: symbol(8#271)           !  ≠ Not equal to.
        #61: symbol(8#272)           !  ≡ Identical to.
        #62: symbol(8#272, '/')      !  ≢ Not identical to.
        #64: symbol(8#243)           !  ≤ Less-than or equal to.
        #65: symbol(8#263)           !  ≥ Greater than or equal to.
        #6E: symbol(8#073, '/')      !  ≮ Not less-than.
        #6F: symbol(8#076, '/')      !  ≯ Not greater-than.
        #70: symbol(8#243, '/')      !  ≰ Neither less-than nor equal to.
        #71: symbol(8#263, '/')      !  ≱ Neither greater-than nor equal to.
        #82: symbol(8#314, op)       !  ⊂ Subset of.
        #83: symbol(8#311, op)       !  ⊃ Superset of.
        #84: symbol(8#314, '/', op)  !  ⊄ Not a subset of.
        #85: symbol(8#311, '/', op)  !  ⊅ Not a superset of.
        #86: symbol(8#315, op)       !  ⊆ Subset of or equal to.
        #87: symbol(8#312, op)       !  ⊇ Superset of or equal to.
        #88: symbol(8#315, '/', op)  !  ⊈ Neither a subset of nor equal to.
        #89: symbol(8#312, '/', op)  !  ⊉ Neither a superset of nor equal to.
        #95: symbol(8#305)           !  ⊕ Circled plus.
        #97: symbol(8#304)           !  ⊗ Circled times.
        #A5: symbol(8#136)           !  ⊥ Up tack.
       none: missing(ch)))

!  TRANSDUCE BIG MATH. Write a part of a multi-line mathematical operator CH to
!  OUTPUT.

  transduceBigMath :−
    (form (char ch) void:
     (case lb(ch)
      of #20: symbol(8#363)  !  ⌠ Top half integral.
         #21: symbol(8#365)  !  ⌡ Bottom half integral.
         #29: symbol(8#341)  !  〈 Left-pointing angle bracket.
         #2A: symbol(8#361)  !  〉 Right-pointing angle bracket .
         #9B: symbol(8#346)  !  ⎛ Left parenthesis upper hook.
         #9C: symbol(8#347)  !  ⎜ Left parenthesis extension.
         #9D: symbol(8#350)  !  ⎝ Left parenthesis lower hook.
         #9E: symbol(8#366)  !  ⎞ Right parenthesis upper hook.
         #9F: symbol(8#367)  !  ⎟ Right parenthesis extension.
         #A0: symbol(8#370)  !  ⎠ Right parenthesis lower hook.
         #A1: symbol(8#351)  !  ⎡ Left square bracket upper corner.
         #A2: symbol(8#352)  !  ⎢ Left square bracket extension.
         #A3: symbol(8#353)  !  ⎣ Left square bracket lower corner.
         #A4: symbol(8#371)  !  ⎤ Right square bracket upper corner.
         #A5: symbol(8#372)  !  ⎥ Right square bracket extension.
         #A6: symbol(8#373)  !  ⎦ Right square bracket lower corner.
         #A7: symbol(8#354)  !  ⎧ Left curly bracket upper hook.
         #A8: symbol(8#355)  !  ⎨ Left curly bracket middle piece.
         #A9: symbol(8#356)  !  ⎩ Left curly bracket lower hook.
         #AA: symbol(8#357)  !  ⎪ Curly bracket extension.
         #AB: symbol(8#374)  !  ⎫ Right curly bracket upper hook.
         #AC: symbol(8#375)  !  ⎬ Right curly bracket middle piece.
         #AD: symbol(8#376)  !  ⎭ Right curly bracket lower hook.
         #AE: symbol(8#364)  !  ⎮ Integral extension.
        none: missing(ch)))

!  TRANSDUCE SUB₀. Write a subscript letter CH to OUTPUT.

  transduceSub₀ :−
   (form (char ch) void:
    (case lb(ch)
     of #62: sub('i')  !  ᵢ Latin subscript small letter I.
        #63: sub('r')  !  ᵣ Latin subscript small letter R.
        #64: sub('u')  !  ᵤ Latin subscript small letter U.
        #65: sub('v')  !  ᵥ Latin subscript small letter V.
       none: missing(ch)))

!  TRANSDUCE SUB₁. Write a subscript letter CH to OUTPUT.

  transduceSub₁ :−
   (form (char ch) void:
    (case lb(ch)
     of #7C: sub('j')  !  ⱼ Latin subscript small letter J.
       none: missing(ch)))

!  TRANSDUCE SUIT. Write a card suit CH to OUTPUT, in gray.

  transduceSuit :−
   (form (char ch) void:
    (case lb(ch)
     of #60: symbol(8#252, sp, sg)  !  ♠ Black spade suit.
        #63: symbol(8#247, sp, sg)  !  ♣ Black club suit.
        #65: symbol(8#251, sp, sg)  !  ♥ Black heart suit.
        #66: symbol(8#250, sp, sg)  !  ♦ Black diamond suit.
       none: missing(ch)))

!  LATIN. Write the PostScript Latin char CH to OUTPUT. It might be overprinted
!  with OVER. The procedures LATIN₁ through LATIN₄ do most of the work.

  latin :−
   (alt
    (form (char ch) void:
      write(ch)),
    (form (char ch, char over) void:
      latin₁(ch, over)),
    (form (char ch, int over) void:
      latin₂(ch, over)),
    (form (int ch) void:
      latin₃(ch)),
    (form (int ch, int over) void:
      latin₄(ch, over)))

!  LATIN₁. Write the PostScript Latin char CH to OUTPUT, overprinted with OVER.

  latin₁ :−
   (proc (char ch, char over) void:
    (in postScript()
     do write(''(%c) show '': ch))
    write(backspace)
    (in postScript()
     do write(''(%c) show '': over)))

!  LATIN₂. Write the PostScript Latin char CH to OUTPUT, overprinted with OVER.

  latin₂ :−
   (proc (char ch, int over) void:
    (in postScript()
     do write(''(%c) show '': ch))
    write(backspace)
    (in postScript()
     do write(''(\\%o) show '': over)))

!  LATIN₃. Write the PostScript Latin char CH to OUTPUT.

  latin₃ :−
   (proc (int ch) void:
    (in postScript()
     do write(''(\\%o) show '': ch)))

!  LATIN₄. Write the PostScript Latin char CH to OUTPUT, overprinted with OVER.

  latin₄ :−
   (proc (int ch, int over) void:
    (in postScript()
     do write(''(\\%o) show '': ch))
    write(backspace)
    (in postScript()
     do write(''(\\%o) show '': over)))

!  MISSING. Add CH to the BST of missing chars, whose root is ROOT, and write a
!  gray MYSTERY char (from the Symbol font) in its place.

  missing :−
   (proc (char ch) void:
    (with var ref node subtree :− root
     do (while
         (if ch < subtree↑.ch
          then (if subtree↑.left = nil
                then subtree↑.left := makeNode(ch)
                     false
                else subtree := subtree↑.left
                     true)
          else if ch > subtree↑.ch
               then (if subtree↑.right = nil
                     then subtree↑.right := makeNode(ch)
                          false
                     else subtree := subtree↑.right
                          true)
               else false)))
    (in postScript()
     do write(''/Symbol findfont %4.2f scalefont setfont '': cp)
        write(''0.5 setgray (\\%o) show 0.0 setgray '': mystery)))

!  RING. A special case kludge to print the ring operator "∘" (U+2218).

  ring :−
   (proc () void:
    (in postScript()
     do write(''0 %i rmoveto (\\260) show 0 %i moveto '': −1, 1)))

!  SUB. Write the char CH to OUTPUT as a subscript. Procedures SUB₀ and SUB₁ do
!  all the work.

  sub :−
   (alt
    (form (char ch) void:
      sub₀(ch)),
    (form (int ch) void:
      sub₁(ch)))

!  SUB₀. Write the char CH to OUTPUT as a subscript.

  sub₀ :−
   (proc (char ch) void:
    (in postScript()
     do write(''/Courier findfont %4.2f scalefont setfont '': ssp)
        write(''0 %i rmoveto '': − lower)
        write(''(%c) show '': ch)
        write(''0 %i rmoveto '': lower)))

!  SUB₁. Write the char CH to OUTPUT as a subscript.

  sub₁ :−
   (proc (int ch) void:
    (in postScript()
     do write(''/Courier findfont %4.2f scalefont setfont '': ssp)
        write(''0 %i rmoveto '': − lower)
        write(''(\\%o) show '': ch)
        write(''0 %i rmoveto '': lower)))

!  SUPER. Write a char CH to OUTPUT as a superscript. The procedures SUPER₀ and
!  SUPER₁ do all the work.

  super :−
   (alt
    (form (char ch) void:
      super₀(ch)),
    (form (int ch) void:
      super₁(ch)))

!  SUPER₀. Write the char CH to OUTPUT as a superscript.

  super₀ :−
   (proc (char ch) void:
    (in postScript()
     do write(''/Courier findfont %4.2f scalefont setfont '': ssp)
        write(''0 %i rmoveto '': upper)
        write(''(%c) show '': ch)
        write(''0 %i rmoveto '': − upper)))

!  SUPER₁. Write the char CH to OUTPUT as a superscript.

  super₁ :−
   (proc (int ch) void:
    (in postScript()
     do write(''/Courier findfont %4.2f scalefont setfont '': ssp)
        write(''0 %i rmoveto '': upper)
        write(''(\\%o) show '': ch)
        write(''0 %i rmoveto '': − upper)))

!  SYMBOL. Write a char CH from the PostScript SYMBOL font to OUTPUT. It may be
!  in the size POINTS and in the shade GRAY. Procedures SYMBOL₀ through SYMBOL₄
!  do all the work.

  symbol :−
   (alt
    (form (int ch) void:
      symbol₀(ch)),
    (form (int ch, real points) void:
      symbol₁(ch, points)),
    (form (int ch, real points, real gray) void:
      symbol₂(ch, points, gray)),
    (form (int ch, char over) void:
      symbol₃(ch, over)),
    (form (int ch, char over, real points) void:
      symbol₄(ch, over, points)))

!  SYMBOL₀. Write the char CH to OUTPUT.

  symbol₀ :−
   (proc (int ch) void:
    (in postScript()
     do write(''/Symbol findfont %4.2f scalefont setfont '': cp)
        write(''(\\%o) show '': ch)))

!  SYMBOL₁. Write the char CH to OUTPUT in size POINTS.

  symbol₁ :−
   (proc (int ch, real points) void:
    (in postScript()
     do write(''/Symbol findfont %4.2f scalefont setfont '': points)
        write(''(\\%o) show '': ch)))

!  SYMBOL₂. Write the char CH to OUTPUT in size POINTS and shade GRAY.

  symbol₂ :−
   (proc (int ch, real points, real gray) void:
    (in postScript()
     do write(''/Symbol findfont %4.2f scalefont setfont '': points)
        write(''%4.2f setgray (\\%o) show 0.0 setgray '': gray, ch)))

!  SYMBOL₃. Write the char CH to OUTPUT, overprinted by OVER.

  symbol₃ :−
   (proc (int ch, char over) void:
    (in postScript()
     do write(''/Symbol findfont %4.2f scalefont setfont '': cp)
        write(''(\\%o) show '': ch))
    write(backspace)
    write(over))

!  SYMBOL₄. Write the char CH to OUTPUT, overprinted by OVER, with both in size
!  POINTS.

  symbol₄ :−
   (proc (int ch, char over, real points) void:
    (in postScript()
     do write(''/Symbol findfont %4.2f scalefont setfont '': points)
        write(''(\\%o) show '': ch))
    write(backspace)
    write(over))

!  POST SCRIPT. Wrapper. Call BODY inside an Enscript PS escape. For an unknown
!  reason, we must have an extra blank after "}" to maintain correct alignment.

  postScript :−
   (form () foj:
    (form (form () obj body) obj:
      write(nul)
      write(''ps{gsave '')
      body()
      write('' grestore} '')))

!  START. Run the main program.

  start :− main()
)
