//
//  ORSON/ERROR. Scold the user about errors.
//
//  Copyright (C) 2014 James B. Moen.
//
//  This program  is free  software: you can  redistribute it and/or  modify it
//  under the terms of the  GNU General Public License as published by the Free
//  Software Foundation, either  version 3 of the License,  or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY  WARRANTY; without  even  the implied  warranty  of MERCHANTABILITY  or
//  FITNESS FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License for
//  more details.
//
//  You should  have received a  copy of the  GNU General Public  License along
//  with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "global.h"

//  INIT ERROR. Initialize globals.

void initError()

//  MAKE ERR. Associate the error ERR with a MNEMONIC and a descriptive STRING.
//  Make sure we don't use the same mnemonic twice.

{ void makeErr(int err, refChar mnemonic, refChar string)
  { errToMnemonic[err] = mnemonic;
    errToString[err] = string;
    err -= 1;
    while (err > minErr)
    { if (strcmp(errToMnemonic[err], mnemonic) == 0)
      { fail("Mnemonic '%s' appears twice in makeErr!", mnemonic); }
      else
      { err -= 1; }}}

//  Variables that attribute errors to source file positions. (See ORSON/FILE.)

  allErrs   = setEmpty();
  charCount = 0;
  places    = nil;

//  Associate ERRs with their corresponding mnemonics and strings. This must be
//  done in order of ERRs. Mnemonics must be ASCII (see ORSON/CHAR).

  makeErr(apostropheErr,    "ax",   "Apostrophe expected.");
  makeErr(apostrophesErr,   "aax",  "Two apostrophes expected.");
  makeErr(assertErr,        "taf",  "Transformation assertion failed.");
  makeErr(assignmentErr,    "asx",  "Assignment expected.");
  makeErr(callErr,          "uc",   "Unexpected call.");
  makeErr(chaInjErr,        "ciex", "'cha' or 'inj' expression expected.");
  makeErr(charErr,          "chx",  "Character expected.");
  makeErr(closeBraceErr,    "cbcx", "Close brace expected.");
  makeErr(closeBracketErr,  "cbkx", "Close bracket expected.");
  makeErr(closeParenErr,    "cpx",  "Close parenthesis expected.");
  makeErr(colonDashErr,     "cdx",  "Colon dash expected.");
  makeErr(colonErr,         "cox",  "Colon expected.");
  makeErr(constantErr,      "cex",  "Constant expression expected.");
  makeErr(divideByZeroErr,  "dbz",  "Division by zero.");
  makeErr(doErr,            "dx",   "'do' expected.");
  makeErr(elementErr,       "uel",  "Unexpected element.");
  makeErr(errNumberErr,     "enx",  "Error number expected.");
  makeErr(exeErr,           "eex",  "'exe' expression expected.");
  makeErr(fileCloseErr,     "cc",   "Cannot close.");
  makeErr(fileOpenErr,      "co",   "Cannot open.");
  makeErr(fileSuffixErr,    "mufs", "Missing or unknown file suffix.");
  makeErr(fileTooBigErr,    "ftl",  "File too large.");
  makeErr(fojErr,           "fex",  "'foj' expression expected.");
  makeErr(formTypeErr,      "fx",   "'form' expected.");
  makeErr(formsTooDeepErr,  "frtd", "Form recursion too deep.");
  makeErr(haltErr,          "th",   "Transformation halted.");
  makeErr(hexDigitErr,      "hdx",  "Hexadecimal digit expected.");
  makeErr(hookErr,          "hx",   "Hook expected.");
  makeErr(illegalCharErr,   "ic",   "Illegal character.");
  makeErr(illegalNumberErr, "in",   "Illegal number.");
  makeErr(illegalRadixErr,  "rdx",  "Radix digit expected.");
  makeErr(illegalTokenErr,  "is",   "Illegal symbol.");
  makeErr(injErr,           "iex",  "'inj' expression expected.");
  makeErr(internalErr,      "ie",   "Internal error.");
  makeErr(jokerErr,         "jtx",  "Joker type expected.");
  makeErr(labelTypeErr,     "lhut", "Case label has unexpected type.");
  makeErr(limitErr,         "ile",  "Internal limit exceeded.");
  makeErr(lineTooLongErr,   "sltl", "Source line too long.");
  makeErr(loadOrProgErr,    "lpx",  "'load' or 'prog' expected.");
  makeErr(memberTypeErr,    "mftx", "Member form type expected.");
  makeErr(metErr,           "meex", "'met' expression expected.");
  makeErr(methodErr,        "mhut", "Method has unexpected type.");
  makeErr(misplacedHookErr, "mh",   "Misplaced hook.");
  makeErr(mutErr,           "mux",  "'mut' expression expected.");
  makeErr(nameErr,          "nx",   "Name expected.");
  makeErr(negInjErr,        "nix",  "Negative 'inj' expression expected.");
  makeErr(noBaseTypeErr,    "thnb", "Type has no base type.");
  makeErr(noneErr,          "nna",  "'none' not allowed here.");
  makeErr(nonForwardErr,    "uftx", "Unforwarded pointer type expected.");
  makeErr(nonJokerErr,      "njtx", "Non joker type expected.");
  makeErr(nonNegInjErr,     "nnix", "Nonnegative 'inj' expression expected.");
  makeErr(nonNilErr,        "nnpx", "Non 'nil' pointer expected.");
  makeErr(nonNullTypeErr,   "ntnx", "Non 'null' pointer type expected.");
  makeErr(nonPosInjErr,     "npix", "Nonpositive 'inj' expression expected.");
  makeErr(nonZeroInjErr,    "nzix", "Nonzero 'inj' expression expected.");
  makeErr(noSuchKeyErr,     "kne",  "Key not in environment.");
  makeErr(noSuchParamErr,   "nspn", "No such parameter name.");
  makeErr(notBindableErr,   "gnub", "Generic name cannot be bound.");
  makeErr(notInsideFormErr, "nif",  "Not inside form.");
  makeErr(objectErr,        "uo",   "Unexpected object.");
  makeErr(ofErr,            "ox",   "'of' expected.");
  makeErr(openParenErr,     "opx",  "Open parenthesis expected.");
  makeErr(outOfMemoryErr,   "me",   "Memory exhausted.");
  makeErr(posInjErr,        "pix",  "Positive 'inj' expression expected.");
  makeErr(quoteErr,         "qx",   "Quote expected.");
  makeErr(rangeErr,         "oor",  "Out of range.");
  makeErr(repeatedLabelErr, "rcl",  "Repeated case label.");
  makeErr(repeatedNameErr,  "rn",   "Repeated name.");
  makeErr(semicolonErr,     "slex", "Semicolon or line end expected.");
  makeErr(shadowedGenErr,   "sgn",  "Shadowed generic name.");
  makeErr(slashableErr,     "scx",  "Slashable character expected.");
  makeErr(subsumedFormErr,  "sf",   "Subsumed form or form type.");
  makeErr(termErr,          "tex",  "Term expected.");
  makeErr(thenErr,          "thx",  "'then' expected.");
  makeErr(tokenErr,         "us",   "Unexpected symbol.");
  makeErr(tooFewElemsErr,   "tfe",  "Too few elements.");
  makeErr(tooManyElemsErr,  "tme",  "Too many elements.");
  makeErr(tooManyKeysErr,   "tmke", "Too many keys in environment.");
  makeErr(typeErr,          "ehut", "Expression has unexpected type.");
  makeErr(typeExeErr,       "teex", "'type exe' expression expected.");
  makeErr(typeMutErr,       "tmex", "'type mut' expression expected.");
  makeErr(typeObjErr,       "toex", "'type obj' expression expected.");
  makeErr(typeSizeErr,      "tstl", "Type size too large.");
  makeErr(typeTypeObjErr,   "ttox", "'type type obj' expression expected.");
  makeErr(unboundErr,       "nhnb", "Name has no binding.");
  makeErr(undeclaredErr,    "nnd",  "Name was never declared.");
  makeErr(unknownCallErr,   "cuo",  "Called unknown object.");
  makeErr(unknownObjectErr, "tuo",  "Transformed unknown object.");
  makeErr(versionErr,       "vdnm", "Version does not match.");
  makeErr(zeroInjErr,       "zix",  "Zero 'inj' expression expected."); }

//  INSERT ERR.  Assert that the error ERR was found at character number COUNT.
//  We add ERR to ALL ERRS, then add COUNT and ERR to a PLACE in PLACES, making
//  sure that PLACES is sorted in increasing order of COUNTs.  FIRST PLACE is a
//  temporary head node that simplifies insertion.

void insertErr(int count, int err)
{ place firstPlace;
  refPlace leftPlace = r(firstPlace);
  refPlace rightPlace = places;
  firstPlace.next = places;
  while (true)
  { if (rightPlace == nil)
    { next(leftPlace) = makePlace(count, err, nil);
      break; }
    else if (count(rightPlace) == count)
         { errs(rightPlace) = setAdjoin(errs(rightPlace), err);
           break; }
         else if (count(rightPlace) > count)
              { next(leftPlace) = makePlace(count, err, rightPlace);
                break; }
              else
              { leftPlace = rightPlace;
                rightPlace = next(rightPlace); }}
  places = firstPlace.next;
  allErrs = setAdjoin(allErrs, err); }

//  OBJECT ERROR. Assert that ERR occurred while we did something to OBJECT. It
//  is an error if OBJECT isn't a pair.

void objectError(refObject object, int err)
{ if (object == nil)
  { fail("Got [Nil] in objectError!"); }
  else if (isPair(object) && ! isTriple(object))
       { if (info(object) >= 0)
         { if (level <= maxDebugLevel)
           { fprintf(stdout, "[%i] Error: %s\n", level, errToString[err]); }
           insertErr(info(object), err); }}
       else
       { fail("Pair expected in objectError!"); }}

//  SOURCE ERROR. Assert that ERR occurred at character number CHAR COUNT.

void sourceError(int err)
{ insertErr(charCount, err); }

//  GET ERRS. Return the set of ERRs from the character count COUNT. If the set
//  isn't empty, then it's in the first PLACE in PLACES, which gets deleted. We
//  need not FREE it because Orson will halt soon anyway.

set getErrs(int count)
{ if (places != nil && count(places) == count)
  { set errs = errs(places);
    places = next(places);
    return errs; }
  else
  { return setEmpty(); }}

//  WRITE ERROR LINES. Write a summary of the errors in all source files.

void writeErrorLines()
{ int       ch;                   //  Most recent char from SOURCE.
  set       errs[maxLineLength];  //  Set of errors at each position from LINE.
  refSet    errsEnd;              //  End of ERRS.
  refFile   file;                 //  A revisited FILE.
  int       index;                //  For a FOR loop.
  int       line[maxLineLength];  //  Most recent line from SOURCE.
  refInt    lineEnd;              //  End of LINE.
  int       lineNumber;           //  Count lines read from SOURCE.
  refInt    lineStart;            //  Start of LINE.
  refChar   newline;              //  A newline or "".
  refStream source;               //  Connected to an Orson source program.
  bool      titled;               //  Have we written a title?

//  NEXT LINE. Read the next line from SOURCE into LINE. If L is the ASCII line
//  feed char, and R is the ASCII return char, then a line may be terminated by
//  L, R, L R, or R L. The last line in SOURCE may also be terminated by an EOF
//  CHAR. The end of SOURCE is signaled by a line containing a lone EOP CHAR.

  void nextLine()
  { int temp;
    lineEnd = line;
    lineStart = line;
    while (true)
    { temp = getChar(source);
      switch (temp)
      { case eofChar:
        { if (lineEnd == lineStart)
          { d(lineEnd) = eopChar;
            lineEnd += 1; }
          d(lineEnd) = eosChar;
          return; }
        case linefeedChar:
        { temp = getc(source);
          if (temp != eofChar && temp != returnChar)
          { ungetc(temp, source); }
          d(lineEnd) = eosChar;
          return; }
        case returnChar:
        { temp = getc(source);
          if (temp != eofChar && temp != linefeedChar)
          { ungetc(temp, source); }
          d(lineEnd) = eosChar;
          return; }
        default:
        { if (lineEnd < line + maxLineLength - 1)
          { if (isIllegalChar(temp))
            { d(lineEnd) = ' '; }
            else
            { d(lineEnd) = temp; }
            lineEnd += 1; }
          break; }}}}

//  NEXT CHAR. Copy the next char from LINE into CH. If LINE is empty, read the
//  next line before we do it.

  void nextChar()
  { if (lineStart > lineEnd)
    { nextLine(); }
    ch = d(lineStart);
    charCount += 1;
    lineStart += 1; }

//  WRITE ERROR LINE. If all sets in ERRS are empty, then do nothing. Otherwise
//  write LINE NUMBER and its LINE. Then on the next line, at each column where
//  an error occurred, write an arrow or caret followed by MNEMONICs describing
//  the error. If this covers columns in LINE where other errors occurred, then
//  write more such lines, until all columns with errors are indicated. This is
//  easier to see than to describe: here's an example in ASCII that needs three
//  lines.
//
//    00018       (case ch := read(source); ch
//                      ^iex,nnd   ^nnd     ^nnd
//                         ^mhut
//                            ^meex,nnd
//
//  We first saw this done in the Wirth-Ammann Pascal compiler for the CDC 6400
//  in the 1970's. It used error numbers instead of mnemonic names and couldn't
//  write the multiple lines that resulted from covered columns.

  void writeErrorLine()
  { int     blanks;     //  Count blanks waiting to be written.
    int     count;      //  Number of ASCII chars in a MNEMONIC.
    refChar delimiter;  //  An arrow, a caret, or a comma.
    int     error;      //  An error that might be in ERRORS.
    set     errors;     //  Set of errors at a location in ERRS.
    refSet  errsStart;  //  A tail of ERRS.
    bool    going;      //  Is there an ERROR waiting to be written?
    refInt  lineStart;  //  A fail of LINE.
    refChar mnemonic;   //  Short mnemonic string for ERROR.
    int     width;      //  Number of columns to display a char.

//  Search ERRS for a nonempty set of errors. If we find such a set, then write
//  the current source file's pathname (if we haven't already), and the current
//  line's number. If the number is too big, write question marks instead.

    errsStart = errs;
    going = false;
    while (! going && errsStart < errsEnd)
    { going = ! isSetEmpty(d(errsStart));
      errsStart += 1; }
    if (going)
    { if (! titled)
      { fprintf(stdout, "%sErrors in '%s'.\n\n", newline, path(file));
        newline = "\n";
        titled = true; }
      if (lineNumber > maxLineNumber)
      { int count = lineNumberLength;
        while (count > 0)
        { fputc('?', stdout);
          count -= 1; }
        fputc(' ', stdout); }
      else
      { fprintf(stdout, "%0*i ", lineNumberLength, lineNumber); }
      lineStart = line;

//  Here we're in ASCII mode, so each char in LINE occupies exactly one column.
//  Write the erroneous line, with non ASCII chars replaced by '_'s. Then write
//  one or more following lines that point at columns where errors appear.

      if (asciiing)
      { while (d(lineStart) != eosChar)
        { if (isVisibleAsciiChar(d(lineStart)))
          { fputc(d(lineStart), stdout); }
          else
          { fputc('_', stdout); }
          lineStart += 1; }
        fputc(eolChar, stdout);
        while (going)
        { going = false;
          blanks = lineNumberLength + 1;
          errsStart = errs;
          while (errsStart < errsEnd)
          { if (isSetEmpty(d(errsStart)))
            { blanks += 1;
              errsStart += 1; }
            else
            { going = true;
              writeBlanks(stdout, blanks);
              blanks = 0;
              errors = d(errsStart);
              d(errsStart) = setEmpty();
              delimiter = caretChar;
              for (error = minErr + 1; error < maxErr; error += 1)
              { if (isInSet(error, errors))
                { mnemonic = errToMnemonic[error];
                  fputs(delimiter, stdout);
                  fputs(mnemonic, stdout);
                  errsStart += strlen(mnemonic) + 1;
                  delimiter = ","; }}}}
          if (going)
          { fputc(eolChar, stdout); }}}

//  Here we're not in ASCII mode, so some chars may occupy two or more columns.
//  Write the erroneous line. Chars with negative widths are written as UNKNOWN
//  WIDTH CHARs. Those with zero width are written after NO BREAK BLANKs. Write
//  one or more following lines that point at columns where errors appear.

      else
      { while (d(lineStart) != eosChar)
        { width = charWidth(d(lineStart));
          if (width < 0)
          { putChar(stdout, unknownWidthChar); }
          else if (width == 0)
               { putChar(stdout, noBreakBlank);
                 putChar(stdout, d(lineStart)); }
               else
               { putChar(stdout, d(lineStart)); }
          lineStart += 1; }
        fputc(eolChar, stdout);
        while (going)
        { going = false;
          blanks = lineNumberLength + 1;
          errsStart = errs;
          lineStart = line;
          while (errsStart < errsEnd)
          { if (isSetEmpty(d(errsStart)))
            { width = charWidth(d(lineStart));
              blanks += (width <= 0 ? 1 : width);
              errsStart += 1;
              lineStart += 1; }
            else
            { going = true;
              writeBlanks(stdout, blanks + charWidth(d(lineStart)) - 1);
              blanks = 0;
              errors = d(errsStart);
              d(errsStart) = setEmpty();
              delimiter = uparrowChar;
              for (error = minErr + 1; error < maxErr; error += 1)
              { if (isInSet(error, errors))
                { mnemonic = errToMnemonic[error];
                  fputs(delimiter, stdout);
                  fputs(mnemonic, stdout);
                  count = strlen(mnemonic) + 1;
                  while (count > 0)
                  { width = charWidth(d(lineStart));
                    count -= (width <= 0 ? 1 : width);
                    errsStart += 1;
                    lineStart += 1; }
                  delimiter = ","; }}}}
          if (going)
          { fputc(eolChar, stdout); }}}}}

//  Lost? This is WRITE ERROR LINES's body. Initialize.

  newline = "";
  for (index = 0; index < maxLineLength; index += 1)
  { errs[index] = setEmpty(); }

//  Reread all Orson source files whose paths are in FILES. Each time we find a
//  line with errors, we write that line, followed by one or more lines showing
//  where the errors occurred.

  file = next(firstFile);
  while (file != nil && places != nil)
  { if (isEnd(path(file), orsonPrelude) || isEnd(path(file), orsonSource))
    { charCount = count(file);
      errsEnd = errs;
      lineNumber = 0;
      titled = false;
      source = fopen(path(file), "r");
      if (source == nil)
      { fail("Cannot open '%s' in writeErrors!", path(file)); }
      else
      { nextLine();
        nextChar();
        while (true)
        { if (ch == eopChar)
          { d(errsEnd) = getErrs(charCount);
            errsEnd += 1;
            nextChar();
            d(errsEnd) = getErrs(charCount);
            errsEnd += 1;
            lineNumber += 1;
            line[0] = eosChar;
            writeErrorLine();
            break; }
          else if (ch == eosChar)
               { d(errsEnd) = getErrs(charCount);
                 errsEnd += 1;
                 lineNumber += 1;
                 writeErrorLine();
                 errsEnd = errs;
                 nextChar(); }
               else
               { d(errsEnd) = getErrs(charCount);
                 errsEnd += 1;
                 nextChar(); }}
        if (fclose(source) != 0)
        { fail("Cannot close '%s' in writeErrors!", path(file)); }}}
    file = next(file); }

//  We'd better have accounted for all errors in this way.

  if (places != nil)
  { fail("Errors remain unresolved in writeErrors!"); }}

//  WRITE ERROR MESSAGES. Write messages that describe errors in ALL ERRS. It's
//  ugly, but we need only call it once, just before Orson halts.

void writeErrorMessages()
{ int     err;
  set     errs;
  int     length;
  refChar mnemonic;
  refChar string;
  int     tempErr;
  int     tempLength;
  refChar tempMnemonic;

//  Find the LENGTH of the longest mnemonic that corresponds to an ERR in ERRS.
//  We use it below for formatting.

  errs = allErrs;
  if (! isSetEmpty(errs))
  { length = 1;
    for (err = minErr + 1; err < maxErr; err += 1)
    { if (isInSet(err, errs))
      { tempLength = strlen(errToMnemonic[err]);
        if (tempLength > length)
        { length = tempLength; }}}

//  Visit all ERRs in ERRS in order of their corresponding MNEMONICs. Write the
//  MNEMONIC for each ERR along with its descriptive STRING. We change STRING's
//  grave accents to quotation marks so we don't have to backslash them.

    fputc(eolChar, stdout);
    while (! isSetEmpty(errs))
    { mnemonic = "~";
      for (tempErr = minErr + 1; tempErr < maxErr; tempErr += 1)
      { if (isInSet(tempErr, errs))
        { tempMnemonic = errToMnemonic[tempErr];
          if (strcmp(tempMnemonic, mnemonic) < 0)
          { err = tempErr;
            mnemonic = tempMnemonic; }}}
      errs = setRemove(errs, err);
      fprintf(stdout, "%*s: ", length, mnemonic);
      string = errToString[err];
      while (d(string) != eosChar)
      { if (d(string) == accentChar)
        { fputc(doubleChar, stdout); }
        else
        { fputc(d(string), stdout); }
        string += 1; }
      fputc(eolChar, stdout); }}}
