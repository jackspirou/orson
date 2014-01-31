//
//  ORSON/MAIN. Main program.
//
//  Copyright (C) 2013 James B. Moen.
//
//  This program  is free  software: you can  redistribute it and/or  modify it
//  under the terms of the GNU  General Public License as published by the Free
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

//  MAIN. Main program. Initialize, translate, and maybe compile.

int main(int count, refRefChar strings)
{ char      path[maxPathLength];  //  An absolute pathname.
  set       seen;                 //  Command line options seen so far.
  refStream source;               //  Read source files through this.
  bool      who;                  //  Did the user ask who we are?

//  FINALIZE. Perform various cleanup actions just before FAILing.

  void finalize()
  { writeErrorLines();
    writeErrorMessages();
    if (fclose(stream(target)) != 0)
    { fail("Cannot close file '%s'.", targetPath); }
    if (unlink(targetPath) != 0)
    { fail("Cannot delete file '%s'.", targetPath); }}

//  STRING OPTION. Return the string value of OPTION. It's either the string on
//  the command line that follows OPTION, or else it's everything but the first
//  character of OPTION.

  refChar stringOption(refChar option)
  { if (option[1] == eosChar)
    { count -= 1; strings += 1;
      if (count > 0 && d(d(strings)) != eosChar)
      { return d(strings); }
      else
      { fail("Missing value for '-%c'.", d(option)); }}
    else
    { return option + 1; }}

//  INT OPTION. Like STRING OPTION, but here we return the INT value of OPTION,
//  which must be between MIN and MAX.

  int intOption(refChar option, int min, int max)
  { refChar end;
    int number;
    errno = 0;
    number = strtol(stringOption(option), r(end), 10);
    if (d(end) != eosChar || errno == outRange || number < min || number > max)
    { fail("Illegal value for '-%c'.", d(option)); }
    else
    { return number; }}

//  Default values for command line options. Some variables must be global so a
//  LONGJMP won't clobber them (see below). The -C and -T options use COMPILING
//  and UNLINKING to control their behavior.

  asciiing      = false;               //  Option -a.     (ASCII.)
  compiling     = true;                //  Option -c, -t. (Compile, Translate.)
  maxDebugLevel = -1;                  //  Option -d.     (Debug.)
  heapCount     = 1;                   //  Option -h.     (Heap.)
  targetPath    = targetFile cSource;  //  Option -o.     (Output.)
  maxLevel      = 1024;                //  Option -s.     (Stack.)
  unlinking     = true;                //  Option -c, -t. (Compile, Translate.)
  usePrelude    = true;                //  Option -r.     (Raw.)
  who           = false;               //  Option -v.     (Version.)

//  Establish actual values for command line options, overriding defaults. Note
//  that non ASCII options are illegal.

  count -= 1; strings += 1;
  seen = setEmpty();
  while (count > 0 && d(d(strings)) == '-')
  { refChar string = d(strings) + 1;
    if (isInSet(d(string), seen))
    { fail("Illegal use of option '-%c'.", string); }
    else
    { switch (d(string))
      { case eosChar:
        { fail("Unknown option '-'."); }
        case 'd':
        { maxDebugLevel = intOption(string, 0, maxInt);
          seen = setAdjoin(seen, 'd');
          break; }
        case 'h':
        { heapCount = intOption(string, 1, maxInt);
          seen = setAdjoin(seen, 'h');
          break; }
        case 'o':
        { targetPath = stringOption(string);
          seen = setAdjoin(seen, 'o');
          break; }
        case 's':
        { maxLevel = intOption(string, 0, maxInt);
          seen = setAdjoin(seen, 's');
          break; }
        default:
        { while (d(string) != eosChar)
          { if (isInSet(d(string), seen))
            { fail("Illegal use of option '-%c'.", d(string)); }
            else
            { switch (d(string))
              { case 'a':
                { asciiing = true;
                  seen = setAdjoin(seen, 'a');
                  break; }
                case 'c':
                { compiling = true;
                  unlinking = true;
                  seen = setAdjoin(seen, 'c');
                  seen = setAdjoin(seen, 't');
                  break; }
                case 'r':
                { usePrelude = false;
                  seen = setAdjoin(seen, 'r');
                  break; }
                case 't':
                { compiling = false;
                  unlinking = false;
                  seen = setAdjoin(seen, 'c');
                  seen = setAdjoin(seen, 't');
                  break; }
                case 'v':
                { who = true;
                  seen = setAdjoin(seen, 'v');
                  break; }
                default:
                { refChar option = encodeChar(removeChar(r(string)));
                  fail("Unknown option '-%s'.", option); }}}
            string += 1; }
          break; }}}
    count -= 1; strings += 1; }

//  Did the user ask who we are?

  if (who)
  { fprintf(stdout,
     "Orson to GNU C translator, "
     "version " versionName " (" versionNumber ")\n"); }

//  If there are arguments left on the command line, then they are the names of
//  source files, so compile them. Start by initializing subsystems, which must
//  be done in a specific order.

  if (count > 0)
  { initBuffer();
    initError();
    initFile();
    initHunk();
    initMake();
    initLayer();
    initLoad();
    initPrelude();
    initSize();
    initSubtype();
    initTransform();
    initEmit();
    initExpression();
    initStatement();

//  Open a target file to receive translated C code.

    stream(target) = fopen(targetPath, "w");
    if (stream(target) == nil)
    { fail("Cannot open file '%s'.", targetPath); }
    else
    { writeHerald(stream(target)); }

//  If something awful happens during translation then we LONGJMP to HALT. Look
//  for a prelude file and translate it to C if we find it.

    if (setjmp(halt) == 0)
    { if (usePrelude)
      { source = openPortablePath(path, "lib.prelude:op");
        if (source == nil)
        { fail("Cannot open prelude file."); }
        else
        { if (isTooBig(path))
          { fail("Too many chars in file '%s'.", path); }
          else
          { loadOrson(path, source, true); }
          if (fclose(source) != 0)
          { finalize();
            fail("Cannot close file '%s'.", path); }}}

//  Translate the files named on the command line to C. This is equivalent to a
//  series of LOAD clauses in which the file names appear.

      while (count > 0)
      { if (realpath(d(strings), path) == nil)
        { finalize();
          fail("Cannot find file '%s'.", d(strings)); }
        else
        { source = fopen(path, "r");
          if (source == nil)
          { finalize();
            fail("Cannot open file '%s'.", path); }
          else
          { if (! isLoaded(path))
            { if (isEnd(path, cHeader) || isEnd(path, cSource))
              { loadC(path, source); }
              else if (isEnd(path, orsonPrelude))
                   { if (isTooBig(path))
                     { finalize();
                       fail("Too many chars in file '%s'.", path); }
                     else
                     { loadOrson(path, source, true); }}
                   else if (isEnd(path, orsonSource))
                        { if (isTooBig(path))
                          { finalize();
                            fail("Too many chars in file '%s'.", path); }
                          else
                          { loadOrson(path, source, false); }}
                        else
                        { finalize();
                          fail("Unexpected suffix in '%s'.", path); }}
            if (fclose(source) != 0)
            { finalize();
              fail("Cannot close file '%s'.", path); }}}
        count -= 1; strings += 1; }

//  Make the MAIN function. Duh.

      emitMain(); }

//  Write errors. Close the target file. Maybe compile it. Maybe unlink it. The
//  value returned to the shell tells if there were errors.

    writeErrorLines();
    writeErrorMessages();
    if (fclose(stream(target)) != 0)
    { fail("Cannot close file '%s'.", targetPath); }
    if (compiling && isSetEmpty(allErrs))
    { char temp[strlen(compiler) + strlen(targetPath) + 1];
      system(strcat(strcpy(temp, compiler), targetPath)); }
    if (unlinking && unlink(targetPath) != 0)
    { fail("Cannot delete file '%s'.", targetPath); }
    return ! isSetEmpty(allErrs); }

//  Do nothing if there were no source files.

  else
  { return 0; }}
