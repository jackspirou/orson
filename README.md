# Orson [![Build Status](https://travis-ci.org/jackspirou/orson.svg?branch=master)](https://travis-ci.org/jackspirou/orson)

**Orson is a small, general purpose programming language, designed to be used by _individuals_ and _small groups_.**

The best hope a language inventor has for the survival of his or her project is to find a group of people who will use it, and then hand it over and let them ruin its perfection. :)

## 0. Introduction.

Orson [MOE 2014] is a small, general purpose programming language, designed to be used by individuals and small groups. It emphasizes efficiency, expressiveness, and extensibility, allowing access to low level representations of objects like that of the programming language C [KER 1988]. Its name is not an acronym, so it doesn’t appear in capital letters. Orson is not named after any person, place, or thing.

Orson programs are made up of expressions that work in two phases, called transformation and execution. During transformation, Orson applies forms to produce new expressions. Forms are similar to macros, but are written in a lexically scoped applicative language that can perform arbitrary computations. During execution, Orson evaluates the expressions that were produced during transformation.

Orson is currently implemented so that transformation occurs at compile time, and execution occurs at run time. This allows writing with forms in an abstract and general way, while still producing efficient programs. For example, forms can be used as inline substitutes for some procedures. Most operators are implemented as forms, so they can be redefined. Abstract data types can be implemented as forms that take types as their arguments and return types as their results. Control structures can be implemented as higher-order forms that take forms as arguments and return forms as their results.

Orson was developed on Intel x86 computers running Debian and Ubuntu GNU/Linux, so it should work on similar systems. The Orson compiler itself is written in GNU C [STA 2008]. It translates Orson source programs to equivalent GNU C programs, and then invokes the GNU C compiler GCC to compile them. Orson is distributed with many example programs, most of which are themselves written in Orson. These were written to test the Orson language and its compiler.

## 1. Licensing.

Orson and its accompanying programs are free software: you can redistribute them and/or modify them under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

Orson and its accompanying programs are distributed in the hope that they will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with Orson and its accompanying programs. If not, see <http://www.gnu.org/licenses/>.

## 2. Contents.

The following directories contain the Orson compiler and its example programs. Follow the links for more information.

apps/

    Various small applications.

bracy/

    Simple document compiler that produces HTML files.

eg/

    Generate example strings from context-free grammars.

library/

    Orson library.

licenses/  

    GNU General Public Licenses.

orson/

    Orson compiler.

ox/

    Cross-reference generator for Orson source programs.

scam/

    Interpreter for a toy Scheme-like language.

tlh/

    Make two-level perfect hash functions for string keys.

These files contain documentation and other supporting material.

LICENSE

    Short statement about licensing.

README

    Short text version of Readme.html.

Readme.by

    Bracy source for Readme.html.

Readme.html

    What you’re reading now.

Test

    Compile Orson source files for debugging.

tech/

    Orson technical reports.

Orson’s example programs and documentation contain special characters. Without proper rendering support, you may see question marks, boxes, or other symbols in place of these characters.

## 3. Makefiles.

Orson and many of its accompanying programs are installed by Unix makefiles [STA 2010]. The next few sections describe how these makefiles compile, install, and uninstall programs. I assume only a minimal knowledge of Unix. Readers who are familiar with Unix can skip section 3.1 and skim the rest.

## 3.1. Root.

Unix systems have a special user (sometimes called the superuser) whose name is root. The root user can do almost anything without the system objecting. If you’re root, you can read any file, write any file, delete any file, and make files in any directory. You can become root if you have the right password. This is potentially dangerous, so you shouldn’t become root unless you must, and even then you should stay root for as short a time as you can.

You must become root to install Orson and the programs that accompany it. This is because these programs must be placed in directories that you ordinarily aren’t allowed to make files in. If you can’t become root for some reason, or you don’t want to, then you can still experiment with Orson and its accompanying programs without installing them. I’ll explain how to do that in section 3.4.

There are at least two different ways to become root. On some systems, you become root only for the duration of a single command. Suppose you want to execute the command tenhut as root. Then you type sudo tenhut to the shell, and type a password. (I guess sudo stands for su do, or maybe it’s a pun on the word pseudo.) The command sudo doesn’t explicitly put you into a new shell, it just executes tenhut as if you’re root.

On other systems, you type su to the shell, then type a password. (I guess su stands for superuser.) On others, you type su root, or sudo su, or sudo su root. This puts you into a new shell where you can type commands that are executed as if you’re root. When you’re done, you exit the new shell by typing the command exit or by typing the character control-D.

The commands sudo and su may remember the root password for a short time, so you don’t have to keep typing it if you do several sudo’s or su’s one after the other. You can get more information about sudo and su by typing the commands man sudo and man su to the shell.

## 3.2. Installing.

To install a program, first cd to the directory that contains the source code for it. There should be a file called Makefile in that directory. It contains Unix commands that can compile, install, and uninstall the program. It also contains comments that describe exactly what it will do to your system when you run it. Read Makefile, and if you’ve satisfied yourself that it won’t do anything bad, then execute the command make install as root. (See section 3.1 if you don’t know how to do that.) The make command will write a list of the things it’s doing to install the program.

After the program is installed, you can run it. Just type its name to the shell, maybe followed by some options. If you don’t know what its options are, you can find out by typing the command man p, where p is the name of the program: you should see a manual page for p on the screen. If you can’t get the manual page, then something has gone wrong with the installation. If you can’t run the program, then something has gone wrong as well.

## 3.3. Uninstalling.

If something did go wrong, or you decide you didn’t want to install the program after all, then you can uninstall it. Just cd to the directory that contains the program you installed, and execute the command make uninstall as root. (See section 3.1 if you don’t know how to do that.) As before, the make command will print a list of things it’s doing to uninstall the program. Once you are no longer root, everything that make install did should be undone.

## 3.4. Rootlessness.

What if you can’t become root? Maybe you’re a student working in a school lab, and you don’t have the root password for the computer you’re using. You can still compile and run these programs. Just cd to the directory that contains the program you want, and type the command make. As before, the make command will print a list of things it’s doing to compile the program. It will then leave the compiled program in the directory.
Now, if the program you’ve just compiled is named p, then you can run it by typing the command ./p to the shell, maybe followed by some options. (That’s dot slash pee.) If you don’t know what its options are, you can read its manual page by typing the command man -l p.1 to the shell. (That’s man space dash ell space pee dot one, so don’t confuse a lower case l with the digit 1.) You should see the manual page for the program p.

## References

[KER 1988]  

Brian W. Kernighan, Dennis M. Ritchie. The C Programming Language. Second Edition. Prentice-Hall. Upper Saddle River, New Jersey. 1988.





[MOE 2014]  

James B. Moen. ‘‘Revised⁻¹ Report on the Algorithmic Language Orson.’’ Unpublished technical report. 2014.





[OKR 2009]  

Arika Okrent. In the Land of Invented Languages. Spiegel and Grau. New York, New York. 2009. Page 262.





[STA 2008]  

Richard M. Stallman, the GCC Developer Community. ‘‘Using and Porting the GNU Compiler Collection.’’ Free Software Foundation. Boston, Massachusetts. 2008.





[STA 2010]  

Richard M. Stallman, Ronald McGrath, Paul D. Smith. ‘‘GNU Make: A Program for Directing Recompilation.’’ Edition 0.71. Free Software Foundation. Boston, Massachusetts. July 19, 2010.
