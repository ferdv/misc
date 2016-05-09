CS-275 URM Simulator
====================

This is a simple URM simulator, inspired by the _CS-275 Automata and Formal
Language Theory_ course at Swansea University. It accepts the instructions and
syntax as introduced in the lecture notes:

 - `Rn := Rn + 1` (where `n` is an integer >= 0): _register increment_ -
   increment the register `n` by 1
 - `Rn := Rn .- 1`: _register decrement_ - decrement the register `n` by 1 (note
   the dot before `-`)
 - `if Rn = 0 then goto k`: _jump if register is zero_ - if register `n` is 0
   then jump to the instruction at `k` (`k` >= 0)

An example program can be found in `add.urm`. Lines are required to have numeric
labels (as in the lecture notes), although these are ignored at the moment and
the instruction number simply corresponds to the line number.

Usage
-----

The simulator can be used both from GHCi and as a standalone executable. There
are two main modes: "straight run" and "stepped execution". The first mode runs
the program with the given "arguments" (stored in registers) and then prints the
final state of the machine. The second mode executes a single instruction at a
time, printing the state after each step and waiting for the user to press Enter
before continuing.

The state display consists of the PC (program counter), the contents of
registers that have been set explicitly, and the program listing with the
current instruction highlighted.

There is very basic error reporting, such as for catching the error in 
`R0 := R1 + 1`, etc.

Note that unspecified registers are assumed to be 0.

You need GHC in order to compile or run the program:

     $ ghc urm
     [1 of 1] Compiling Main             ( urm.hs, urm.o )
     Linking urm ...

Once the program is compiled, running it without arguments will give you basic
help. 

     $ ./urm
     A simple URM machine simulator based on the CS-275 module.
     Usage: urm [--step] FILE reg0 reg1 ...
     
     Arguments:
         --step         (optional) pause after each step of the machine
     
         FILE           URM program file
         reg0 reg1 ...  initial values for registers 0, 1, etc.
     
     Example:
       urm --step add.urm 12 13

Here is an example of a straight run.

     $ ./urm add.urm 142 243
     - URM State ----------------------------------
     PC: 4
     Registers:
        R0: 385   R1: 0
     Program:
        0:  if R1 = 0 then goto 4
        1:  R1 := R1 .- 1
        2:  R0 := R0 + 1
        3:  if R2 = 0 then goto 0
     -> 

Here is an example of stepped execution:

     $ ./urm --step add.urm 3 2
     - URM State ----------------------------------
     PC: 0
     Registers:
        R0: 3   R1: 2
     Program:
     -> 0:  if R1 = 0 then goto 4
        1:  R1 := R1 .- 1
        2:  R0 := R0 + 1
        3:  if R2 = 0 then goto 0
     
     Press Enter...
     
     - URM State ----------------------------------
     PC: 1
     Registers:
        R0: 3   R1: 2
     Program:
        0:  if R1 = 0 then goto 4
     -> 1:  R1 := R1 .- 1
        2:  R0 := R0 + 1
        3:  if R2 = 0 then goto 0
     
     Press Enter...
     
     - URM State ----------------------------------
     PC: 2
     Registers:
        R0: 3   R1: 1
     Program:
        0:  if R1 = 0 then goto 4
        1:  R1 := R1 .- 1
     -> 2:  R0 := R0 + 1
        3:  if R2 = 0 then goto 0
     
     Press Enter...
     
     - URM State ----------------------------------
     PC: 3
     Registers:
        R0: 4   R1: 1
     Program:
        0:  if R1 = 0 then goto 4
        1:  R1 := R1 .- 1
        2:  R0 := R0 + 1
     -> 3:  if R2 = 0 then goto 0
     
     Press Enter...
     
     - URM State ----------------------------------
     PC: 0
     Registers:
        R0: 4   R1: 1
     Program:
     -> 0:  if R1 = 0 then goto 4
        1:  R1 := R1 .- 1
        2:  R0 := R0 + 1
        3:  if R2 = 0 then goto 0
     
     Press Enter...
     
     ...
     
     - URM State ----------------------------------
     PC: 4
     Registers:
        R0: 5   R1: 0
     Program:
        0:  if R1 = 0 then goto 4
        1:  R1 := R1 .- 1
        2:  R0 := R0 + 1
        3:  if R2 = 0 then goto 0
     -> 
     Press Enter...
     
     Finished.
     

To run the program in GHCi, just load it and use the function `urmRunFile` or
`urmRunFileStepped`, passing it the filename and an array of the initial
registers.

     GHCi, version 7.10.3: http://www.haskell.org/ghc/  :? for help
     Prelude> :l urm.hs
     [1 of 1] Compiling Main             ( urm.hs, interpreted )
     Ok, modules loaded: Main.
     *Main> urmRunFile "add.urm" [142, 243]
     - URM State ----------------------------------
     PC: 4
     Registers:
        R0: 385   R1: 0
     Program:
        0:  if R1 = 0 then goto 4
        1:  R1 := R1 .- 1
        2:  R0 := R0 + 1
        3:  if R2 = 0 then goto 0
     -> 
     ([IfZero 1 4,Pred 1,Succ 0,IfZero 2 0],4,fromList [(0,385),(1,0)])
     *Main>


Source
------

Feel free to investigate the source code, althought it's not very well commented
at the moment. The important function is `urmStep` which takes a URM machine
state and returns the next state - if there is one.

