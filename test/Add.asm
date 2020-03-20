// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/add/Add.asm

// Computes R0 = 2 + 3  (R0 refers to RAM[0])

@2  // load 2 into A
D=A // load 2 stored in A to D

@3  // load 3 into A
D=D+A// add 3 to 2

@0  // load 0 into A
M=D // set R0 to 2 + 3
