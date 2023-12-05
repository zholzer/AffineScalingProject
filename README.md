# AffineScalingProject
This Fortran project was conducted for COMP 526 Computational Methods for Scientists taught by Professor Miguel Dumett
Topic: Karmarkar and Affine Scaling Algorithms 
Group members: Zoe Holzer, Luna Huynh, Emma Topolcsik

We implemented a modified version of Karmarkar algorithm - Affine Scaling Algorithm to linear programming problems in Fortran 90. 
These linear programming problems minimize an objective equation bounded by a system of equation
                minimize      c^T x, 
                subject to    Ax ≤ b

This AffineScalingProject folder contains AffineProject.f90 and README.txt 
    AffineProject. f90 contains all code necessary to run the Affine scaling algorithm 
    README.txt (this file) introduces the project and explains how to run the program 

Please follow the following steps to run our Affine Scaling Algorithm program:
   1. To compile the program, in the terminal type this command: gfortran -Wall AffineProject.f90
   2. To run the program, in the terminal type this command: ./a.out
   3. Follow step by step what the program is asking the user to input
    - First, the program asks the user to choose a test case (total of 4 test cases), the user needs to enter a letter Z, E, L, or M
    - Then, the program asks the user to choose a step size between 0 and 1
