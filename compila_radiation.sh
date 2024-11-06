#!/bin/bash
gfortran -Wall -Wconversion -Wextra -fcheck=all -fimplicit-none -fbacktrace -pedantic apri_file.o interpolation.o -o radiation.exe radiation.f90
