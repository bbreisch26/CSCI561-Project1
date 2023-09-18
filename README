CSCI 561 --- Project 1
======================

The purpose of this project is to learn about:

1. Algorithms for regular language manipulation.

2. The application of regular languages to:
   a. text processing,
   b. the control of discrete event systems.

INSTRUCTIONS
============

Setup
-----

1. Form teams of 2-4 students.  You may create a corresponding github
   team in the course organization.

2. From the github.com interface for this repo, click `Use this
   template` to import the starter code.  Create a new, private repo
   for your team in the course organization.  Name your repo
   `TYY-project-1-TEAMNAME`, where,
   - `T` is one charcter for the term: `f` Fall or `s` for Spring,
   - `YY` is the two digit year, e.g. `19` for 2019,
   - `TEAMNAME` is a name for your team.

3. Clone the repo.

        git clone git@github.com:mines-csci561/TYY-project-1-TEAMNAME

5. Add all team members' names and CWIDs to the `AUTHORS` file.

6. Double-check that (1) your team's github repo is correctly named
   and (2) only your teammates (and the instructor/TA) can access your
   team's github repo.

7. Complete the function stubs in project-1.lisp.

8. Answer the questions in project-1-report.tex (Use `pdflatex` to
   compile the PDF).

9. Push your code changes to your group repo in the course
   organization before the project deadline.  At the project deadline,
   the TA/instructor will download for grading all repositories in the
   course organization beginning with the prefix `TYY-project-1-` as
   described above

10. Submit your report as a PDF on canvas. Include all group members'
    names in the submitted PDF (only one group member needs to
    submit).

Code
----

The code portion of the project is divided into four parts,
corresponding to different lecture segments.  For each function to
complete, the expected total length of the function (starter code plus
code you will write) is listed below.

- Part 0, Introduction.  Complete the following functions:
  - DFA-SIMULATE, Expected length: about 15 lines

- Part 1, Subset construction. Complete the following functions:
  - E-CLOSURE, Expected length: about 10 lines
  - MOVE-E-CLOSURE, Expected length: less than 10 lines
  - NFA->DFA, Expected length: about 30 lines
  - NFA-SIMULATE, Expected length: about 15 lines

- Part 2, Regular Expressions. Complete the following functions:
  - SIMPLIFY-REGEX, Expected length: about 25 lines
  - FA-UNION, Expected length: about 20 lines
  - FA-REPEAT, Expected length: about 15 lines
  - REGEX->NFA, Expected length: about 30 lines

- Part 3, Regular Language Properties, Complete the following
  functions:
  - FA-EMPTY, Expected length: about 15 lines
  - DFA-MINIMIZE, Expected length: about 30 lines
  - DFA-INTERSECTION, Expected length: about 50 lines
  - DFA-EQUIVALENT, Expected length: about 10 lines

Ensure that you push your changes to your team's repository in the
course organization before the deadline for each part.

Report
------

1. Answer the report questions in project-1-report.tex.

2. Submit your report as a PDF on canvas. Include all group members'
   names in the submitted PDF (only one group member needs to submit).

Grading
=======

The grading script will download your repository from the github
course organization at the deadline.  Your repository must (1) be
located in the course organization and (2) have the correct prefix for
the grading script to find it.

Your code will be run for grading using (approximately) the following
procedure:

    sbcl --load project-1.lisp --load testcases.lisp

Your code needs to produce the correct result to receive credit for
each test case. Make sure your code loads cleanly without compilation
errors, extra I/O, or other side effects.

"Correct" for REGEX->NFA, NFA->DFA, and DFA-INTERSECTION means that
the language defined by the returned finite automaton is equivalent
and, for NFA->DFA, that the result is a DFA.  "Correct" for
DFA-MINIMIZE means that the language defined by the returned DFA is
equivalent and the state is minimized.

Please consider and appropriately handle potential edge cases in the
input.
