# Manipulating Regular and Context-Free Languages

Professor: Jerusa Marchi

Student: Henrique da Cunha Buss

## Roadmap

### [x] First Submission (items 1-7, 05/11)

- [x] AFs
  - [x] Display AF
  - [x] Parse AF
  - [x] Add AF to history
  - [x] Read AF from file
  - [x] Display AFND
  - [x] Parse AFND
  - [x] Add AFND to history
  - [x] Convert AFND (no Epsilon) to AFD
  - [x] Convert AFND (with Epsilon) to AFD
  - [x] Recognize sentences with AFDs
  - [x] Recognize sentences with AFs
  - [x] AFD Union
  - [x] AFD Intersection
  - [x] Minimize AFD
    - [x] Filter reachable states
    - [x] Filter alive states
    - [x] Apply classes algorithm
  - [x] Convert AFD to GR
- [x] GR
  - [x] Display GR
  - [x] Parse GR
  - [x] Add GR to history
  - [x] Add GR from file
  - [x] Convert GR to AFND
- [x] ER
  - [x] Display ER
  - [x] Parse ER
  - [x] Add ER to history
  - [x] Add ER from file
  - [x] Convert ER to AFD

### [ ] Second Submission (items 8-12, 02/12)

- [x] GLC
  - [x] Model GLC
  - [x] Display GLC
  - [x] Parse GLC
  - [x] Save GLC
  - [x] Add GLC to history
  - [x] Add GLC from file
  - [x] Remove Epsilon
  - [x] Eliminate left recursion
    - [x] Direct
    - [x] Indirect
    - [x] All
  - [x] Factor GLC (Remove non-determinism)
    - [x] Direct
    - [x] Indirect
    - [x] All
  - [x] Transform to normal Chomsky
    - [x] Remove epsilon productions
    - [x] Remove unitary productions
    - [x] Remove useless symbols
- [ ] AP
  - TODO

## Tasks

Create an application, with a GUI, to manipulate Finite Automata, Regular
Grammars, Regular Expressions, Context-Free Grammars and Stack Automata.

- First delivery

  1. [x] Reading, writing and editing AF, GR and ER
  2. [x] Converting AFND (with and without epsilon) to AFD
  3. [x] Converting AFD to GR and GR to AFND
  4. [x] Recognizing sentences in AF
  5. [x] Minimizing AFD
  6. [x] AFD union and intersection
  7. [x] Converting ER to AFD (using syntactic tree-based algorithm)

- Second delivery

  1. [x] Reading, writing and editing GLC
  2. [ ] Transforming GLC to a GLC in normal Chomsky form
  3. [x] Left-recursion elimination
  4. [x] Factoration
  5. [ ] Recognizing sentences in AP (implementing one of the analysis tables)

### Notes

- AFs can be presented via transition tables
- All AFs (intermediate or final) must be reutilizable (capable of being edited)

## Submitting

### Due dates

- 05/11 - First part - itens 1-7
- 02/12 - Second part - itens 8-12

### Instructions

The submition must be via moodle in a file \<Name>.zip, with two subdirectories:
Aplicação and Testes.

Test files must be in the format \<teste\<i>\<funcionalidade>.ext> e.g.
teste1AfndAfd.jff, and must follow the [specified format](##Format)

There must be a README (txt or pdf) file in the main directory with a header
(institution, department, name and date) and information about the source code
(used language, modeling details, data structures used, etc.) and details about
the use of the application.

Auxiliary libraries, that are not specificlly about automata, regex or grammars,
can be used aiming to clean the code (e.g. for IO). All source files must have
a header and be well commented.

## Grading

The project will be graded by the algorithm correctness, usability and
robustness (70%). Things like code legibility and source code organization
(20%) will also be considered (subjective). Some of the final grade will come
from the complete project presentation, which will happen between 03/12 and
10/12 during the class times. Scheduling will be done via moodle.

## Format

### AF

Files about AF must be like:

    number of states
    initial state
    final states
    alphabet
    transitions (one by line)

#### AF Examples

AFD:

    5
    0
    1,2
    a,b
    0,a,1
    0,b,2
    1,a,1
    1,b,3
    2,a,4
    2,b,2
    3,a,1
    3,b,3
    4,a,4
    4,b,2

AFND without epsilon-transitions:

    4
    0
    3
    a,b
    0,a,0-1
    0,b,0
    1,b,2
    2,b,3

AFND with epsilon-transitions (represented by &):

    4
    0
    3
    a,b,&
    0,&,1-2
    1,a,1
    1,b,2
    2,a,1
    2,&,3
    3,b,3

### ER

Files about ER must be like

    def-reg1: ER1
    def-reg2: ER2
    ...
    def-regn: ERn

ERs must accept groups like \[a-zA-z] and \[0-9] and usual operators like \*, +, ?

#### ER Examples

Example 1:

    digit: [0-9]
    letter: [a-zA-Z]
    id: letter(letter | digit)*

Example 2:

    er: a?(a | b)+

### Grammars

GR and GLC files must use the usual pattern used in class with & representing
epsilon:

### Grammars Examples

Example GR:

    S -> aA | a
    A -> bA | a

Example GLC:

    S -> aSb | &

## Symbol table

| Symbol | Meaning                                                                 |
| :----- | :---------------------------------------------------------------------- |
| AF     | Finite Automaton (Autômato Finito)                                      |
| GR     | Regular Grammar (Gramática Regular)                                     |
| ER     | Regular Expression (Expressão Regular)                                  |
| AFD    | Deterministic Finite Automaton (Autômato Finito Determinístico)         |
| AFND   | Non-deterministic Finite Automaton (Autômato Finito Não Determinístico) |
| GLC    | Context-Free Grammar (Gramática Livre de Contexto)                      |
| AP     | Stack/Push Automaton (Autômato de Pilha)                                |
