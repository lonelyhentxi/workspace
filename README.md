# impls

## Overview

My workspace, containing almost everything. Such as implementation of books, docs, tutorials, reference.

### About Stage Tag

- 🚧 **In Progress**: Working...
- 👌  **Just So So**: It feels average
- 👨‍💻 **Follow the Tutorial**: Does not require much creativity
- ⭐ **Nice Work**: Relative to the ability when done, it is a good project
- 🔗 **Split out**: Split from subfolder out to a new repo because for maintainability or revealing
- 😂 **Embarrassed**: Don't want to continue (although the most part of it is complete or there is only some bugs)
- 🉑 **Chinese Only Now**: Now only have Chinese version
- 🔒 **Private Now**: Temporarily locked because of some early work

## Compile

### 1. [YATC - Yet Another Tiny Compiler](./compile/yatc) ⭐

A `object-ocaml` compiler, using the `top-down + trick` translation approach, with `LLVM` toolchains, and is written by `rust` language.

- Parser
    - Tokenizer
    - AST Parser
- Codegen
    - Optimization
    - IR gen
    - ASM gen
- Driver & Intergration
    - Token Mode
    - AST Mode
    - Interpreter Mode
    - JIT Mode
    - Compiler Mode

### 2. [Minsys Assembler](./compile/minsys-assembler) 👌

This project is a nine-task task of the "Computer Design and Practice" course to implement a `Minisys` assembler. There is a one-to-one correspondence between assembly instructions and instructions, that is, the process of literal translation.

### 3. [Naive Compiler](./compile/naive_compiler) ⭐😂🉑

This project is a C89 Parser implemented by Rust, and it achieves:

- Combinator-based regular engine
- All C89-oriented syntax parser
- Most C89-oriented semantic (grammer) parser

### 4. [GC Book](./comile/gc_book) 🚧🉑

This project includes the naive C++ implementation of a variety of algorithms in the "The Algorithm Section" of the "Algorithms and Implementation" book, and the understanding and notes on various designs in the "The Implement Section".

## Web

### 1. [Minellius](https://github.com/lonelyhentai/minellius)⭐🔗

Minellius, a experimental github data analysis solution, which designed to combine data collection, persistence, integration, analysis, visualization, real-time monitoring, trend tracking, originating from a group project of COMP-3002, 2018 fall, HITSZ.

### 2. [WebAPI](./web/webapi) 👨‍💻🚧

Learn various new web API with MDN:

- custom element
- fetch
- local worker

### 3. [MathML](./web/mathml) 👨‍💻

Learn MathML with MDN.

## Practice

### 1. [Rusty Leetcode](https://github.com/lonelyhentai/rusty-leetcode)⭐🔗

Provide all my leetcode solutions with rust language.

For a small amount of questions without rust version, I will point out their programming language.

### 2. [Codewars Kaleidoscope](https://github.com/lonelyhentai/codewars-kaleidoscope)👌🔗

Multiple programming language solutions for [codewars.com](https://www.codewars.com), including the following languages now:

- C++
- C#
- Dart
- Haskell
- Java
- Python
- Ruby
- Rust
- Javascript
- Typescript

### 3. [Nowcoder Collection](https://github.com/lonelyhentai/nowcoder-collection)⭐🔗

My solution collection of [nowcoder.com](https://www.nowcoder.com), including the following collections now:

- Coding Interviews
- JS Assessment 

### 4. [CSP Challenges](./practice/csp) 😂

My solution collection to CSP.

## ECMAScript

### 1. [CMake Node Starter](https://github.com/lonelyhentai/cmake-node-starter) ⭐🔗

Starter for nodejs addons development with bare cmake instead of node-gyp or cmake-js. 

### 2. [V8 Learn RS](https://github.com/lonelyhentai/v8-learn-rs) 🔒🚧🔗

A trial to learn v8 with rusty_v8. 

### 3. [Javascript Questions](./ecmascript/javascript_questions) 👨‍💻

Test-Driven javascript exercise.

### 4. [Learn Libs](./ecmascript/learn_libs) 👨‍💻

Learn ecmascript & typescript libs with writing code from tutorials:

- learn-angular

## Data & AI

### 1. [COMP3005](./data_ai/comp3005) ⭐🔗

The labs and tasks of COMP3005, HITSZ - Artificial Intelligence. Implement an agent bot in the Pac-Man game.

### 2. [COMP3006](./data_ai/comp3006) 👌

Part of the labs and tasks of COMP3006, HITSZ - Introduction to Machine Learning. Identify security risks from a sequence of system API access records.

### 3. [COMP3007](./data_ai/comp3007) ⭐

Part of the labs and tasks of COMP3007, HITSZ - Pattern recognition. Implement the naive bayes and nearest neighbors algorithm.

### 4. [COMP3009](./data_ai/comp3009) 👌

The labs and tasks of COMP3009, HITSZ - Introduction to Big Data. 

- Exp1: Analyze the characteristics of the given bank data
- Exp2: Preprocess, learn, regression predict and test the given bank data
- Lab1: User traditional statistics to analyze user account data
- Lab2: Crawling house data from the centain site
- Lab3: Use decision tree, naive bayes, ensemble methods and neural network to learn, predict and test on the iris data and the house data crawlered in lab2

### [COMP3021](./data_ai/comp3021) 👌

Part of the labs and tasks of COMP3021, HITSZ - Natural language processing. 

- Task1: Implement some simple preprocessing of NLP
- Task2: Process a given dataset using a relatively complete NLP pipeline
- Task3: Analyze given food reviews using modern tools to identify food safety issues

### [COMP3031](./data_ai/comp3031) 👌

Part of the labs and tasks of COMP3031, HITSZ - WEB Information Processing. Implement LDA by myself, and analyze text with the function.

### [COMP3035](./data_ai/comp3035) 👌

Part of the labs and tasks of COMP3035, HITSZ - Computational Finance. 

- Task1: Crawler Fi-news from the certain fiance news site
- Task2: Forest stock price with traditional machine learning and nerual network

## System

### 1. [Chainbank](https://github.com/lonelyhentai/chainbank)⭐🔗

A tiny bank demo based on lens blockchain. Project of course COMP-3026, 2019 fall, HITSZ.

### 2. [COMP3003](./system/comp3003) 👌

Part of the labs and tasks of COMP3021, HITSZ - Computer Network. Implement ICMP echo and simple web server.

## Database

### 1. [COMP3010](./data_ai/comp3010) 👌😂

The labs and tasks of COMP3010, HITSZ - Database. 

- Task1: Write some SQLs to finish a series of function
- Task2: Execute some SQL with a console app
- Task3: Write a full app with a certain database
- Task4: Write a database engine

## Rust

### 1. [Rustudy](./rust/rustudy) 👨‍💻

Learn Rust with "the RUST Book".

### 2. [Dive into Rust](./rust/dive_into_rust) 👨‍💻

Learn Rust with the book "Dive into Rust".

### 3. [RUST WASM](./rust/rust_wasm) 👌

Demo to use rust to generate wasm.

### 4. [Actix Learn](./rust/actix_learn) 👨‍💻🚧

Learn actix and actix web with the tutorials.

## CPP

### 1. [CMake Emscripten Explore](https://github.com/lonelyhentai/cmake-emscripten-explore) 🔒🚧🔗

Explore emscripten, binaryen, wasi and so on with cmake and c++.

### 2. [CPPIA](./cpp/cppia) 👨‍💻

Learn CPP concurrency with the book "C++ Concurrency in Action".

### 3. [EMCPP](./cpp/emcpp) 👨‍💻

Learn modern CPP with the book "Effective Modern C++".

## Game

### 1. [GE Demo](./game/ge_demo) 🚧

Write a game engine demo.

## Graphics

### 1. [Modern Computer Graphics](https://github.com/lonelyhentai/modern-computer-graphics) 🔒🚧🔗

Learn Graphics with the tigger book and GAMES-101.

### 2. [COMP3018](./graphics/comp3018) 👌

Part of the labs and tasks of COMP3018, HITSZ - Computer Vision. 

- Task1: Learn several part of opencv
- Task2: Identify palm prints with opencv 

### 3. [OpenGL Step by Step](./graphics/opgl_sbs)  👨‍💻

Learn traditional OpenGL step by step. 

## DotNet

### 1. [Essential CS](./dotnet/essential_cs) 👨‍💻

Learn C# with the book "Essential CSharp".

## Haskell

### 1. [Haskell For Fun](./haskell/haskell-for-fun) 👨‍💻

Learn Haskell with the book "Haskell for Fun".

## Mobile

### 1. [Dart Tour](./mobile/dart_tour) 👨‍💻

Learn Dart with the dart tutorial.

### 2. [Flutter in Action](./mobile/flutter_in_action) 👨‍💻🚧

Learn Flutter with the book "Flutter in Action" by writing a flutter app.

## Ruby

### 1. [Ruby Study](./ruby/study) 👨‍💻

Learn Ruby with the ruby tutorials.

### 2. [Rails Study](./ruby/rails_study) 👨‍💻

Learn Rails with the rails tutorials.

## Scala

### 1. [Prog Scala](./scala/prog-scala) 👨‍💻

Learn Scala with the book "Programming in Scala".

## Agda & Idris

### 1. [PLFA - Programming Language Foundations in Agda](./agda-plfa) 👨‍💻🚧

### 2. [SMLSW-idris](./idris/SMLSW-idris) 👨‍💻🚧