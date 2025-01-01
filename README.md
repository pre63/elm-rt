# ElmRT Compiler in Haskell

### **1. Lexical Analysis (Lexer)**
The lexical analysis stage involves converting the source code into a stream of tokens. This project uses `alex`, a lexical analyzer generator for Haskell, to define token patterns and generate the lexer.

#### **Token Definitions**
`alex` supports regular expressions for defining tokens, which are mapped to the `Token` data type. The token definitions correspond to keywords, identifiers, literals, and operators from the ElmRT language.


### **2. Syntax Analysis (Parser)**
The syntax analysis stage involves defining the grammar of the ElmRT language using `Happy`, a parser generator for Haskell. `Happy` uses context-free grammars to describe the structure of the language and generates a parser.

#### **Grammar Specification**
The grammar defines constructs such as expressions, declarations, and module definitions, mapping them to the corresponding abstract syntax tree (AST) nodes. For example, the `ModuleDecl` type in the `Types` module represents a module declaration.

### **3. Semantic Analysis**
Semantic analysis ensures the program's correctness beyond syntax by validating the types and scope of variables.

#### **Type Checking**
The strong static typing of ElmRT is implemented using custom type inference and checking logic. Haskell's algebraic data types (ADTs) and pattern matching simplify the implementation of ElmRT's type system.

#### **Scope Resolution**
Custom functions manage variable and function scopes, ensuring identifiers are defined before use. This is critical for resolving nested and imported scopes in ElmRT.

### **4. Intermediate Code Generation**
Intermediate representations (IR) bridge the gap between high-level syntax and machine code.

#### **LLVM IR**
`llvm-hs` is used to generate LLVM IR, enabling low-level code generation and optimizations. Alternatively, a custom IR can be defined using Haskell's ADTs for simpler code generation.

### **5. Optimization**
The optimization stage improves performance and reduces code size.

#### **Custom Optimizations**
Custom routines like constant folding, dead code elimination, and loop unrolling are implemented directly in Haskell.

#### **LLVM Optimizations**
If LLVM is used for IR, its built-in optimization passes can be applied for highly efficient transformations.

### **6. Code Generation**
The final stage generates executable code or source code in another language.

#### **Python Code Generation**
Python code is generated using string manipulation in Haskell. A template-based approach ensures consistency and readability.

#### **C Code Generation**
If targeting C, libraries like `llvm-hs` or direct string manipulation are used to generate C source code from the IR.

### **7. Error Handling**
Error handling provides meaningful feedback during lexing, parsing, and semantic analysis.

#### **Error Reporting**
Detailed error messages are constructed using Haskell's `Either` type. Libraries like `pretty` format errors for readability.

#### **Type Errors**
Type errors are caught and reported during semantic analysis, leveraging Haskell's strong type system for early detection.
