

	# Compiler and tools
GHC = ghc
ALEX = alex
HAPPY = happy

# Sources
LEXER_SRC = Grammar/Lexer.x
PARSER_SRC = Grammar/Parser.y
MAIN_SRC = Main.hs

# Generated files
LEXER_GEN = Gen/Lexer.hs
PARSER_GEN = Gen/Parser.hs

# Example input file
EXAMPLE_INPUT = Example/example.elm

# Executable
EXECUTABLE = parser

# Build steps
all: $(EXECUTABLE)

clean-target:
	#@rm -rf Gen

$(LEXER_GEN): $(LEXER_SRC)
	@mkdir -p Gen
	$(ALEX) $(LEXER_SRC) -o $(LEXER_GEN)

$(PARSER_GEN): $(PARSER_SRC)
	@mkdir -p Gen
	$(HAPPY) $(PARSER_SRC) -i -o $(PARSER_GEN)

$(EXECUTABLE): clean-target $(MAIN_SRC) $(LEXER_GEN) $(PARSER_GEN)
	$(GHC) -o $(EXECUTABLE) $(MAIN_SRC) $(LEXER_GEN) $(PARSER_GEN)

# Run the parser with the example input
run: $(EXECUTABLE)
	./$(EXECUTABLE) < $(EXAMPLE_INPUT)

# Clean build artifacts
clean:
	rm -f $(LEXER_GEN) $(PARSER_GEN) $(EXECUTABLE) *.o *.hi


install:
	brew install ghc cabal-install
	cabal install happy
	cabal install alex
