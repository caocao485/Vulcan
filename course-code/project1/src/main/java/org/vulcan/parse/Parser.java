package org.vulcan.parse;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;

/**
 * Jam Parser class
 * The Parser class should contain a public method AST parse()
 * that returns the abstract syntax tree for the Jam expression
 * in the input stream
 */
public class Parser {

    private Lexer in;

    //todo
    public Parser(Lexer i) {
        in = i;
        initParser();
    }

    Parser(Reader inputStream) {
        this(new Lexer(inputStream));
    }

    Parser(String fileName) throws IOException {
        this(new FileReader(fileName));
    }

    Lexer lexer() {
        return in;
    }

    private void initParser() {
    }

    /**
     * Parse the program text in the lexer bound to 'in' and returns the corresponding AST.
     *
     * @throws ParseException if a syntax error is encountered(include lexical errors).
     */
    public AST parse() throws ParseException {
        return null;
    }

    /**
     * Parses:
     * <exp> ::= if <exp> then <exp> else <exp>
     * | let <prop-def-list> in <exp>
     * | map <id-list> to <exp>
     * | <binary-exp>
     * <binary-exp> ::= <term> {<biop> <exp>}*
     *
     * @return the corresponding AST.
     */
    private AST parseExp() {
        return null;
    }

    /*find it helpful to define separate parse methods for <binary-exp>, if expressions, and map expressions.
     * This is a stylistic choice. */


    /**
     * Parses:
     * <term>     ::= <unop> <term> | <constant> | <factor> {( <exp-list> )}
     * <constant> ::= <null> | <int> | <bool>
     *
     * @param token first token in input stream to be parsed; remainder in Lexer named in.
     */
    private AST parseTerm(Token token) {

        if (token instanceof Op) {
            Op op = (Op) token;
            if (!op.isUnOp()) error(op, "unary operator");
            return new UnOpApp(op, parseTerm(in.readToken()));
        }

        if (token instanceof Constant) return (Constant) token;
        AST factor = parseFactor(token);
        Token next = in.peek();
        if (next == LeftParen.ONLY) {
            in.readToken();  // remove next from input stream
            AST[] exps = parseArgs();  // including closing paren
            return new App(factor, exps);
        }
        return factor;
    }

    private AST[] parseArgs() {
        return null;
    }

    private AST parseFactor(Token token) {
        return null;
    }


    private void error(Op op, String operatorString) {
    }


    /**
     * System.out.println will be used to print the output String
     * of org.vulcan.parse.Parse.parse 's return AST s toString method
     */
    public static void main(String[] args) {

    }


}
