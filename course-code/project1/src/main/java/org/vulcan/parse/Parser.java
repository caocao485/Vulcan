package org.vulcan.parse;

import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;

import static org.vulcan.parse.Lexer.*;
import static org.vulcan.parse.TokenType.*;

/**
 * Jam Parser class
 * The Parser class should contain a public method AST parse()
 * that returns the abstract syntax tree for the Jam expression
 * in the input stream
 */
public class Parser {

    private final Lexer in;

    private AST result;


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

    public Parser(InputStream in) {
        this(new InputStreamReader(in));
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
        if (result != null) {
            return result;
        }
        result = parseExp();
        if (in.readToken() != null) {
            throw new ParseException("error eof parsing");
        }
        // TODO: eof check
        return result;
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
        Token peek = in.peek();
        //if expression
        if (peek == IF) {
            in.readToken();
            AST test = parseExp();
            if (in.readToken() != THEN) throw new ParseException("missing THEN keyword");
            AST conseq = parseExp();
            if (in.readToken() != ELSE) throw new ParseException("missing ELSE keyword");
            AST alt = parseExp();
            return new If(test, conseq, alt);
        }
        if (peek == LET) {
            in.readToken();
            Def[] defs = parseDefPlus();
            if (in.readToken() != IN) throw new ParseException("missing IN keyword");
            AST body = parseExp();
            return new Let(defs, body);
        }
        if (peek == MAP) {
            in.readToken();
            Variable[] vars = parseIdList();
            if (in.readToken() != TO) throw new ParseException("missing TO keyword");
            AST body = parseExp();
            return new Map(vars, body);
        }
        AST firstTerm = parseTerm(in.readToken());
        if (in.peek() != null && in.peek().getType() == OPERATOR && ((Op) in.peek()).isBinOp()) {
            Op binOp = (Op) in.readToken();
            AST secondTerm = parseTerm(in.readToken());
            return new BinOpApp(binOp, firstTerm, secondTerm);
        }
        // TODO: multiple terms
        return firstTerm;
    }


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


    /**
     * Parses:
     * <expList> ::=  <exp> {( <exp-list> )}？
     * for term method
     */
    private AST[] parseArgs() {
        ArrayList<AST> argsList = new ArrayList<>();
        if (in.peek() == RightParen.ONLY) {
            in.readToken();
            return argsList.toArray(new AST[argsList.size()]);
        }
        argsList.add(parseExp());
        while (in.peek() == Comma.ONLY) {
            in.readToken();
            argsList.add(parseExp());
        }
        Token token = in.readToken();
        if (token != RightParen.ONLY) error(token, "missing right-paren");
        return argsList.toArray(new AST[argsList.size()]);
    }

    /**
     * Parses:
     * <Factor> ::=  (<exp>) | primFun | id
     *
     * @param token first token in input stream to be parsed; remainder in Lexer named in.
     */
    private AST parseFactor(Token token) {
        //TODO:  may be could better
        if (token == LeftParen.ONLY) {
            AST ast = parseExp();
            if (in.readToken() != RightParen.ONLY) error(token, "missing right-paren");
            return ast;
        }
        if (token.getType() == PRIM_FUN || token.getType() == VAR) {
            return (Term) token;
        }
        error(token, "error factor expression");
        return null;

    }

    /**
     * Parses:
     * <defPlus> ::=  <def> {<def> }？
     * for let method
     */
    private Def[] parseDefPlus() {
        ArrayList<Def> defList = new ArrayList<>();
        defList.add(parseDef());
        //let in前探
        while (in.peek() != IN) {
            defList.add(parseDef());
        }
        return defList.toArray(new Def[defList.size()]);
    }

    /**
     * Parses:
     * <def> ::=  <id> := <exp> ;
     */
    private Def parseDef() {
        Token lhs = in.readToken();
        if (lhs.getType() != VAR) error(lhs, "error definition expression");
        Token keywordBind = in.readToken();
        if (keywordBind != BIND) error(keywordBind, "missing bind keyword");
        AST rhs = parseExp();
        Token semicolon = in.readToken();
        if (semicolon != SemiColon.ONLY) error(semicolon, "missing semicolon ';'");
        return new Def((Variable) lhs, rhs);
    }

    /**
     * Parses:
     * <idList> ::=  <id> {,<id>}?
     * for map method
     */
    private Variable[] parseIdList() {
        ArrayList<Variable> idList = new ArrayList<>();
        if (in.peek() == TO) {
            return idList.toArray(new Variable[idList.size()]);
        }
        Token id = in.readToken();
        if (id.getType() != VAR) error(id, "error Variable type");
        idList.add((Variable) id);
        while (in.peek() == Comma.ONLY) {
            in.readToken();
            id = in.readToken();
            if (id.getType() != VAR) error(id, "error Variable type");
            idList.add((Variable) id);
        }
        return idList.toArray(new Variable[idList.size()]);
    }


    private void error(Token token, String hintString) {
        throw new ParseException(token.toString() + ": " + hintString);
    }

    /**
     * System.out.println will be used to print the output String
     * of org.vulcan.parse.Parse.parse 's return AST s toString method
     */
    public static void main(String[] args) {
        if (args.length == 0) {
            Scanner s = new Scanner(System.in);
            try {
                System.out.println("out: " + new Parser(new StringReader(s.nextLine())).parse());
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            try {
                System.out.println("out: " + new Parser(args[0]).parse());
            } catch (Exception e) {
                //
            }
        }

    }


}
