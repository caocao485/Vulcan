package org.vulcan.parse;

import org.vulcan.TypeChecker.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Scanner;



import static org.vulcan.parse.Lexer.*;
import static org.vulcan.parse.TokenType.*;

/**
 * Jam Parser class
 * The Parser class should contain a public method Ast parse()
 * that returns the abstract syntax tree for the Jam expression
 * in the input stream
 * @author Think
 */
public class Parser {

    private final Lexer in;

    private Ast result;


    public Parser(final Lexer i) {
        this.in = i;
    }

    public Parser(final Reader inputStream) {
        this(new Lexer(inputStream));
    }

    public Parser(final String fileName) throws IOException {
        this(new FileReader(fileName));
    }

    public Parser(final InputStream in) {
        this(new InputStreamReader(in));
    }



    /**
     * Parse the program text in the lexer bound to 'in' and returns the corresponding Ast.
     *
     * @throws ParseException if a syntax error is encountered(include lexical errors).
     */
    public Ast parse() throws ParseException {
        if (this.result != null) {
            return this.result;
        }
        this.result = this.parseExp();
        if (this.in.readToken() != null) {
            throw new ParseException("error eof parsing");
        }
        // TODO: eof check
        return this.result;
    }

    /**
     * Parses:
     * <exp> ::= if <exp> then <exp> else <exp>
     * | let <prop-def-list> in <exp>
     * | map <id-list> to <exp>
     * | <binary-exp>
     * <binary-exp> ::= <term> {<biop> <exp>}*
     *
     * @return the corresponding Ast.
     */

    private Ast parseExp() {
        final Token peek = this.in.peek();
        //if expression
        if (peek == IF) {
            this.in.readToken();
            final Ast test = this.parseExp();
            if (this.in.readToken() != THEN) {
                throw new ParseException("missing THEN keyword");
            }
            final Ast conseq = this.parseExp();
            if (this.in.readToken() != ELSE) {
                throw new ParseException("missing ELSE keyword");
            }
            final Ast alt = this.parseExp();
            return new If(test, conseq, alt);
        }
        if (peek == LET) {
            this.in.readToken();
            final Def[] defs = this.parseDefPlus();
            if (this.in.readToken() != IN) {
                throw new ParseException("missing IN keyword");
            }
            final Ast body = this.parseExp();
            return new Let(defs, body);
        }
        if (peek == MAP) {
            this.in.readToken();
            final ArrayList<JType> jTypeList = new ArrayList<>();
            final Variable[] vars = this.parseIdList(jTypeList);
            if (this.in.readToken() != TO) {
                throw new ParseException("missing TO keyword");
            }
            final Ast body = this.parseExp();
            return new TypedMap(vars, jTypeList.toArray(new JType[jTypeList.size()]),body);
        }
        if(peek == LeftBrace.ONLY){
            this.in.readToken();
            return this.parseBlock();
        }
        Ast currentAst = this.parseTerm(this.in.readToken());
        while (this.in.peek() != null && this.in.peek().getType() == OPERATOR && ((Op) this.in.peek()).isBinOp()) {
            final Op binOp = (Op) this.in.readToken();
            if (this.in.peek() == null) {
                throw new ParseException("miss second term");
            }
            final Ast secondTerm = this.parseTerm(this.in.readToken());
            currentAst = new BinOpApp(binOp, currentAst, secondTerm);
        }
        // TODO: multiple terms
        return currentAst;
    }

    private Block parseBlock() {
        final ArrayList<Ast> ExpList = new ArrayList<>();
        if (this.in.peek() == RightBrace.ONLY) {
            throw new ParseException("empty block");
        }
        ExpList.add(this.parseExp());
        while (this.in.peek() == SemiColon.ONLY) {
            this.in.readToken();
            ExpList.add(this.parseExp());
        }
        final Token token = this.in.readToken();
        if (token != RightBrace.ONLY) {
            this.error(token, "missing right-brace");
        }
        return new Block(ExpList.toArray(new Ast[ExpList.size()]));
    }


    /**
     * Parses:
     * <term>     ::= <unop> <term> | <constant> | <factor> {( <exp-list> )}
     * <constant> ::= <null> | <int> | <bool>
     *
     * @param token first token in input stream to be parsed; remainder in Lexer.java named in.
     */
    private Ast parseTerm(final Token token) {

        if (token instanceof Op) {
            final Op op = (Op) token;
            if (!op.isUnOp()) {
                this.error(op, "unary operator");
            }
            return new UnOpApp(op, this.parseTerm(this.in.readToken()));
        }
        if(token instanceof NullConstant){
            if(this.in.readToken() != TYPE_BIND){
                this.error(token, "miss TYPE_BIND ");
            }
            JType jType = parseJType();
            return new NullConstant(jType);
        }
        if (token instanceof Constant) {
            return (Constant) token;
        }
        final Ast factor = this.parseFactor(token);
        final Token next = this.in.peek();
        if(factor instanceof PrimFun){
            if(this.in.readToken() !=LeftParen.ONLY) {
                this.error((PrimFun)factor, "primitive function are not allow in this position");
            };
            final Ast[] exps = this.parseArgs();  // including closing paren
            return new App(factor, exps);
        }
        if (next == LeftParen.ONLY) {
            this.in.readToken();  // remove next from input stream
            final Ast[] exps = this.parseArgs();  // including closing paren
            return new App(factor, exps);
        }
        return factor;
    }


    /**
     * Parses:
     * <expList> ::=  <exp> {( <exp-list> )}?
     * for term method
     */
    private Ast[] parseArgs() {
        final ArrayList<Ast> argsList = new ArrayList<>();
        if (this.in.peek() == RightParen.ONLY) {
            this.in.readToken();
            return argsList.toArray(new Ast[argsList.size()]);
        }
        argsList.add(this.parseExp());
        while (this.in.peek() == Comma.ONLY) {
            this.in.readToken();
            argsList.add(this.parseExp());
        }
        final Token token = this.in.readToken();
        if (token != RightParen.ONLY) {
            this.error(token, "missing right-paren");
        }
        return argsList.toArray(new Ast[argsList.size()]);
    }

    /**
     * Parses:
     * <Factor> ::=  (<exp>) | primFun | id
     *
     * @param token first token in input stream to be parsed; remainder in Lexer.java named in.
     */
    private Ast parseFactor(final Token token) {
        //TODO:  may be could better
        if (token == LeftParen.ONLY) {
            final Ast ast = this.parseExp();
            if (this.in.readToken() != RightParen.ONLY) {
                this.error(token, "missing right-paren");
            }
            return ast;
        }
        if (token.getType() == PRIM_FUN || token.getType() == VAR) {
            return (Term) token;
        }
        this.error(token, "error factor expression");
        return null;

    }

    /**
     * Parses:
     * <defPlus> ::=  <def> {<def> }?
     * for let method
     */
    private Def[] parseDefPlus() {
        final ArrayList<Def> defList = new ArrayList<>();
        defList.add(this.parseDef());
        //let in前探
        while (this.in.peek() != IN) {
            defList.add(this.parseDef());
        }
        return defList.toArray(new Def[defList.size()]);
    }

    /**
     * Parses:
     * <def> ::=  <id> := <exp> ;
     */
    private Def parseDef() {
        final Token lhs = this.in.readToken();
        if (lhs.getType() != VAR) {
            this.error(lhs, "error definition expression");
        }

        if(this.in.readToken() != TYPE_BIND){
            this.error(lhs, "miss TYPE_BIND ");
        }
        JType jType = parseJType();
        final Token keywordBind = this.in.readToken();
        if (keywordBind != BIND) {
            this.error(keywordBind, "missing bind keyword");
        }
        final Ast rhs = this.parseExp();
        final Token semicolon = this.in.readToken();
        if (semicolon != SemiColon.ONLY) {
            this.error(semicolon, "missing semicolon ';'");
        }
        return new TypedDef((Variable)lhs,jType, rhs);
    }

    /**
     * Parses:
     * <idList> ::=  <id> {,<id>}?
     * for map method
     */
    private Variable[] parseIdList(ArrayList<JType> jTypeList) {
        final ArrayList<Variable> idList = new ArrayList<>();
        //map to 前探
        if (this.in.peek() == TO) {
            return idList.toArray(new Variable[idList.size()]);
        }
        Token id = this.in.readToken();
        if (id.getType() != VAR) {
            this.error(id, "error Variable type");
        }
        idList.add((Variable) id);
        if(this.in.readToken() != TYPE_BIND){
            this.error(id, "miss TYPE_BIND ");
        }
        jTypeList.add(parseJType());
        while (this.in.peek() == Comma.ONLY) {
            this.in.readToken();
            id = this.in.readToken();
            if (id.getType() != VAR) {
                this.error(id, "error Variable type");
            }
            idList.add((Variable) id);
            if(this.in.readToken() != TYPE_BIND){
                this.error(id, "miss TYPE_BIND ");
            }
            jTypeList.add(parseJType());
        }
        return idList.toArray(new Variable[idList.size()]);
    }



    private JType parseJType(){
        Token readToken = this.in.readToken();
        if(readToken == TYPE_UNIT){
            return UnitT.JUNIT_TYPE;
        }
        if (readToken == TYPE_INT){
            return intT.JINT_TYPE;
        }
        if (readToken == TYPE_BOOL){
            return boolT.JBOOL_TYPE;
        }
        if(readToken == TYPE_LIST){
            JType type = parseJType();
            return new ListT(type);
        }
        if(readToken.getType() == OPERATOR && "ref".equals(((Op)readToken).getSymbol())){
            JType type = parseJType();
            return new RefT(type);
        }
        if(readToken == LeftParen.ONLY){
            return parseTypeArrow();
        }
        error(readToken ," error JType");
        return null;
    }


    private ArrowT parseTypeArrow(){
        Token peekToken = this.in.peek();
        if (peekToken == ARROW){
            this.in.readToken();
            JType rangeType = parseJType();
            if(this.in.readToken() != RightParen.ONLY) {
                this.error(peekToken, "missing right-paren");
            }
            return new ArrowT(new JType[0],rangeType);
        }
        final ArrayList<JType> jTypeList = new ArrayList<>();
        jTypeList.add(parseJType());
        while (this.in.peek() == Comma.ONLY){
            this.in.readToken();
            jTypeList.add(parseJType());
        }
        if(this.in.readToken() != ARROW){
            error(this.in.peek(), " miss arrow");
        }
        JType rangeType = parseJType();
        if(this.in.readToken() != RightParen.ONLY) {
            this.error(peekToken, "missing right-paren");
        }
        return new ArrowT(jTypeList.toArray(new JType[jTypeList.size()]),rangeType);
    }

    private void error(final Token token, final String hintString) {
        throw new ParseException(token + ": " + hintString);
    }

    /**
     * System.out.println will be used to print the output String
     * of org.vulcan.parse.Parse.parse 's return Ast s toString method
     */
    public static void main(final String[] args) {
        if (args.length == 0) {
            final Scanner s = new Scanner(System.in);
            try {
                System.out.println("out: " + new Parser(new StringReader(s.nextLine())).parse());
            } catch (final Exception e) {
                e.printStackTrace();
            }
        } else {
            try {
                System.out.println("out: " + new Parser(args[0]).parse());
            } catch (final Exception e) {
                //
            }
        }

    }


}
