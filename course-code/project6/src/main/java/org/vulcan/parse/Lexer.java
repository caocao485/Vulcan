package org.vulcan.parse;

import java.io.*;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Jam term org.vulcan.parse.Ast type
 */
interface Term extends Ast {
    @Override
    <T> T accept(AstVisitor<T> v);
}

/**
 * Jam constant type
 */
interface Constant extends Term {
    @Override
    <T> T accept(AstVisitor<T> v);
}

/**
 * The enum org.vulcan.parse.Token type.
 */
enum TokenType {
    /**
     * Bool token type.
     */
    BOOL,
    /**
     * Int token type.
     */
    INT,
    /**
     * Null token type.
     */
    NULL,
    /**
     * Prim fun token type.
     */
    PRIM_FUN,
    /**
     * Variable token type.
     */
    VAR,
    /**
     * Operator token type.
     */
    OPERATOR,
    /**
     * Keyword token type.
     */
    KEYWORD,
    /**
     * Left paren token type.
     */
    LEFT_PAREN,
    /**
     * Right paren token type.
     */
    RIGHT_PAREN,
    /**
     * Left brack token type.
     */
    LEFT_BRACK,
    /**
     * Right brack token type.
     */
    RIGHT_BRACK,
    /**
     * Left brace token type.
     */
    LEFT_BRACE,
    /**
     * Right brace token type.
     */
    RIGHT_BRACE,
    /**
     * org.vulcan.parse.Comma token type.
     */
    COMMA,
    /**
     * Semicolon token type.
     */
    SEMICOLON
}

/**
 * Jam token type
 */
interface Token {
    /**
     * Gets type.
     *
     * @return the type
     */
    TokenType getType();
}

/**
 * Jam Boolean constant class
 */
class BoolConstant implements Token, Constant {
    private final boolean value;

    private BoolConstant(final boolean b) {
      this.value = b;
    }

    /**
     * The constant FALSE.
     */
// ** singleton pattern **
    public static final BoolConstant FALSE = new BoolConstant(false);
    /**
     * The constant TRUE.
     */
    public static final BoolConstant TRUE = new BoolConstant(true);

    /**
     * Gets value.
     *
     * @return the value
     */
    public boolean getValue() {
        return this.value;
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forBoolConstant(this);
    }

    @Override
    public String toString() {
        return String.valueOf(this.value);
    }

    @Override
    public TokenType getType() {
        return TokenType.BOOL;
    }
}

/**
 * Jam integer constant class
 */
class IntConstant implements Token, Constant {
    private final int value;

    /**
     * Instantiates a new Int constant.
     *
     * @param i the
     */
    IntConstant(final int i) {
      this.value = i;
    }
    // duplicates can occur!

    /**
     * Gets value.
     *
     * @return the value
     */
    public int getValue() {
        return this.value;
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forIntConstant(this);
    }

    public String toString() {
        return String.valueOf(this.value);
    }

    @Override
    public TokenType getType() {
        return TokenType.INT;
    }
}

/**
 * Jam null constant class, which is a singleton
 */
class NullConstant implements Token, Constant {
    /**
     * The constant ONLY.
     */
    public static final NullConstant ONLY = new NullConstant();

    private NullConstant() {
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forNullConstant(this);
    }

    public String toString() {
        return "null";
    }

    @Override
    public TokenType getType() {
        return TokenType.NULL;
    }
}

/**
 * Jam primitive function Class
 */
class PrimFun implements Token, Term {
    private final String name;

    /**
     * Instantiates a new Prim fun.
     *
     * @param n the n
     */
    PrimFun(final String n) {
      this.name = n;
    }

    /**
     * Gets name.
     *
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forPrimFun(this);
    }

    public String toString() {
        return this.name;
    }

    @Override
    public TokenType getType() {
        return TokenType.PRIM_FUN;
    }
}

/**
 * Jam operator class
 */
class Op implements Token {
    private final String symbol;
    private final boolean isUnOp;
    private final boolean isBinOp;

    /**
     * Instantiates a new org.vulcan.parse.Op.
     *
     * @param s  the s
     * @param iu the iu
     * @param ib the ib
     */
    Op(final String s, final boolean iu, final boolean ib) {
      this.symbol = s;
      this.isUnOp = iu;
      this.isBinOp = ib;
    }

    /**
     * Instantiates a new org.vulcan.parse.Op.
     *
     * @param s the s
     */
    Op(final String s) {
        // isBinOp only!
        this(s, false, true);
    }

    /**
     * Gets symbol.
     *
     * @return the symbol
     */
    public String getSymbol() {
        return this.symbol;
    }

    /**
     * Is un op boolean.
     *
     * @return the boolean
     */
    public boolean isUnOp() {
        return this.isUnOp;
    }

    /**
     * Is bin op boolean.
     *
     * @return the boolean
     */
    public boolean isBinOp() {
        return this.isBinOp;
    }

    public String toString() {
        return this.symbol;
    }

    @Override
    public TokenType getType() {
        return TokenType.OPERATOR;
    }
}

/**
 * The type Key word.
 */
class KeyWord implements Token {
    private final String name;

    /**
     * Instantiates a new Key word.
     *
     * @param n the n
     */
    KeyWord(final String n) {
      this.name = n;
    }

    /**
     * Gets name.
     *
     * @return the name
     */
    public String getName() {
        return this.name;
    }

    public String toString() {
        return this.name;
    }

    @Override
    public TokenType getType() {
        return TokenType.KEYWORD;
    }
}

/**
 * Jam left paren token
 */
class LeftParen implements Token {
    public String toString() {
        return "(";
    }

    private LeftParen() {
    }

    /**
     * The constant ONLY.
     */
    public static final LeftParen ONLY = new LeftParen();

    @Override
    public TokenType getType() {
        return TokenType.LEFT_PAREN;
    }
}

/**
 * Jam right paren token
 */
class RightParen implements Token {
    public String toString() {
        return ")";
    }

    private RightParen() {
    }

    /**
     * The constant ONLY.
     */
    public static final RightParen ONLY = new RightParen();

    @Override
    public TokenType getType() {
        return TokenType.RIGHT_PAREN;
    }
}

/**
 * Jam left bracket token
 */
class LeftBrack implements Token {
    public String toString() {
        return "[";
    }

    private LeftBrack() {
    }

    /**
     * The constant ONLY.
     */
    public static final LeftBrack ONLY = new LeftBrack();

    @Override
    public TokenType getType() {
        return TokenType.LEFT_BRACK;
    }
}

/**
 * Jam right bracket token
 */
class RightBrack implements Token {
    public String toString() {
        return "]";
    }

    private RightBrack() {
    }

    /**
     * The constant ONLY.
     */
    public static final RightBrack ONLY = new RightBrack();

    @Override
    public TokenType getType() {
        return TokenType.RIGHT_BRACK;
    }
}

/**
 * Jam left brace token
 */
class LeftBrace implements Token {
    public String toString() {
        return "{";
    }

    private LeftBrace() {
    }

    /**
     * The constant ONLY.
     */
    public static final LeftBrace ONLY = new LeftBrace();

    @Override
    public TokenType getType() {
        return TokenType.LEFT_BRACE;
    }
}

/**
 * Jam right brace token
 */
class RightBrace implements Token {
    public String toString() {
        return "}";
    }

    private RightBrace() {
    }

    /**
     * The constant ONLY.
     */
    public static final RightBrace ONLY = new RightBrace();

    @Override
    public TokenType getType() {
        return TokenType.RIGHT_BRACE;
    }
}

/**
 * Jam comma token
 */
class Comma implements Token {
    public String toString() {
        return ",";
    }

    private Comma() {
    }

    /**
     * The constant ONLY.
     */
    public static final Comma ONLY = new Comma();

    @Override
    public TokenType getType() {
        return TokenType.COMMA;
    }
}

/**
 * Jam semi-colon token
 */
class SemiColon implements Token {
    public String toString() {
        return ";";
    }

    private SemiColon() {
    }

    /**
     * The constant ONLY.
     */
    public static final SemiColon ONLY = new SemiColon();

    @Override
    public TokenType getType() {
        return TokenType.SEMICOLON;
    }
}


// org.vulcan.parse.Ast class definitions

/**
 * Jam unary operator application class
 */
class UnOpApp implements Ast {
    private final Op rator;
    private final Ast arg;

    /**
     * Instantiates a new Un op app.
     *
     * @param r the r
     * @param a the a
     */
    UnOpApp(final Op r, final Ast a) {
      this.rator = r;
      this.arg = a;
    }

    /**
     * Gets rator.
     *
     * @return the rator
     */
    public Op getRator() {
        return this.rator;
    }

    /**
     * Gets arg.
     *
     * @return the arg
     */
    public Ast getArg() {
        return this.arg;
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forUnOpApp(this);
    }

    @Override
    public String toString() {
        return this.rator + " " + this.arg;
    }
}

/**
 * Jam binary operator application class
 */
class BinOpApp implements Ast {
    private final Op rator;
    private final Ast arg1;
  private final Ast arg2;

    /**
     * Instantiates a new Bin op app.
     *
     * @param r  the r
     * @param a1 the a 1
     * @param a2 the a 2
     */
    BinOpApp(final Op r, final Ast a1, final Ast a2) {
      this.rator = r;
      this.arg1 = a1;
      this.arg2 = a2;
    }

    /**
     * Gets rator.
     *
     * @return the rator
     */
    public Op getRator() {
        return this.rator;
    }

    /**
     * Gets arg 1.
     *
     * @return the arg 1
     */
    public Ast getArg1() {
        return this.arg1;
    }

    /**
     * Gets arg 2.
     *
     * @return the arg 2
     */
    public Ast getArg2() {
        return this.arg2;
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forBinOpApp(this);
    }

    @Override
    public String toString() {
        return "(" + this.arg1 + " " + this.rator + " " + this.arg2 + ")";
    }
}

/**
 * Jam map (closure) class
 */
class MapAst implements Ast {
    private final Variable[] vars;
    private final Ast body;

    /**
     * Instantiates a new org.vulcan.parse..
     *
     * @param v the v
     * @param b the b
     */
    MapAst(final Variable[] v, final Ast b) {
      this.vars = v;
      this.body = b;
    }

    /**
     * Get vars variable [ ].
     *
     * @return the variable [ ]
     */
    public Variable[] getVars() {
        return this.vars;
    }

    /**
     * Gets body.
     *
     * @return the body
     */
    public Ast getBody() {
        return this.body;
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forMap(this);
    }

    public String toString() {
        return "map " + ToString.toString(this.vars, ",") + " to " + this.body;
    }
}

/**
 * Jam letcc (continuation) class
 */
class Letcc implements Ast {
    private final Variable var;
    private final Ast body;


    /**
     * Instantiates a new org.vulcan.parse..
     *
     * @param v the v
     * @param b the b
     */
    Letcc(final Variable v, final Ast b) {
        this.var = v;
        this.body = b;
    }

    /**
     * Get vars variable [ ].
     *
     * @return the variable [ ]
     */
    public Variable getVar() {
        return this.var;
    }

    /**
     * Gets body.
     *
     * @return the body
     */
    public Ast getBody() {
        return this.body;
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forLetcc(this);
    }

    public String toString() {
        return "letcc " + var + " in " + this.body;
    }
}

/**
 * Jam function (org.vulcan.parse.PrimFun or org.vulcan.parse.) application class
 */
class App implements Ast {
    private final Ast rator;
    private final Ast[] args;

    /**
     * Instantiates a new org.vulcan.parse.App.
     *
     * @param r the r
     * @param a the a
     */
    App(final Ast r, final Ast[] a) {
      this.rator = r;
      this.args = a;
    }

    /**
     * Gets rator.
     *
     * @return the rator
     */
    public Ast getRator() {
        return this.rator;
    }

    /**
     * Get args ast [ ].
     *
     * @return the ast [ ]
     */
    public Ast[] getArgs() {
        return this.args;
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forApp(this);
    }

    public String toString() {
        if ((this.rator instanceof Variable) || (this.rator instanceof PrimFun))
            return this.rator + "(" + ToString.toString(this.args, ", ") + ")";
        else
            return "(" + this.rator + ")(" + ToString.toString(this.args, ", ") + ")";
    }
}

class Block implements Ast {
    private final Ast[] states;

    Block(Ast[] states) {
        this.states = states;
    }

    public Ast[] getStates() {
        return states;
    }

    @Override
    public String toString() {
        return "{" + Arrays
                .stream(states)
                .map(Objects::toString)
                .collect(Collectors.joining("; ")) +
                '}';
    }

    @Override
    public <T> T accept(AstVisitor<T> v) {
        return v.forBlock(this);
    }
}


/**
 * Jam if expression class
 */
class If implements Ast {
    private final Ast test;
  private final Ast conseq;
  private final Ast alt;

    /**
     * Instantiates a new org.vulcan.parse.If.
     *
     * @param t the t
     * @param c the c
     * @param a the a
     */
    If(final Ast t, final Ast c, final Ast a) {
      this.test = t;
      this.conseq = c;
      this.alt = a;
    }

    /**
     * Gets test.
     *
     * @return the test
     */
    public Ast getTest() {
        return this.test;
    }

    /**
     * Gets conseq.
     *
     * @return the conseq
     */
    public Ast getConseq() {
        return this.conseq;
    }

    /**
     * Gets alt.
     *
     * @return the alt
     */
    public Ast getAlt() {
        return this.alt;
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forIf(this);
    }

    public String toString() {
        return "if " + this.test + " then " + this.conseq + " else " + this.alt;
    }
}

/**
 * Jam let expression class
 */
class Let implements Ast {
    private final Def[] defs;
    private final Ast body;

    /**
     * Instantiates a new org.vulcan.parse.Let.
     *
     * @param d the d
     * @param b the b
     */
    Let(final Def[] d, final Ast b) {
      this.defs = d;
      this.body = b;
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forLet(this);
    }

    /**
     * Get defs def [ ].
     *
     * @return the def [ ]
     */
    public Def[] getDefs() {
        return this.defs;
    }

    /**
     * Gets body.
     *
     * @return the body
     */
    public Ast getBody() {
        return this.body;
    }

    public String toString() {
        return "let " + ToString.toString(this.defs, " ") + " in " + this.body;
    }
}


/**
 * Jam letrec expression class
 */
class LetRec implements Ast {
    private final MapDef[] defs;
    private final Ast body;

    /**
     * Instantiates a new org.vulcan.parse.Let.
     *
     * @param d the d
     * @param b the b
     */
    LetRec(final MapDef[] d, final Ast b) {
        this.defs = d;
        this.body = b;
    }

    @Override
    public <T> T accept(final AstVisitor<T> v) {
        return v.forLetRec(this);
    }

    /**
     * Get defs def [ ].
     *
     * @return the def [ ]
     */
    public MapDef[] getDefs() {
        return this.defs;
    }

    /**
     * Gets body.
     *
     * @return the body
     */
    public Ast getBody() {
        return this.body;
    }

    public String toString() {
        return "letrec " + ToString.toString(this.defs, " ") + " in " + this.body;
    }
}

class MapDef {
    private final Variable lhs;
    private final Ast rhs;

    /**
     * Instantiates a new org.vulcan.parse.Def.
     *
     * @param l the l
     * @param r the r
     */
    MapDef(final Variable l, final Ast r) {
        this.lhs = l;
        this.rhs = r;
    }

    /**
     * Lhs variable.
     *
     * @return the variable
     */
    public Variable lhs() {
        return this.lhs;
    }

    /**
     * Rhs ast.
     *
     * @return the ast
     */
    public Ast rhs() {
        return this.rhs;
    }

    public String toString() {
        return this.lhs + " := " + this.rhs + ";";
    }
}

/**
 * Jam definition class
 */
class Def {
    private final Variable lhs;
    private final Ast rhs;

    /**
     * Instantiates a new org.vulcan.parse.Def.
     *
     * @param l the l
     * @param r the r
     */
    Def(final Variable l, final Ast r) {
      this.lhs = l;
      this.rhs = r;
    }

    /**
     * Lhs variable.
     *
     * @return the variable
     */
    public Variable lhs() {
        return this.lhs;
    }

    /**
     * Rhs ast.
     *
     * @return the ast
     */
    public Ast rhs() {
        return this.rhs;
    }

    public String toString() {
        return this.lhs + " := " + this.rhs + ";";
    }
}

/**
 * String utility class
 */
class ToString {

    /**
     * prints array a with separator s between elements
     * this method does NOT accept a == null, since null
     * is NOT an array  @param a the a
     *
     * @param s the s
     * @return the string
     */
    public static String toString(final Object[] a, final String s) {
        final StringBuffer result = new StringBuffer();
        for (int i = 0; i < a.length; i++) {
            if (i > 0) result.append(s);
            final Object elt = a[i];
            final String eltString = (elt instanceof Object[]) ?
                    ToString.toString((Object[]) elt, s) : elt.toString();
            result.append(eltString);
        }
        return result.toString();
    }
}

/**
 * Jam lexer class.
 * Given a org.vulcan.parse.Lexer object, the next token in that input stream being
 * processed by the org.vulcan.parse.Lexer is returned by static method readToken(); it
 * throws a org.vulcan.parse.ParseException (a form of RuntimeException) if it
 * encounters a syntax error.  Calling readToken() advances the cursor
 * in the input stream to the next token.
 * The static method peek() in the org.vulcan.parse.Lexer class has the same behavior as
 * readToken() except for the fact that it does not advance the cursor.
 * @author Think
 */
public class Lexer extends StreamTokenizer {

    /**
     * The constant WORD.
     */
    /* short names for StreamTokenizer codes */
    public static final int WORD = StreamTokenizer.TT_WORD;
    /**
     * The constant NUMBER.
     */
    public static final int NUMBER = StreamTokenizer.TT_NUMBER;
    /**
     * The constant EOF.
     */
    public static final int EOF = StreamTokenizer.TT_EOF;
    /**
     * The constant EOL.
     */
    public static final int EOL = StreamTokenizer.TT_EOL;

    /**
     * The constant IF.
     */
    /* Keywords */
    public static final KeyWord IF = new KeyWord("if");
    /**
     * The constant THEN.
     */
    public static final KeyWord THEN = new KeyWord("then");
    /**
     * The constant ELSE.
     */
    public static final KeyWord ELSE = new KeyWord("else");
    /**
     * The constant LET.
     */
    public static final KeyWord LET = new KeyWord("let");

    /**
     * The constant LETREC.
     */
    public static final KeyWord LET_REC = new KeyWord("letrec");

    public static final KeyWord LET_CC = new KeyWord("letcc");
    /**
     * The constant IN.
     */
//  public static final org.vulcan.parse.KeyWord LETREC = new org.vulcan.parse.KeyWord("letrec");   // Used to support letrec extension
    public static final KeyWord IN = new KeyWord("in");
    /**
     * The constant MAP.
     */
    public static final KeyWord MAP = new KeyWord("map");
    /**
     * The constant TO.
     */
    public static final KeyWord TO = new KeyWord("to");
    /**
     * The constant BIND.
     */
    public static final KeyWord BIND = new KeyWord(":=");

    /**
     * The Word table.
     */
// wordtable for classifying words (identifiers/operators) in token stream
    public HashMap<String, Token> wordTable = new HashMap<String, Token>();

    // org.vulcan.parse.Lexer peek cannot be implemented using StreamTokenizer pushBack
    // because some Tokens are composed of two StreamTokenizer tokens

    /**
     * The Buffer.
     */
    Token buffer;  // holds token for peek() operation

    /* constructors */

    /**
     * Constructs a org.vulcan.parse.Lexer for the specified inputStream  @param inputStream the input stream
     */
    public Lexer(final Reader inputStream) {
        super(new BufferedReader(inputStream));
      this.initLexer();
    }

    /**
     * Constructs a org.vulcan.parse.Lexer for the contents of the specified file  @param fileName the file name
     *
     * @throws IOException the io exception
     */
    public Lexer(final String fileName) throws IOException {
        this(new FileReader(fileName));
    }

    /**
     * Constructs a org.vulcan.parse.Lexer for the default console input stream System.in
     */
    Lexer() {
        super(new BufferedReader(new InputStreamReader(System.in)));
      this.initLexer();
    }

    /* Initializes lexer tables and the StreamTokenizer that the lexer extends */
    private void initLexer() {

        // configure StreamTokenizer portion of this
      this.resetSyntax();
      this.parseNumbers();
      this.ordinaryChar('-');
      this.slashSlashComments(true);
      this.wordChars('0', '9');
      this.wordChars('a', 'z');
      this.wordChars('A', 'Z');
      this.wordChars('_', '_');
      this.wordChars('?', '?');
      this.whitespaceChars(0, ' ');

        // `+' `-' `*' `/' `~' `=' `<' `>' `&' `|' `:' `;' `,' '!'
        // `(' `)' `[' `]' are ordinary characters (self-delimiting)


      this.initWordTable();
      this.buffer = null;  // buffer initially empty
    }

    /**
     * Reads tokens until next end-of-line  @throws IOException the io exception
     */
    public void flush() throws IOException {
      this.eolIsSignificant(true);
        while (this.nextToken() != Lexer.EOL) ; // eat tokens until EOL
      this.eolIsSignificant(false);
    }

    /**
     * Returns the next token in the input stream without consuming it  @return the token
     */
    public Token peek() {
        if (this.buffer == null) this.buffer = this.readToken();
        return this.buffer;
    }

    /**
     * Reads the next token as defined by StreamTokenizer in the input stream
     * (consuming it).
     */
    private int getToken() {
        // synonymous with nextToken() except for throwing an unchecked
        // org.vulcan.parse.ParseException instead of a checked IOException
        try {
            final int tokenType = this.nextToken();
            return tokenType;
        } catch (final IOException e) {
            throw new ParseException("IOException " + e + "thrown by nextToken()");
        }
    }

    /**
     * Reads the next org.vulcan.parse.Token in the input stream (consuming it)  @return the token
     */
    public Token readToken() {

        // uses getToken() to read next token
        // constructs org.vulcan.parse.Token object representing that token
        // NOTE: token representations for all org.vulcan.parse.Token classes except
        //   org.vulcan.parse.IntConstant are unique; a HashMap is used to avoid duplication
        //   Hence, == can safely be used to compare all Tokens except IntConstants
        //   for equality

        if (this.buffer != null) {
            final Token token = this.buffer;
          this.buffer = null;          // clear buffer
            return token;
        }

        int tokenType = this.getToken();
        switch (tokenType) {
            case Lexer.NUMBER:
                final int value = (int) this.nval;
                if (this.nval == (double) value) return new IntConstant(value);
                throw
                        new ParseException("The number " + this.nval + " is not a 32 bit integer");
            case Lexer.WORD:
                final Token regToken = this.wordTable.get(this.sval);
                if (regToken == null) {
                    // must be new variable name
                    final Variable newVar = new Variable(this.sval);
                  this.wordTable.put(this.sval, newVar);
                    return newVar;
                }
                return regToken;
            case Lexer.EOF:
                return null;
            case '(':
                return LeftParen.ONLY;
            case ')':
                return RightParen.ONLY;
            case '[':
                return LeftBrack.ONLY;
            case ']':
                return RightBrack.ONLY;
            case '{': return LeftBrace.ONLY;
            case '}': return RightBrace.ONLY;
            case ',':
                return Comma.ONLY;
            case ';':
                return SemiColon.ONLY;

            case '+':
                return this.wordTable.get("+");
            case '-':
                return this.wordTable.get("-");
            case '*':
                return this.wordTable.get("*");
            case '/':
                return this.wordTable.get("/");
            case '~':
                return this.wordTable.get("~");
            case '=':
                return this.wordTable.get("=");
            case '<':
                tokenType = this.getToken();
                if (tokenType == '=') return this.wordTable.get("<=");
                if (tokenType == '-') return wordTable.get("<-");
              this.pushBack();
                return this.wordTable.get("<");
            case '>':
                tokenType = this.getToken();
                if (tokenType == '=') return this.wordTable.get(">=");
              this.pushBack();
                return this.wordTable.get(">");
            case '!':
                tokenType = this.getToken();
                if (tokenType == '=') return this.wordTable.get("!=");
                // this alternate else clause will be added in later assignments
                 this.pushBack();
                 return wordTable.get("!");
            case '&':
                return this.wordTable.get("&");
            case '|':
                return this.wordTable.get("|");
            case ':': {
                tokenType = this.getToken();
                if (tokenType == '=') return this.wordTable.get(":=");
              this.pushBack();
                throw new ParseException("`:' is not a legalken");
            }
            default:
                throw new
                        ParseException("`" + ((char) tokenType) + "' is not a legal token");
        }
    }

    /**
     * Initializes the table of Strings used to recognize Tokens
     */
    private void initWordTable() {
        // initialize wordTable

        // constants
        // <null>  ::= null
        // <bool>  ::= true | false

      this.wordTable.put("null", NullConstant.ONLY);
      this.wordTable.put("true", BoolConstant.TRUE);
      this.wordTable.put("false", BoolConstant.FALSE);

        // Install operator symbols constructed from self-delimiting characters

        // operators
        // <unop>  ::= <sign> | ~   | !
        // <binop> ::= <sign> | "*" | / | = | != | < | > | <= | >= | & | "|" |
        //             <-
        // <sign>  ::= "+" | -

        //  Note: there is no class distinction between <unop> and <binop> at
        //  lexical level because of ambiguity; <sign> belongs to both

      this.wordTable.put("+", new Op("+", true, true));
      this.wordTable.put("-", new Op("-", true, true));
      this.wordTable.put("~", new Op("~", true, false));
      this.wordTable.put("!", new Op("!", true, false));
      this.wordTable.put("ref",new Op("ref", true, false));

      this.wordTable.put("*", new Op("*"));
      this.wordTable.put("/", new Op("/"));
      this.wordTable.put("=", new Op("="));
      this.wordTable.put("!=", new Op("!="));
      this.wordTable.put("<", new Op("<"));
      this.wordTable.put(">", new Op(">"));
      this.wordTable.put("<=", new Op("<="));
      this.wordTable.put(">=", new Op(">="));
      this.wordTable.put("&", new Op("&"));
      this.wordTable.put("|", new Op("|"));
      this.wordTable.put("<-", new Op("<-"));

        // Install keywords

      this.wordTable.put("if", Lexer.IF);
      this.wordTable.put("then", Lexer.THEN);
      this.wordTable.put("else", Lexer.ELSE);
      this.wordTable.put("let", Lexer.LET);
      this.wordTable.put("letrec", Lexer.LET_REC);
      this.wordTable.put("letcc",LET_CC);
      this.wordTable.put("in", Lexer.IN);
      this.wordTable.put("map", Lexer.MAP);
      this.wordTable.put("to", Lexer.TO);
      this.wordTable.put(":=", Lexer.BIND);


        // Install primitive functions
        // <prim>  ::= number? | function? | list? | null?
        //           | cons? | cons | first | rest | arity

      this.wordTable.put("number?", new PrimFun("number?"));
      this.wordTable.put("function?", new PrimFun("function?"));
      this.wordTable.put("ref?",new PrimFun("ref?"));
      this.wordTable.put("list?", new PrimFun("list?"));
      this.wordTable.put("null?", new PrimFun("null?"));
      this.wordTable.put("cons?", new PrimFun("cons?"));
      this.wordTable.put("arity", new PrimFun("arity"));
      this.wordTable.put("cons", new PrimFun("cons"));
      this.wordTable.put("first", new PrimFun("first"));
      this.wordTable.put("rest", new PrimFun("rest"));
      this.wordTable.put("asBool", new PrimFun("asBool"));
    }

    /**
     * Provides a command line interface to the lexer  @param args the input arguments
     *
     * @throws IOException the io exception
     */
    public static void main(final String[] args) throws IOException {
        // check for legal argument list
        final Lexer in;
        if (args.length == 0) {
            in = new Lexer();
        } else in = new Lexer(args[0]);
        do {
            final Token t = in.readToken();
            if (t == null) break;
            System.out.println("org.vulcan.parse.Token " + t + " in " + t.getClass());
        } while (true);
    }
}
