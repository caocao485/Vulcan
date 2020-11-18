package org.vulcan.parse;

import java.io.*;
import java.util.*;

/**
 * Jam general org.vulcan.parse.AST type
 */
interface AST {
  /**
   * Accept t.
   *
   * @param <T> the type parameter
   * @param v   the v
   * @return the t
   */
  public <T> T accept(ASTVisitor<T> v);
}

/**
 * Visitor class for general org.vulcan.parse.AST type  @param <T>  the type parameter
 */
interface ASTVisitor<T> {
  /**
   * For bool constant t.
   *
   * @param b the b
   * @return the t
   */
  T forBoolConstant(BoolConstant b);

  /**
   * For int constant t.
   *
   * @param i the
   * @return the t
   */
  T forIntConstant(IntConstant i);

  /**
   * For null constant t.
   *
   * @param n the n
   * @return the t
   */
  T forNullConstant(NullConstant n);

  /**
   * For variable t.
   *
   * @param v the v
   * @return the t
   */
  T forVariable(Variable v);

  /**
   * For prim fun t.
   *
   * @param f the f
   * @return the t
   */
  T forPrimFun(PrimFun f);

  /**
   * For un op app t.
   *
   * @param u the u
   * @return the t
   */
  T forUnOpApp(UnOpApp u);

  /**
   * For bin op app t.
   *
   * @param b the b
   * @return the t
   */
  T forBinOpApp(BinOpApp b);

  /**
   * For app t.
   *
   * @param a the a
   * @return the t
   */
  T forApp(App a);

  /**
   * For map t.
   *
   * @param m the m
   * @return the t
   */
  T forMap(Map m);

  /**
   * For if t.
   *
   * @param i the
   * @return the t
   */
  T forIf(If i);

  /**
   * For let t.
   *
   * @param l the l
   * @return the t
   */
  T forLet(Let l);
}

/**
 * Jam term org.vulcan.parse.AST type
 */
interface Term extends AST {
  public <T> T accept(ASTVisitor<T> v);
}

/**
 * Jam constant type
 */
interface Constant extends Term {
  public <T> T accept(ASTVisitor<T> v);
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
   * Var token type.
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
  SEMICOLON;
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
  public TokenType getType();
}

/**
 * Jam Boolean constant class
 */
class BoolConstant implements Token, Constant {
  private boolean value;
  private BoolConstant(boolean b) { value = b; }

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
  public boolean getValue() { return value; }

  public <T> T accept(ASTVisitor<T> v) { return v.forBoolConstant(this); }
  public String toString() { return String.valueOf(value); }
  public TokenType getType() { return TokenType.BOOL; }
}

/**
 * Jam integer constant class
 */
class IntConstant implements Token, Constant {
  private int value;

  /**
   * Instantiates a new Int constant.
   *
   * @param i the
   */
  IntConstant(int i) { value = i; }
  // duplicates can occur!

  /**
   * Gets value.
   *
   * @return the value
   */
  public int getValue() { return value; }

  public <T> T accept(ASTVisitor<T> v) { return v.forIntConstant(this); }
  public String toString() { return String.valueOf(value); }
  public TokenType getType() { return TokenType.INT; }
}

/**
 * Jam null constant class, which is a singleton
 */
class NullConstant implements Token, Constant {
  /**
   * The constant ONLY.
   */
  public static final NullConstant ONLY = new NullConstant();
  private NullConstant() {}
  public <T> T accept(ASTVisitor<T> v) { return v.forNullConstant(this); }
  public String toString() { return "null"; }
  public TokenType getType() { return TokenType.NULL; }
}

/**
 * Jam primitive function Class
 */
class PrimFun implements Token, Term {
  private String name;

  /**
   * Instantiates a new Prim fun.
   *
   * @param n the n
   */
  PrimFun(String n) { name = n; }

  /**
   * Gets name.
   *
   * @return the name
   */
  public String getName() { return name; }
  public <T> T accept(ASTVisitor<T> v) { return v.forPrimFun(this); }
  public String toString() { return name; }
  public TokenType getType() { return TokenType.PRIM_FUN; }
}

/**
 * Jam variable class
 */
class Variable implements Token, Term {
  private String name;

  /**
   * Instantiates a new org.vulcan.parse.Variable.
   *
   * @param n the n
   */
  Variable(String n) { name = n; }

  /**
   * Gets name.
   *
   * @return the name
   */
  public String getName() { return name; }
  public <T> T accept(ASTVisitor<T> v) { return v.forVariable(this); }
  public String toString() { return name; }
  public TokenType getType() { return TokenType.VAR; }
}

/**
 * Jam operator class
 */
class Op implements Token {
  private String symbol;
  private boolean isUnOp;
  private boolean isBinOp;

  /**
   * Instantiates a new org.vulcan.parse.Op.
   *
   * @param s  the s
   * @param iu the iu
   * @param ib the ib
   */
  Op(String s, boolean iu, boolean ib) {
    symbol = s; isUnOp = iu; isBinOp = ib;
  }

  /**
   * Instantiates a new org.vulcan.parse.Op.
   *
   * @param s the s
   */
  Op(String s) {
    // isBinOp only!
    this(s,false,true);
  }

  /**
   * Gets symbol.
   *
   * @return the symbol
   */
  public String getSymbol() { return symbol; }

  /**
   * Is un op boolean.
   *
   * @return the boolean
   */
  public boolean isUnOp() { return isUnOp; }

  /**
   * Is bin op boolean.
   *
   * @return the boolean
   */
  public boolean isBinOp() { return isBinOp; }
  public String toString() { return symbol; }
  public TokenType getType() { return TokenType.OPERATOR; }
}

/**
 * The type Key word.
 */
class KeyWord implements Token {
  private String name;

  /**
   * Instantiates a new Key word.
   *
   * @param n the n
   */
  KeyWord(String n) { name = n; }

  /**
   * Gets name.
   *
   * @return the name
   */
  public String getName() { return name; }
  public String toString() { return name; }
  public TokenType getType() { return TokenType.KEYWORD; }
}

/**
 * Jam left paren token
 */
class LeftParen implements Token {
  public String toString() { return "("; }
  private LeftParen() {}

  /**
   * The constant ONLY.
   */
  public static final LeftParen ONLY = new LeftParen();
  public TokenType getType() { return TokenType.LEFT_PAREN; }
}

/**
 * Jam right paren token
 */
class RightParen implements Token {
  public String toString() { return ")"; }
  private RightParen() {}

  /**
   * The constant ONLY.
   */
  public static final RightParen ONLY = new RightParen();
  public TokenType getType() { return TokenType.RIGHT_PAREN; }
}

/**
 * Jam left bracket token
 */
class LeftBrack implements Token {
  public String toString() { return "["; }
  private LeftBrack() {}

  /**
   * The constant ONLY.
   */
  public static final LeftBrack ONLY = new LeftBrack();
  public TokenType getType() { return TokenType.LEFT_BRACK; }
}

/**
 * Jam right bracket token
 */
class RightBrack implements Token {
  public String toString() { return "]"; }
  private RightBrack() {}

  /**
   * The constant ONLY.
   */
  public static final RightBrack ONLY = new RightBrack();
  public TokenType getType() { return TokenType.RIGHT_BRACK; }
}

/**
 * Jam left brace token
 */
class LeftBrace implements Token {
  public String toString() { return "{"; }
  private LeftBrace() {}

  /**
   * The constant ONLY.
   */
  public static final LeftBrace ONLY = new LeftBrace();
  public TokenType getType() { return TokenType.LEFT_BRACE; }
}

/**
 * Jam right brace token
 */
class RightBrace implements Token {
  public String toString() { return "}"; }
  private RightBrace() {}

  /**
   * The constant ONLY.
   */
  public static final RightBrace ONLY = new RightBrace();
  public TokenType getType() { return TokenType.RIGHT_BRACE; }
}

/**
 * Jam comma token
 */
class Comma implements Token {
  public String toString() { return ","; }
  private Comma() {}

  /**
   * The constant ONLY.
   */
  public static final Comma ONLY = new Comma();
  public TokenType getType() { return TokenType.COMMA; }
}

/**
 * Jam semi-colon token
 */
class SemiColon implements Token {
  public String toString() { return ";"; }
  private SemiColon() {}

  /**
   * The constant ONLY.
   */
  public static final SemiColon ONLY = new SemiColon();
  public TokenType getType() { return TokenType.SEMICOLON; }
}


// org.vulcan.parse.AST class definitions

/**
 * Jam unary operator application class
 */
class UnOpApp implements AST {
  private Op rator;
  private AST arg;

  /**
   * Instantiates a new Un op app.
   *
   * @param r the r
   * @param a the a
   */
  UnOpApp(Op r, AST a) { rator = r; arg = a; }

  /**
   * Gets rator.
   *
   * @return the rator
   */
  public Op getRator() { return rator; }

  /**
   * Gets arg.
   *
   * @return the arg
   */
  public AST getArg() { return arg; }
  public <T> T accept(ASTVisitor<T> v) { return v.forUnOpApp(this); }
  public String toString() { return rator + " " + arg; }
}

/**
 * Jam binary operator application class
 */
class BinOpApp implements AST {
  private Op rator;
  private AST arg1, arg2;

  /**
   * Instantiates a new Bin op app.
   *
   * @param r  the r
   * @param a1 the a 1
   * @param a2 the a 2
   */
  BinOpApp(Op r, AST a1, AST a2) { rator = r; arg1 = a1; arg2 = a2; }

  /**
   * Gets rator.
   *
   * @return the rator
   */
  public Op getRator() { return rator; }

  /**
   * Gets arg 1.
   *
   * @return the arg 1
   */
  public AST getArg1() { return arg1; }

  /**
   * Gets arg 2.
   *
   * @return the arg 2
   */
  public AST getArg2() { return arg2; }
  public <T> T accept(ASTVisitor<T> v) { return v.forBinOpApp(this); }
  public String toString() { 
    return "(" + arg1 + " " + rator + " " + arg2 + ")"; 
  }
}

/**
 * Jam map (closure) class
 */
class Map implements AST {
  private Variable[] vars;
  private AST body;

  /**
   * Instantiates a new org.vulcan.parse.Map.
   *
   * @param v the v
   * @param b the b
   */
  Map(Variable[] v, AST b) { vars = v; body = b; }

  /**
   * Get vars variable [ ].
   *
   * @return the variable [ ]
   */
  public Variable[] getVars() { return vars; }

  /**
   * Gets body.
   *
   * @return the body
   */
  public AST getBody() { return body; }
  public <T> T accept(ASTVisitor<T> v) { return v.forMap(this); }
  public String toString() { 
    return "map " + ToString.toString(vars,",") + " to " + body ;
  }
}

/**
 * Jam function (org.vulcan.parse.PrimFun or org.vulcan.parse.Map) application class
 */
class App implements AST {
  private AST rator;
  private AST[] args;

  /**
   * Instantiates a new org.vulcan.parse.App.
   *
   * @param r the r
   * @param a the a
   */
  App(AST r, AST[] a) { rator = r; args = a; }

  /**
   * Gets rator.
   *
   * @return the rator
   */
  public AST getRator() { return rator; }

  /**
   * Get args ast [ ].
   *
   * @return the ast [ ]
   */
  public AST[] getArgs() { return args; }

  public <T> T accept(ASTVisitor<T> v) { return v.forApp(this); }
  public String toString() { 
    if ((rator instanceof Variable) || (rator instanceof PrimFun))
      return rator + "(" + ToString.toString(args,", ") + ")"; 
    else
      return "(" +  rator + ")(" + ToString.toString(args,", ") + ")"; 
  }
}

/**
 * Jam if expression class
 */
class If implements AST {
  private AST test, conseq, alt;

  /**
   * Instantiates a new org.vulcan.parse.If.
   *
   * @param t the t
   * @param c the c
   * @param a the a
   */
  If(AST t, AST c, AST a) { test = t; conseq = c; alt = a; }

  /**
   * Gets test.
   *
   * @return the test
   */
  public AST getTest() { return test; }

  /**
   * Gets conseq.
   *
   * @return the conseq
   */
  public AST getConseq() { return conseq; }

  /**
   * Gets alt.
   *
   * @return the alt
   */
  public AST getAlt() { return alt; }
  public <T> T accept(ASTVisitor<T> v) { return v.forIf(this); }
  public String toString() { 
    return "if " + test + " then " + conseq + " else " + alt ; 
  }
}

/**
 * Jam let expression class
 */
class Let implements AST {
  private Def[] defs;
  private AST body;

  /**
   * Instantiates a new org.vulcan.parse.Let.
   *
   * @param d the d
   * @param b the b
   */
  Let(Def[] d, AST b) { defs = d; body = b; }

  public <T> T accept(ASTVisitor<T> v) { return v.forLet(this); }

  /**
   * Get defs def [ ].
   *
   * @return the def [ ]
   */
  public Def[] getDefs() { return defs; }

  /**
   * Gets body.
   *
   * @return the body
   */
  public AST getBody() { return body; }
  public String toString() { 
    return "let " + ToString.toString(defs," ") + " in " + body; 
  }
}


/**
 * Jam definition class
 */
class Def {
  private Variable lhs;
  private AST rhs;

  /**
   * Instantiates a new org.vulcan.parse.Def.
   *
   * @param l the l
   * @param r the r
   */
  Def(Variable l, AST r) { lhs = l; rhs = r; }

  /**
   * Lhs variable.
   *
   * @return the variable
   */
  public Variable lhs() { return lhs; }

  /**
   * Rhs ast.
   *
   * @return the ast
   */
  public AST rhs() { return rhs; }

  public String toString() { return lhs + " := " + rhs + ";"; }
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
  public static String toString(Object[] a, String s) {
    StringBuffer result = new StringBuffer();
    for (int i = 0; i < a.length; i++) {
      if (i > 0) result.append(s);
      Object elt = a[i];
      String eltString = (elt instanceof Object[]) ?
        toString((Object[]) elt, s) : elt.toString();
      result.append(eltString);
    }
    return result.toString();
  }
}

/**
 * Parsing error class
 */
class ParseException extends RuntimeException {
  /**
   * Instantiates a new Parse exception.
   *
   * @param s the s
   */
  ParseException(String s) {
    super(s);
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
 */
class Lexer extends StreamTokenizer {

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
  public static final KeyWord IF     = new KeyWord("if");
  /**
   * The constant THEN.
   */
  public static final KeyWord THEN   = new KeyWord("then");
  /**
   * The constant ELSE.
   */
  public static final KeyWord ELSE   = new KeyWord("else");
  /**
   * The constant LET.
   */
  public static final KeyWord LET    = new KeyWord("let");
  /**
   * The constant IN.
   */
//  public static final org.vulcan.parse.KeyWord LETREC = new org.vulcan.parse.KeyWord("letrec");   // Used to support letrec extension
  public static final KeyWord IN     = new KeyWord("in");
  /**
   * The constant MAP.
   */
  public static final KeyWord MAP    = new KeyWord("map");
  /**
   * The constant TO.
   */
  public static final KeyWord TO     = new KeyWord("to");
  /**
   * The constant BIND.
   */
  public static final KeyWord BIND   = new KeyWord(":=");

  /**
   * The Word table.
   */
// wordtable for classifying words (identifiers/operators) in token stream
  public HashMap<String,Token>  wordTable = new HashMap<String,Token>();

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
  Lexer(Reader inputStream) {
    super(new BufferedReader(inputStream));
    initLexer();
  }

  /**
   * Constructs a org.vulcan.parse.Lexer for the contents of the specified file  @param fileName the file name
   *
   * @throws IOException the io exception
   */
  Lexer(String fileName) throws IOException {
    this(new FileReader(fileName));
  }

  /**
   * Constructs a org.vulcan.parse.Lexer for the default console input stream System.in
   */
  Lexer() {
    super(new BufferedReader(new InputStreamReader(System.in)));
    initLexer();
  }

  /* Initializes lexer tables and the StreamTokenizer that the lexer extends */
  private void initLexer() {

    // configure StreamTokenizer portion of this
    resetSyntax();
    parseNumbers();
    ordinaryChar('-');
    slashSlashComments(true);
    wordChars('0','9');
    wordChars('a','z');
    wordChars('A','Z');
    wordChars('_','_');
    wordChars('?','?');
    whitespaceChars(0,' '); 

    // `+' `-' `*' `/' `~' `=' `<' `>' `&' `|' `:' `;' `,' '!'
    // `(' `)' `[' `]' are ordinary characters (self-delimiting)
    

    
    initWordTable();
    buffer = null;  // buffer initially empty
  }

  /**
   * Reads tokens until next end-of-line  @throws IOException the io exception
   */
  public void flush() throws IOException {
    eolIsSignificant(true);
    while (nextToken() != EOL) ; // eat tokens until EOL
    eolIsSignificant(false);
  }

  /**
   * Returns the next token in the input stream without consuming it  @return the token
   */
  public Token peek() {
    if (buffer == null) buffer = readToken();
    return buffer;
  }
    
  /** Reads the next token as defined by StreamTokenizer in the input stream 
      (consuming it).  
   */
  private int getToken() {
    // synonymous with nextToken() except for throwing an unchecked 
    // org.vulcan.parse.ParseException instead of a checked IOException
    try {
      int tokenType = nextToken();
      return tokenType;
    } catch(IOException e) {
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

    if (buffer != null) {
      Token token = buffer;
      buffer = null;          // clear buffer
      return token;
    }
    
    int tokenType = getToken();
    switch (tokenType) {
    case NUMBER:
      int value = (int) nval;
      if (nval == (double) value) return new IntConstant(value);
      throw 
        new ParseException("The number " + nval + " is not a 32 bit integer");
    case WORD:
      Token regToken = wordTable.get(sval);
      if (regToken == null) {
        // must be new variable name
        Variable newVar = new Variable(sval);
        wordTable.put(sval,newVar);
        return newVar;
      }
      return regToken;
    case EOF: return null;
    case '(': return LeftParen.ONLY;
    case ')': return RightParen.ONLY;
    case '[': return LeftBrack.ONLY;
    case ']': return RightBrack.ONLY;
 // case '{': return org.vulcan.parse.LeftBrace.ONLY;
 // case '}': return org.vulcan.parse.RightBrace.ONLY;
    case ',': return Comma.ONLY;
    case ';': return SemiColon.ONLY;

    case '+': return wordTable.get("+");  
    case '-': return wordTable.get("-");  
    case '*': return wordTable.get("*");  
    case '/': return wordTable.get("/");  
    case '~': return wordTable.get("~");  
    case '=': return wordTable.get("=");  
    case '<': 
      tokenType = getToken();
      if (tokenType == '=') return wordTable.get("<=");  
      // if (tokenType == '-') return wordTable.get("<-");  
      pushBack();
      return wordTable.get("<");  
    case '>': 
      tokenType = getToken();
      if (tokenType == '=') return wordTable.get(">=");  
      pushBack();
      return wordTable.get(">"); 
      case '!': 
        tokenType = getToken();
        if (tokenType == '=') return wordTable.get("!=");  
        else throw new ParseException("!" + ((char) tokenType) + " is not a legal token");

        /*
         * // this alternate else clause will be added in later assignments
         * pushBack();
         * return wordTable.get("!");  
         */
    case '&': return wordTable.get("&");  
    case '|': return wordTable.get("|");  
    case ':': {
      tokenType = getToken();
      if (tokenType == '=') return wordTable.get(":=");  
      pushBack();
      throw new ParseException("`:' is not a legalken");
    }
    default:  
      throw new 
        ParseException("`" + ((char) tokenType) + "' is not a legal token");
    }
  }
    
  /** Initializes the table of Strings used to recognize Tokens */
  private void initWordTable() {
    // initialize wordTable

    // constants
    // <null>  ::= null
    // <bool>  ::= true | false

    wordTable.put("null",  NullConstant.ONLY);
    wordTable.put("true",  BoolConstant.TRUE);
    wordTable.put("false", BoolConstant.FALSE);

    // Install operator symbols constructed from self-delimiting characters

    // operators
    // <unop>  ::= <sign> | ~   | ! 
    // <binop> ::= <sign> | "*" | / | = | != | < | > | <= | >= | & | "|" |
    //             <- 
    // <sign>  ::= "+" | -

    //  Note: there is no class distinction between <unop> and <binop> at 
    //  lexical level because of ambiguity; <sign> belongs to both

    wordTable.put("+",   new Op("+",true,true)); 
    wordTable.put("-",   new Op("-",true,true)); 
    wordTable.put("~",   new Op("~",true,false)); 
    wordTable.put("!",   new Op("!",true,false)); 

    wordTable.put("*",  new Op("*")); 
    wordTable.put("/",  new Op("/")); 
    wordTable.put("=",  new Op("=")); 
    wordTable.put("!=", new Op("!=")); 
    wordTable.put("<",  new Op("<")); 
    wordTable.put(">",  new Op(">")); 
    wordTable.put("<=", new Op("<=")); 
    wordTable.put(">=", new Op(">=")); 
    wordTable.put("&",  new Op("&")); 
    wordTable.put("|",  new Op("|")); 
    wordTable.put("<-", new Op("<-")); 
    
    // Install keywords
    
    wordTable.put("if",   IF);
    wordTable.put("then", THEN);
    wordTable.put("else", ELSE);
    wordTable.put("let",  LET);
    wordTable.put("in",   IN);
    wordTable.put("map",  MAP);
    wordTable.put("to",   TO);
    wordTable.put(":=",   BIND);


    // Install primitive functions
    // <prim>  ::= number? | function? | list? | null? 
    //           | cons? | cons | first | rest | arity

    wordTable.put("number?",   new PrimFun("number?"));
    wordTable.put("function?", new PrimFun("function?"));
    // wordTable.put("ref?",      new org.vulcan.parse.PrimFun("ref?"));
    wordTable.put("list?",     new PrimFun("list?"));
    wordTable.put("null?",     new PrimFun("null?"));
    wordTable.put("cons?",     new PrimFun("cons?"));
    wordTable.put("arity",     new PrimFun("arity"));
    wordTable.put("cons",      new PrimFun("cons"));
    wordTable.put("first",     new PrimFun("first"));
    wordTable.put("rest",      new PrimFun("rest"));
  }

  /**
   * Provides a command line interface to the lexer  @param args the input arguments
   *
   * @throws IOException the io exception
   */
  public static void main(String[] args) throws IOException {
    // check for legal argument list 
    Lexer in;
    if (args.length == 0) {
      in = new Lexer();
    }
    else in = new Lexer(args[0]);
    do {
      Token t = in.readToken();
      if (t == null) break;
      System.out.println("org.vulcan.parse.Token " + t + " in " + t.getClass());
    } while (true);
  }
}
