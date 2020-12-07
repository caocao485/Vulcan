package org.vulcan.parse;

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
    SEMICOLON,

    TBOOL,
    TINT,
    TUNIT
}
