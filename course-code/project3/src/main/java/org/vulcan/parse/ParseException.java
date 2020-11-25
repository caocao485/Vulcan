package org.vulcan.parse;

/**
 * Parsing error class
 * @author Think
 */
public class ParseException extends RuntimeException {
    /**
     * Instantiates a new Parse exception.
     *
     * @param s the s
     */
    ParseException(final String s) {
        super(s);
    }
}
