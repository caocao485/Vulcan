package org.vulcan.parse;

/**
 * Jam general org.vulcan.parse.Ast type
 * @author Think
 */
public interface Ast {
    /**
     * Accept t.
     *
     * @param <T> the type parameter
     * @param v   the v
     * @return the t
     */
    <T> T accept(AstVisitor<T> v);
}
