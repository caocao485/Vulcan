package org.vulcan.parse;

/**
 * Visitor class for general org.vulcan.parse.Ast type  @param <T>  the type parameter
 * @author Think
 */
public interface AstVisitor<T> {
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
