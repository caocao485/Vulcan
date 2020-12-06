package org.vulcan.eval;

/**
 * @author Think
 */
@FunctionalInterface
public interface BinaryOperator<E, T> {

    /**
     * for binary operators
     * @param a
     * @param b
     * @return
     */
    E binaryOperate(T a, T b);
}
