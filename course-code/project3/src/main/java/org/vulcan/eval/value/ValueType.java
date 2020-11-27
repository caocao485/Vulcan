package org.vulcan.eval.value;

/**
 * @author Think
 */

public enum ValueType {
    /**
     * Bool value type.
     */
    BOOL,
    /**
     * Int value type.
     */
    INT,
    /**
     * Null value type.
     */
    NULL,
    /**
     * Prim fun value type.
     */
    PRIM_FUN,
    /**
     * Closure value type
     */
    CLOSURE,
    /**
     * ListVal value type
     */
    LIST,
    LAZY_LIST
}
