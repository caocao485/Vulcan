package org.vulcan.parse;

import org.vulcan.eval.value.JamVal;

/**
 * @author Think
 */
public class EvalException extends RuntimeException {
    public EvalException(final Ast ast, final String msg) {
        super("ast: " + ast + "  msg: " + msg);
    }

    public EvalException(final JamVal val, final String msg) {
        super("val: " + val + "  msg: " + msg);
    }
}
