package org.vulcan.eval.value;

/**
 * @author Think
 */
public interface JamVal {

    /**
     * JamValVisitor
     * @param jamValVisitor
     * @param <T>
     * @return
     */
    <T> T accept(JamValVisitor<T> jamValVisitor);
}
