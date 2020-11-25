package org.vulcan.eval.value;

import java.util.Collection;

/**
 * @author Think
 */
public final class NullVal implements ListVal {

    private NullVal() {

    }

    public static final NullVal NULL_VALUE = new NullVal();

    @Override
    public String toString() {
        return "()";
    }

    @Override
    public <T> T accept(final JamValVisitor<T> jamValVisitor) {
        return null;
    }

    @Override
    public ValueType getType() {
        return ValueType.NULL;
    }

    @Override
    public <T> Collection<? extends T> getValues() {
        throw new UnsupportedOperationException("Null Val has no value");
    }

}
