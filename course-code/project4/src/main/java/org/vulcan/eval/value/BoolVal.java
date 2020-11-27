package org.vulcan.eval.value;

import java.util.Objects;

/**
 * @author Think
 */
public final class BoolVal implements JamVal {
    private boolean value;

    /**
     * The constant FALSE.
     */
    public static final BoolVal FALSE_VALUE = new BoolVal(false);
    /**
     * The constant TRUE.
     */
    public static final BoolVal TRUE_VALUE = new BoolVal(true);

    private BoolVal(final boolean value) {
        this.value = value;
    }

    public boolean isValue() {
        return this.value;
    }

    public void setValue(final boolean value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return String.valueOf(this.value);
    }

    @Override
    public <T> T accept(final JamValVisitor<T> jamValVisitor) {
        return jamValVisitor.forBoolVal(this);
    }

    public ValueType getType() {
        return ValueType.BOOL;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        BoolVal boolVal = (BoolVal) o;
        return value == boolVal.value;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}
