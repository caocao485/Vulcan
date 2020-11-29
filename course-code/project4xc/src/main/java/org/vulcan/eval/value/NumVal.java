package org.vulcan.eval.value;

import java.util.Objects;

/**
 * @author Think
 */
public class NumVal implements JamVal {
    private Integer value;

    public NumVal(final Integer value) {
        this.value = value;
    }

    public Integer getValue() {
        return this.value;
    }

    public void setValue(final Integer value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return this.value.toString();
    }

    @Override
    public <T> T accept(final JamValVisitor<T> jamValVisitor) {
        return null;
    }

    public ValueType getType() {
        return ValueType.INT;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        NumVal numVal = (NumVal) o;
        return Objects.equals(value, numVal.value);
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}
