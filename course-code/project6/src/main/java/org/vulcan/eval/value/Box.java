package org.vulcan.eval.value;

import java.util.Objects;

public class Box implements JamVal{
    private JamVal value;

    public Box(JamVal value) {
        this.value = value;
    }

    public JamVal getValue() {
        return value;
    }

    public Unit setBox(JamVal value) {
        this.value = value;
        return Unit.UNIT;
    }

    @Override
    @SuppressWarnings("unchecked")
    public <T> T accept(JamValVisitor<T> jamValVisitor) {
        return jamValVisitor.forBox( this);
    }

    @Override
    public String toString() {
        return "(ref " +
                "" + value +
                ')';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        return false;
    }


    public ValueType getType() {
        return ValueType.BOX;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value);
    }
}
