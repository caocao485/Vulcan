package org.vulcan.eval.value;

public class Unit implements JamVal {
    @Override
    public <T> T accept(JamValVisitor<T> jamValVisitor) {
        return null;
    }

    private Unit() {

    }

    @Override
    public String toString() {
        return "unit";
    }

    public static final Unit UNIT = new Unit();
}
