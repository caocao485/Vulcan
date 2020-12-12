package org.vulcan.eval.value;

public class Variable implements JamVal {

    private Variable(){

    }

    public static final Variable VOID = new Variable();

    @Override
    public <T> T accept(JamValVisitor<T> jamValVisitor) {
        return jamValVisitor.forVoid(this);
    }
}
