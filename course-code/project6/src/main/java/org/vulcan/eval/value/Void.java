package org.vulcan.eval.value;

public class Void implements JamVal {

    private Void(){

    }

    public static final Void VOID = new Void();

    @Override
    public <T> T accept(JamValVisitor<T> jamValVisitor) {
        return jamValVisitor.forVoid(this);
    }
}
