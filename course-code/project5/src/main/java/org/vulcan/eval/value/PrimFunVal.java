package org.vulcan.eval.value;


import java.util.HashMap;

/**
 * @author Think
 */
public class PrimFunVal implements JamVal {

    private String funValue;
    private static final HashMap<String, PrimFunVal> WORD2_VALUE = new HashMap<>();

    static {
        //PrimFunVal.WORD2_VALUE.put("number?", new PrimFunVal("number?"));
        //PrimFunVal.WORD2_VALUE.put("function?", new PrimFunVal("function?"));
        //PrimFunVal.WORD2_VALUE.put("list?", new PrimFunVal("list?"));
        PrimFunVal.WORD2_VALUE.put("null?", new PrimFunVal("null?"));
        PrimFunVal.WORD2_VALUE.put("cons?", new PrimFunVal("cons?"));
        //PrimFunVal.WORD2_VALUE.put("arity", new PrimFunVal("arity"));
        PrimFunVal.WORD2_VALUE.put("cons", new PrimFunVal("cons"));
        PrimFunVal.WORD2_VALUE.put("first", new PrimFunVal("first"));
        PrimFunVal.WORD2_VALUE.put("rest", new PrimFunVal("rest"));
    }

    private PrimFunVal(final String funValue) {
        this.funValue = funValue;
    }

    public static PrimFunVal getFunValue(final String name) {
        return PrimFunVal.WORD2_VALUE.get(name);
    }


    public String getFunValue() {
        return this.funValue;
    }

    public void setFunValue(final String funValue) {
        this.funValue = funValue;
    }

    @Override
    public String toString() {
        return this.funValue;
    }

    @Override
    public <T> T accept(final JamValVisitor<T> jamValVisitor) {
        return null;
    }

    public ValueType getType() {
        return ValueType.PRIM_FUN;
    }


}
