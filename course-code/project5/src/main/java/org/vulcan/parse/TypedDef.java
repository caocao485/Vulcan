package org.vulcan.parse;

import org.vulcan.TypeChecker.JType;

public class TypedDef extends Def {
    private JType jType;
    /**
     * Instantiates a new org.vulcan.parse.Def.
     *
     * @param l the l
     * @param r the r
     */
    public TypedDef(Variable l,JType jType, Ast r) {
        super(l, r);
        this.jType = jType;
    }

    public JType getJType() {
        return jType;
    }

    @Override
    public String toString() {
        return lhs() +
                ":" + jType +
                ":=" +  rhs() + ";";
    }
}
