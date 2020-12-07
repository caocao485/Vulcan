package org.vulcan.parse;

import org.vulcan.TypeChecker.JType;

public class TypedMap extends Map {
    private final JType[] jTypes;
    /**
     * Instantiates a new org.vulcan.parse.Map.
     *
     * @param v the v
     * @param b the b
     */
    public TypedMap(Variable[] v,  JType[] jTypes,Ast b) {
        super(v, b);
        this.jTypes = jTypes;
    }

    public JType[] getJTypes() {
        return jTypes;
    }
}
