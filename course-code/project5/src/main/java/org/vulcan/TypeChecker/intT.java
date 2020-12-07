package org.vulcan.TypeChecker;

public class intT implements JType {
    private intT(){

    }

    public static final intT JINT_TYPE = new intT();

    @Override
    public String toString() {
        return "intT{}";
    }
}
