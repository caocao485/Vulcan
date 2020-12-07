package org.vulcan.TypeChecker;

public class boolT implements JType{
    private boolT() {

    }

    public static final boolT JBOOL_TYPE = new boolT();

    @Override
    public String toString() {
        return "boolT{}";
    }
}
