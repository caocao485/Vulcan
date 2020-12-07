package org.vulcan.TypeChecker;

public class UnitT implements JType {
    private UnitT() {

    }

    public static final UnitT JUNIT_TYPE = new UnitT();

    @Override
    public String toString() {
        return "UnitT{}";
    }
}
