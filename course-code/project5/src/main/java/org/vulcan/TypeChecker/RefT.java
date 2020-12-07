package org.vulcan.TypeChecker;

import java.util.Objects;

public class RefT implements JType {

    private JType refType;

    public RefT(JType refType) {
        this.refType = refType;
    }

    public JType getRefType() {
        return refType;
    }

    public void setRefType(JType refType) {
        this.refType = refType;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        RefT jref = (RefT) o;
        return Objects.equals(refType, jref.refType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(refType);
    }

    @Override
    public String toString() {
        return "RefT{" +
                "refType=" + refType +
                '}';
    }
}
