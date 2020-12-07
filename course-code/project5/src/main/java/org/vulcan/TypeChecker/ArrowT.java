package org.vulcan.TypeChecker;

import java.util.Arrays;
import java.util.Objects;

public class ArrowT implements JType{
    private JType[] domainType;
    private JType rangeType;

    public ArrowT(JType[] domainType, JType rangeType) {
        this.domainType = domainType;
        this.rangeType = rangeType;
    }

    public JType[] getDomainType() {
        return domainType;
    }

    public void setDomainType(JType[] domainType) {
        this.domainType = domainType;
    }

    public JType getRangeType() {
        return rangeType;
    }

    public void setRangeType(JType rangeType) {
        this.rangeType = rangeType;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ArrowT jarrow = (ArrowT) o;
        return Arrays.equals(domainType, jarrow.domainType) &&
                Objects.equals(rangeType, jarrow.rangeType);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(rangeType);
        result = 31 * result + Arrays.hashCode(domainType);
        return result;
    }

    @Override
    public String toString() {
        return "ArrowT{" +
                "domainType=" + Arrays.toString(domainType) +
                ", rangeType=" + rangeType +
                '}';
    }
}
