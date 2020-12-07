package org.vulcan.TypeChecker;


import java.util.Objects;

public class ListT implements JType {
    private JType elementType;

    public ListT(JType elementType) {
        this.elementType = elementType;
    }

    public JType getElementType() {
        return elementType;
    }

    public void setElementType(JType elementType) {
        this.elementType = elementType;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        ListT jlist = (ListT) o;
        return Objects.equals(elementType, jlist.elementType);
    }

    @Override
    public int hashCode() {
        return Objects.hash(elementType);
    }

    @Override
    public String toString() {
        return "ListT{" +
                "elementType=" + elementType +
                '}';
    }
}
