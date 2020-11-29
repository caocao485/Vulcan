package org.vulcan.eval.value;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

// TODO: 2020/11/21 可能有问题
/**
 * @author Think
 */
public class ConsVal implements ListVal {
    private JamVal first;
    /**
     * null means null ListVal
     **/
    private ListVal rest;

    public ConsVal(final JamVal first, final ListVal rest) {
        this.first = first;
        this.rest = rest;
    }

    public JamVal getFirst() {
        return this.first;
    }

    public void setFirst(final JamVal first) {
        this.first = first;
    }

    public ListVal getRest() {
        return this.rest;
    }

    public void setRest(final ListVal rest) {
        this.rest = rest;
    }

    @Override
    @SuppressWarnings("unchecked")
    public List<JamVal> getValues() {
        final List<JamVal> list = new ArrayList<>();
        if (this.first != null) {
            list.add(this.first);
        }
        if (ValueType.NULL != this.rest.getType()) {
            list.addAll(this.rest.getValues());
        }
        return list;
    }

    @Override
    public String toString() {
        final StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append("(");

        final List<JamVal> list = this.getValues();
        final Iterator<JamVal> it = list.iterator();
        if (!it.hasNext()) {
            return stringBuilder.append(")").toString();
        }
        for (; ; ) {
            final JamVal e = it.next();
            stringBuilder.append(e == this ? "(this Collection)" : e);
            if (!it.hasNext()) {
                return stringBuilder.append(')').toString();
            }
            stringBuilder.append(' ');
        }

    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ConsVal consVal = (ConsVal) o;
        return Objects.equals(first, consVal.first) &&
                Objects.equals(rest, consVal.rest);
    }

    @Override
    public int hashCode() {
        return Objects.hash(first, rest);
    }

    @Override
    public ValueType getType() {
        return ValueType.LIST;
    }

    @Override
    public <S> S accept(JamValVisitor<S> jamValVisitor) {
        return jamValVisitor.forConsVal(this);
    }
}
