package org.vulcan.eval.value;

import org.vulcan.parse.Ast;
import org.vulcan.parse.AstVisitor;
import org.vulcan.parse.EvalException;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;



public class LazyConsVal implements ListVal {
  private Ast first;
  private JamVal firstValue;
  /** null means null ListVal */
  private Ast rest;
  private JamVal restValue;

  private AstVisitor<JamVal> valueVisitor;


  private boolean shouldCached;

  public LazyConsVal(Ast first, Ast rest, AstVisitor<JamVal> valueVisitor, boolean shouldCached) {
    this.first = first;
    this.rest = rest;
    this.valueVisitor = valueVisitor;
    this.shouldCached = shouldCached;
  }

  public static JamVal forceEval(final JamVal thunk) {
    JamVal value = thunk;
    while (value instanceof Thunk) {
      value = ((Thunk<JamVal>) value).value();
    }

    return value;
  }

  public JamVal getFirstValue() {
    if (this.shouldCached) {
      if (this.firstValue == null) {
        this.firstValue = forceEval(this.first.accept(valueVisitor));
      }
      return this.firstValue;
    } else {
      return forceEval(this.first.accept(valueVisitor));
    }
  }

  public void setFirst(final Ast first) {
    this.first = first;
  }

  public JamVal getRestValue() {
    if (this.shouldCached) {
      if (this.restValue == null) {
        this.restValue = forceEval(this.rest.accept(valueVisitor));
        if (!(restValue instanceof ListVal)) {
          throw new EvalException(rest, "rest are not a list");
        }
      }
      return this.restValue;
    } else {
      JamVal restV =  forceEval(this.rest.accept(valueVisitor));
      if (!(restV instanceof ListVal)) {
        throw new EvalException(rest, "rest are not a list");
      }
      return restV;
    }
  }

  public void setRest(final Ast rest) {
    this.rest = rest;
  }

  @Override
  @SuppressWarnings("unchecked")
  public List<JamVal> getValues() {
    final List<JamVal> list = new ArrayList<>();
    JamVal firstV = this.getFirstValue();
    if (firstV != null) {
      list.add(firstV);
    }

    ListVal restV = (ListVal)this.getRestValue();

    if (ValueType.NULL != restV.getType()) {
      list.addAll(((LazyConsVal)restV).getValues());
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
      LazyConsVal lazyConsVal = (LazyConsVal) o;
    return (Objects.equals(first, lazyConsVal.first)
            |Objects.equals(getFirstValue(), lazyConsVal.getFirstValue()))
            &&
            Objects.equals(rest, lazyConsVal.rest);
  }

  @Override
  public int hashCode() {
    return Objects.hash(first, rest);
  }

  @Override
  public ValueType getType() {
    return ValueType.LAZY_LIST;
  }

  @Override
  public <S> S accept(JamValVisitor<S> jamValVisitor) {
    return jamValVisitor.forLazyConVal(this);
  }
}
