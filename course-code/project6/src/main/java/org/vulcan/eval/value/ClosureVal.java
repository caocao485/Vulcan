package org.vulcan.eval.value;

import org.vulcan.eval.Env;
import org.vulcan.parse.Ast;
import org.vulcan.parse.Variable;

import java.util.Arrays;
import java.util.Objects;

/**
 * @author Think
 */
public class ClosureVal<T extends JamVal> implements JamVal {
    private Variable[] vars;
    private Ast body;
    private Env<Variable,T> env;

    public ClosureVal(final Variable[] vars, final Ast body, final Env<Variable,T> env) {

        this.vars = vars;
        this.body = body;
        this.env = env;
    }

    public Variable[] getVars() {
        return vars;
    }

    public void setVars(Variable[] vars) {
        this.vars = vars;
    }

    public Ast getBody() {
        return body;
    }

    public void setBody(Ast body) {
        this.body = body;
    }

    public Env<Variable,T> getEnv() {
        return env;
    }

    public void setEnv(Env<Variable,T> env) {
        this.env = env;
    }

    @Override
    public <S> S accept(final JamValVisitor<S> jamValVisitor) {
        return jamValVisitor.forClosureVal(this);
    }

    public ValueType getType() {
        return ValueType.CLOSURE;
    }

    @Override
    public String toString() {
        return "ClosureVal{" +
                "vars=" + Arrays.toString(vars) +
                ", body=" + body +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ClosureVal<?> that = (ClosureVal<?>) o;
        return Arrays.equals(vars, that.vars) &&
                Objects.equals(body, that.body) &&
                Objects.equals(env, that.env);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(body, env);
        result = 31 * result + Arrays.hashCode(vars);
        return result;
    }
}
