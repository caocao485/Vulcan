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
    private Boolean[] isRefs;
    private Ast body;
    private Env<T> env;

    public ClosureVal(final Variable[] vars, final Ast body, final Env<T> env) {

        this.vars = vars;
        this.body = body;
        this.env = env;
        this.isRefs = new Boolean[vars.length];
    }

    public ClosureVal(final Variable[] vars, final Boolean[] isRefs,final Ast body, final Env<T> env) {

        this.vars = vars;
        this.body = body;
        this.env = env;
        this.isRefs = isRefs;
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

    public Env<T> getEnv() {
        return env;
    }

    public void setEnv(Env<T> env) {
        this.env = env;
    }

    @Override
    public <S> S accept(final JamValVisitor<S> jamValVisitor) {
        return jamValVisitor.forClosureVal(this);
    }

    public ValueType getType() {
        return ValueType.CLOSURE;
    }

    public Boolean[] getIsRefs() {
        return isRefs;
    }

    public void setIsRefs(Boolean[] isRefs) {
        this.isRefs = isRefs;
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
                Arrays.equals(isRefs, that.isRefs) &&
                Objects.equals(body, that.body) &&
                Objects.equals(env, that.env);
    }

    @Override
    public int hashCode() {
        int result = Objects.hash(body, env);
        result = 31 * result + Arrays.hashCode(vars);
        result = 31 * result + Arrays.hashCode(isRefs);
        return result;
    }
}
