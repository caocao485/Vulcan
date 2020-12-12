package org.vulcan.eval;

import org.vulcan.eval.value.JamVal;
import org.vulcan.eval.value.JamValVisitor;
import org.vulcan.parse.Ast;

import java.util.Objects;

public class SDClosure implements JamVal {
    private int paramNum;
    private Ast body;
    private SDEnv env;

    public SDClosure(int paramNum, Ast body, SDEnv env) {
        this.paramNum = paramNum;
        this.body = body;
        this.env = env;
    }

    public int getParamNum() {
        return paramNum;
    }

    public void setParamNum(int paramNum) {
        this.paramNum = paramNum;
    }

    public Ast getBody() {
        return body;
    }

    public void setBody(Ast body) {
        this.body = body;
    }

    public SDEnv getEnv() {
        return env;
    }

    public void setEnv(SDEnv env) {
        this.env = env;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SDClosure sdClosure = (SDClosure) o;
        return paramNum == sdClosure.paramNum && Objects.equals(body, sdClosure.body) && Objects.equals(env, sdClosure.env);
    }

    @Override
    public int hashCode() {
        return Objects.hash(paramNum, body, env);
    }

    @Override
    public <T> T accept(JamValVisitor<T> jamValVisitor) {
        return null;
    }
}
