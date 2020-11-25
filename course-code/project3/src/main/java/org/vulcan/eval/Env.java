package org.vulcan.eval;

import org.vulcan.eval.value.JamVal;
import org.vulcan.parse.Variable;

import java.util.HashMap;

/**
 * @author Think
 */
public class Env<T extends JamVal> {
    private HashMap<Variable, T> frame;
    private Env<T> fatherEnv;

    public Env() {

    }

    private Env(final HashMap<Variable, T> frame, final Env<T> fatherEnv) {
        this.frame = frame;
        this.fatherEnv = fatherEnv;
    }

    public static <T extends JamVal> Env<T> extendEnv(final HashMap<Variable, T> frame, final Env<T> fatherEnv) {
        return new Env<>(frame, fatherEnv);
    }

    /**
     * // TODO: 2020/11/23
     * @param var
     * @param value
     */
    public void setValueByVar(final Variable var, final T value) {

    }

    @SuppressWarnings("unchecked")
    public <T> T lookup(final Variable var) {
        T value = null;
        if (this.frame != null) {
            value = (T) this.frame.get(var);
            if (value == null) {
                if (this.fatherEnv != null) {
                    return this.fatherEnv.lookup(var);
                } else {
                    return null;
                }
            }
        }
        return value;

    }

    @Override
    public String toString() {
        return "Env{" +
                "frame=" + frame +
                ", fatherEnv=" + fatherEnv +
                '}';
    }
}

