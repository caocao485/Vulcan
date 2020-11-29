package org.vulcan.eval;

import org.vulcan.parse.EvalException;
import org.vulcan.parse.Variable;

import java.util.HashMap;

import static org.vulcan.eval.value.Void.VOID;

/**
 * @author Think
 */
public class Env<T> {
    private HashMap<Variable, T> frame;
    private Env<T> fatherEnv;

    public Env() {

    }

    @SuppressWarnings("unchecked")
    private Env(final HashMap<Variable, T> frame, final Env<T> fatherEnv) {
        this.frame =frame;
        this.fatherEnv = fatherEnv;
    }

    public static <T> Env<T> extendEnv(final HashMap<Variable, T> frame, final Env<T> fatherEnv) {
        return new Env<>(frame, fatherEnv);
    }



    /**
     * // TODO: 2020/11/23
     * @param var
     * @param value
     */
    public void setValueByVar(final Variable var, final T value) {

    }


    public boolean hasVariable(Variable var){
        if(this.frame != null && this.frame.containsKey(var)){
            return true;
        }else{
            return this.fatherEnv != null && this.fatherEnv.hasVariable(var);
        }
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
            }else if(value == VOID){
                throw new EvalException(var,"could refer to a uninitialized variable");
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

