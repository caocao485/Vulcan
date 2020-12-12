package org.vulcan.eval;

import org.vulcan.parse.EvalException;

import java.util.HashMap;

import static org.vulcan.eval.value.Variable.VOID;

/**
 * @author Think
 */
public class Env<K,V> {
    private HashMap<K, V> frame;
    private Env<K,V> fatherEnv;

    public Env() {

    }

    @SuppressWarnings("unchecked")
    private Env(final HashMap<K, V> frame, final Env<K, V> fatherEnv) {
        this.frame =frame;
        this.fatherEnv = fatherEnv;
    }

    public static <K, V> Env<K,V> extendEnv(final HashMap<K, V>  frame, final Env<K,V> fatherEnv) {
        return new Env<>(frame, fatherEnv);
    }


    public HashMap<K, V> getFrame() {
        return frame;
    }

    public void setFrame(HashMap<K, V> frame) {
        this.frame = frame;
    }

    public Env<K, V> getFatherEnv() {
        return fatherEnv;
    }

    public void setFatherEnv(Env<K, V> fatherEnv) {
        this.fatherEnv = fatherEnv;
    }

    /**
     * // TODO: 2020/11/23
     * @param var
     * @param value
     */
    public void setValueByVar(final K var, final V value) {

    }


    public boolean hasVariable(K var){
        if(this.frame != null && this.frame.containsKey(var)){
            return true;
        }else{
            return this.fatherEnv != null && this.fatherEnv.hasVariable(var);
        }
    }

    @SuppressWarnings("unchecked")
    public  V lookup(final K var) {
        V value = null;
        if (this.frame != null) {
            value = this.frame.get(var);
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

