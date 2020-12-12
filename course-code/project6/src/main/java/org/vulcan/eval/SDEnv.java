package org.vulcan.eval;

import org.vulcan.eval.value.JamVal;
import org.vulcan.parse.EvalException;
import org.vulcan.parse.sd.Pair;

import static org.vulcan.eval.value.Variable.VOID;

public class SDEnv {
    private JamVal[]  frame;
    private SDEnv fatherEnv;

    public SDEnv(){

    }

    public SDEnv(JamVal[] frame, SDEnv fatherEnv) {
        this.frame = frame;
        this.fatherEnv = fatherEnv;
    }

    public static  SDEnv extendEnv(final JamVal[] frame, final SDEnv fatherEnv) {
        return new SDEnv(frame, fatherEnv);
    }

    public static JamVal lookup(SDEnv sdEnv, Pair pair){
        int level = pair.getLevel();
        int offset = pair.getOffset();
        if(level < 0 || offset < 0){
            throw new EvalException(pair,"pair cannot be negative");
        }
        JamVal value;
        SDEnv currenEnv = sdEnv;
        while(level !=0){
            currenEnv  = currenEnv.getFatherEnv();
            if(currenEnv == null){
                throw new EvalException(currenEnv,"env cannot be empty");
            }
            level--;
        }
        value = currenEnv.getFrame()[offset];
        if(value == VOID){
            throw new EvalException(pair,"could refer to a uninitialized variable");
        }
        return value;
    }

    public JamVal[] getFrame() {
        return frame;
    }

    public void setFrame(JamVal[] frame) {
        this.frame = frame;
    }

    public SDEnv getFatherEnv() {
        return fatherEnv;
    }

    public void setFatherEnv(SDEnv fatherEnv) {
        this.fatherEnv = fatherEnv;
    }


}
