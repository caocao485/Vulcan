package org.vulcan.parse;

import org.vulcan.eval.Env;
import org.vulcan.eval.SyntaxException;
import org.vulcan.eval.value.*;
import org.vulcan.eval.value.JamVal;
import org.vulcan.eval.value.Void;


import java.util.HashMap;


import static org.vulcan.eval.value.Void.VOID;

public class CheckAstVisitor implements AstVisitor<JamVal> {
    private final Env<Void> env;

    public CheckAstVisitor(Env<Void> env) {
        this.env = env;
    }

    @Override
    public JamVal forBoolConstant(BoolConstant b) {
        return null;
    }

    @Override
    public JamVal forIntConstant(IntConstant i) {
        return null;
    }

    @Override
    public JamVal forNullConstant(NullConstant n) {
        return null;
    }

    @Override
    public JamVal forVariable(Variable v) {
        if(!env.hasVariable(v)){
            throw new SyntaxException("variable: " + v + "is a free variable");
        }
        return null;
    }

    @Override
    public JamVal forPrimFun(PrimFun f) {
        return null;
    }

    @Override
    public JamVal forUnOpApp(UnOpApp u) {
        u.getArg().accept(this);
        return null;
    }

    @Override
    public JamVal forBinOpApp(BinOpApp b) {
        b.getArg1().accept(this);
        b.getArg2().accept(this);
        return null;
    }

    @Override
    public JamVal forApp(App a) {
        a.getRator().accept(this);

        for (Ast ast : a.getArgs() ){
            ast.accept(this);
        }
        return null;
    }

    @Override
    public JamVal forMap(Map m) {
        final HashMap<Variable, Void> frame = new HashMap<>();
        //Set
        Variable[] params = m.getVars();
        for (int i = 0; i < params.length; i++) {
            for (int j = i + 1 ; j < params.length; j++) {
                if (params[i].equals(params[j])) {
                    throw new SyntaxException("duplicate elements in map: "+m);
                }
            }
        }
        for (Variable var : m.getVars() ){
            frame.put(var,VOID);
        }
        CheckAstVisitor newChecker = new CheckAstVisitor(Env.extendEnv(frame, this.env));
        m.getBody().accept(newChecker);
        return null;
    }

    @Override
    public JamVal forIf(If i) {
        i.getTest().accept(this);
        i.getConseq().accept(this);
        i.getAlt().accept(this);
        return null;
    }

    @Override
    public JamVal forLet(Let l) {
        final HashMap<Variable, Void> frame = new HashMap<>();
        final CheckAstVisitor newChecker = new CheckAstVisitor(Env.extendEnv(frame, this.env));
        Def[]  defs = l.getDefs();
        for (int i = 0; i < defs.length; i++) {
            for (int j = i + 1 ; j < defs.length; j++) {
                if (defs[i].lhs().equals(defs[j].lhs())) {
                    throw new SyntaxException("duplicate elements in let: "+l);
                }
            }
        }
        //for rec let
        for (final Def def : l.getDefs()) {
            frame.put(def.lhs(), VOID);
            def.rhs().accept(newChecker);
        }
        l.getBody().accept(newChecker);
        return null;
    }
}
