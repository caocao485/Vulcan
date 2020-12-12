package org.vulcan.parse;



import org.vulcan.eval.Env;
import org.vulcan.parse.sd.Pair;
import org.vulcan.parse.sd.SDLet;
import org.vulcan.parse.sd.SDLetRec;
import org.vulcan.parse.sd.SDMap;

import java.util.Arrays;
import java.util.HashMap;

public class ToSDAstVisitor implements AstVisitor<Ast> {
    private Env<Variable,Integer> env;


    public ToSDAstVisitor(Env<Variable, Integer> env) {
        this.env = env;
    }

    public static Pair sdLookup(Env<Variable,Integer> env, Variable var){
        int level = 0;
        int offset;
        Env<Variable, Integer> currentEnv = env;
        while(!currentEnv.getFrame().containsKey(var)){
            level++;
            currentEnv = currentEnv.getFatherEnv();
            if(currentEnv == null){
                throw new ParseException(currentEnv+"empty env");
            }
        }
        offset = currentEnv.getFrame().get(var);
        return new Pair(level,offset);
    }
    @Override
    public Ast forBoolConstant(BoolConstant b) {
        return b;
    }

    @Override
    public Ast forIntConstant(IntConstant i) {
        return i;
    }

    @Override
    public Ast forNullConstant(NullConstant n) {
        return n;
    }

    @Override
    public Ast forVariable(Variable v) {
        return sdLookup(env,v);
    }

    @Override
    public Ast forPrimFun(PrimFun f) {
        return f;
    }

    @Override
    public Ast forUnOpApp(UnOpApp u) {
        return new UnOpApp(u.getRator(),u.getArg().accept(this));
    }

    @Override
    public Ast forBinOpApp(BinOpApp b) {
        return new BinOpApp(b.getRator(),b.getArg1().accept(this),b.getArg2().accept(this));
    }

    @Override
    public Ast forApp(App a) {
        return new App(a.getRator().accept(this),
                Arrays.stream(a.getArgs())
                        .map(arg -> arg.accept(this))
                        .toArray(Ast[]::new));
    }

    @Override
    public Ast forMap(MapAst m) {
        HashMap<Variable,Integer> frame = new HashMap<>();
        Variable[] vars = m.getVars();
        int varSize = vars.length;
        for(int i = 0; i < varSize; i++){
            frame.put(vars[i],i);
        }
        Env<Variable, Integer> newEnv = Env.extendEnv(frame,env);
        Ast newBody = m.getBody().accept(new ToSDAstVisitor(newEnv));
        return new SDMap(varSize,newBody);
    }

    @Override
    public Ast forIf(If i) {
        return new If(i.getTest().accept(this),
                i.getConseq().accept(this),
                i.getAlt().accept(this));
    }

    @Override
    public Ast forLet(Let l) {
       Variable[] vars =  Arrays.stream(l.getDefs())
                .map(Def::lhs)
                .toArray(Variable[]::new);
       Ast[] rhsBodys =  Arrays.stream(l.getDefs())
                .map(def -> def.rhs().accept(this))
                .toArray(Ast[]::new);
       Ast body = l.getBody();

       HashMap<Variable,Integer> frame = new HashMap<>();
       int varSize = vars.length;
       for(int i = 0; i < varSize; i++){
            frame.put(vars[i],i);
       }
       Env<Variable, Integer> newEnv = Env.extendEnv(frame,env);
       Ast newBody = body.accept(new ToSDAstVisitor(newEnv));

       return new SDLet(rhsBodys,newBody);
    }



    @Override
    public Ast forBlock(Block b) {
        return new Block(
                Arrays.stream(b.getStates())
                        .map(ast -> ast.accept(this))
                        .toArray(Ast[]::new));
    }

    @Override
    public Ast forLetRec(LetRec ml) {
        Variable[] vars =  Arrays.stream(ml.getDefs())
                .map(MapDef::lhs)
                .toArray(Variable[]::new);
        Ast[] rhsBodys =  Arrays.stream(ml.getDefs())
                .map(MapDef::rhs)
                .toArray(Ast[]::new);
        Ast body = ml.getBody();
        SDMap[] newBodys = new SDMap[rhsBodys.length];

        HashMap<Variable,Integer> frame = new HashMap<>();
        Env<Variable, Integer> newEnv = Env.extendEnv(frame,env);
        ToSDAstVisitor newVisitor = new ToSDAstVisitor(newEnv);
        int varSize = vars.length;
        for(int i = 0; i < varSize; i++){
            frame.put(vars[i],i);
            newBodys[i] = (SDMap) rhsBodys[i].accept(newVisitor);
        }

        Ast newBody = body.accept(newVisitor);

        return new SDLetRec(newBodys,newBody);
    }


    @Override
    public Ast forLetcc(Letcc letcc) {
        HashMap<Variable,Integer> frame = new HashMap<>();
        frame.put(letcc.getVar(),0);
        Env<Variable, Integer> newEnv = Env.extendEnv(frame,env);
        Ast newBody = letcc.getBody().accept(new ToSDAstVisitor(newEnv));
        return new Letcc(new Variable("[*1*]"),newBody);
    }
}
