package org.vulcan.parse;

import org.vulcan.eval.Env;
import org.vulcan.eval.SyntaxException;

import java.util.Arrays;
import java.util.HashMap;

import static org.vulcan.parse.BoolConstant.FALSE;
import static org.vulcan.parse.BoolConstant.TRUE;


public class CheckAstVisitor implements AstVisitor<Ast> {
    private final Env<Variable, Variable> env;
    private final Integer layerNum;

    public CheckAstVisitor(Env<Variable, Variable> env) {
        this.env = env;
        this.layerNum = 1;
    }

    public CheckAstVisitor(Env<Variable, Variable> env, Integer layerNum) {
        this.env = env;
        this.layerNum = layerNum;
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
        if (!env.hasVariable(v)) {
            throw new SyntaxException("variable: " + v + " is a free variable");
        }
        return env.lookup(v);
    }

    @Override
    public Ast forPrimFun(PrimFun f) {
        return f;
    }

    @Override
    public Ast forUnOpApp(UnOpApp u) {
        Ast arg = u.getArg().accept(this);
        return new UnOpApp(u.getRator(), arg);
    }

    @Override
    public Ast forBinOpApp(BinOpApp b) {
        Ast arg1 = b.getArg1().accept(this);
        Ast arg2 = b.getArg2().accept(this);
        Op rator = b.getRator();
        switch(rator.getSymbol()){
            case "|":
                return new If(arg1,TRUE,
                        new App(new PrimFun("asBool"),
                                new Ast[]{arg2}));
            case "&":
                return new If(arg1,
                        new App(new PrimFun("asBool"),
                                new Ast[]{arg2}),
                        FALSE);
            default :
                return new BinOpApp(b.getRator(), arg1, arg2);
        }
    }

    @Override
    public Ast forApp(App a) {
        Ast rator = a.getRator().accept(this);

        return new App(rator,
                Arrays.stream(a.getArgs()).
                        map(ast -> ast.accept(this)).
                        toArray(Ast[]::new)
        );

    }

    @Override
    public Ast forMap(MapAst m) {
        final HashMap<Variable, Variable> frame = new HashMap<>();
        //Set
        Variable[] params = m.getVars();
        Variable[] newParams = new Variable[params.length];
        for (int i = 0; i < params.length; i++) {
            Variable param = params[i];
            if (frame.containsKey(param)) {
                throw new SyntaxException("duplicate elements in map: " + m);
            }
            String newName = param.getName() + ":" + layerNum;
            Variable newParam = new Variable(newName);
            newParams[i] = newParam;
            frame.put(param, newParam);
        }

        CheckAstVisitor newChecker = new CheckAstVisitor(Env.extendEnv(frame, this.env), layerNum + 1);
        Ast newBody = m.getBody().accept(newChecker);
        return new MapAst(newParams, newBody);
    }

    @Override
    public Ast forIf(If i) {
        Ast newTest = i.getTest().accept(this);
        Ast newConseq = i.getConseq().accept(this);
        Ast newAlt = i.getAlt().accept(this);
        return new If(newTest, newConseq, newAlt);
    }

    @Override
    public Ast forLet(Let l) {
        final HashMap<Variable, Variable> frame = new HashMap<>();
        Def[] defs = l.getDefs();
        Def[] newDefs = new Def[defs.length];
        for (int i = 0; i < defs.length; i++) {
            Def def = defs[i];
            Variable defName = def.lhs();
            Ast body = def.rhs();
            if (frame.containsKey(defName)) {
                throw new SyntaxException("duplicate elements in let: " + defName);
            }
            String newName = defName.getName() + ":" + layerNum;
            Variable newDefName = new Variable(newName);
            newDefs[i] = new Def(newDefName, body.accept(this));
            frame.put(defName, newDefName);
        }

        final CheckAstVisitor newChecker = new CheckAstVisitor(Env.extendEnv(frame, this.env), layerNum + 1);
        Ast body = l.getBody().accept(newChecker);
        return new Let(newDefs, body);
    }

    @Override
    public Ast forBlock(Block b) {
        Ast[] blocks = b.getStates();
        Ast[] newBlocks = new Ast[blocks.length];
        for (int i = 0; i < blocks.length; i++) {
            newBlocks[i] = blocks[i].accept(this);
        }
        return new Block(newBlocks);
    }

    @Override
    public Ast forLetRec(LetRec ml) {
        final HashMap<Variable, Variable> frame = new HashMap<>();
        MapDef[] defs = ml.getDefs();
        MapDef[] newDefs = new MapDef[defs.length];
        final CheckAstVisitor newChecker = new CheckAstVisitor(Env.extendEnv(frame, this.env), layerNum + 1);
        for (int i = 0; i < defs.length; i++) {
            MapDef def = defs[i];
            Variable defName = def.lhs();
            Ast body = def.rhs();
            if (frame.containsKey(defName)) {
                throw new SyntaxException("duplicate elements in letrec: " + defName);
            }
            String newName = defName.getName() + ":" + layerNum;
            Variable newDefName = new Variable(newName);
            frame.put(defName, newDefName);
            newDefs[i] = new MapDef(newDefName, body.accept(newChecker));
        }


        Ast body = ml.getBody().accept(newChecker);
        return new LetRec(newDefs, body);
    }

    @Override
    public Ast forLetcc(Letcc letcc) {
        final HashMap<Variable, Variable> frame = new HashMap<>();
        String newName = letcc.getVar().getName() + ":" + layerNum;
        Variable newDefName = new Variable(newName);
        frame.put(letcc.getVar(), newDefName);
        final CheckAstVisitor newChecker
                = new CheckAstVisitor(
                        Env.extendEnv(frame, this.env),
                layerNum + 1);
        Ast body = letcc.getBody().accept(newChecker);
        return new Letcc(newDefName,body);
    }

}
