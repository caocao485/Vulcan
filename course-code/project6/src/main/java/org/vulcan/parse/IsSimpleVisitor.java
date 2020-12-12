package org.vulcan.parse;

import java.util.Arrays;

public class IsSimpleVisitor implements AstVisitor<Boolean> {
    @Override
    public Boolean forBoolConstant(BoolConstant b) {
        return true;
    }

    @Override
    public Boolean forIntConstant(IntConstant i) {
        return true;
    }

    @Override
    public Boolean forNullConstant(NullConstant n) {
        return true;
    }

    @Override
    public Boolean forVariable(Variable v) {
        return true;
    }

    @Override
    public Boolean forPrimFun(PrimFun f) {
        return true;
    }

    @Override
    public Boolean forUnOpApp(UnOpApp u) {
        return u.getArg().accept(this);
    }

    @Override
    public Boolean forBinOpApp(BinOpApp b) {
        return b.getArg1().accept(this) && b.getArg2().accept(this);
    }

    @Override
    public Boolean forApp(App a) {

        //todo bug
        Ast rator = a.getRator();
        //boolean ratorSimple = a.getRator().accept(this);
        boolean argsSimple = Arrays.stream(a.getArgs()).allMatch(arg -> arg.accept(this));
        return ((rator instanceof PrimFun) || (rator instanceof MapAst) ) && argsSimple;
    }

    @Override
    public Boolean forMap(MapAst m) {
        return true;
    }

    @Override
    public Boolean forIf(If i) {
        return i.getTest().accept(this) &&
                i.getConseq().accept(this) &&
                i.getAlt().accept(this);
    }

    @Override
    public Boolean forLet(Let l) {
        boolean bodySimple = l.getBody().accept(this);
        boolean defSimple = Arrays.stream(l.getDefs()).allMatch(def -> def.rhs().accept(this));
        return bodySimple && defSimple;
    }

    @Override
    public Boolean forBlock(Block b) {
        return Arrays.stream(b.getStates()).allMatch(ast -> ast.accept(this));
    }

    @Override
    public Boolean forLetRec(LetRec ml) {
        return ml.getBody().accept(this);
    }

    @Override
    public Boolean forLetcc(Letcc letcc) {
        return false;
    }
}
