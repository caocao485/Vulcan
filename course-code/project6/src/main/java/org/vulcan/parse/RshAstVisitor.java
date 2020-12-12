package org.vulcan.parse;

import java.util.Arrays;

public class RshAstVisitor implements AstVisitor<Ast> {

    private IndexBox indexBox;

    public RshAstVisitor(IndexBox indexBox) {
        this.indexBox = indexBox;
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
        return v;
    }

    @Override
    public Ast forPrimFun(PrimFun f) {
        Variable x = new Variable("x");
        Variable y = new Variable("y");
        Variable k = new Variable("k");
        Ast tempBody;
        switch(f.getName()){
            case "arity":
                tempBody = new App(k,
                        new Ast[]{new BinOpApp(new Op("-", true, true),
                        new App(f,new Ast[]{x}),new IntConstant(1))});
                return new MapAst(new Variable[]{x,k},tempBody);
            case "cons":
                tempBody = new App(k,
                        new Ast[]{
                                new App(f,new Ast[]{x,y})
                        });
                return new MapAst(new Variable[]{x,y,k},tempBody);
            default:
                tempBody = new App(k,
                        new Ast[]{
                                new App(f,new Ast[]{x})
                        });
                return new MapAst(new Variable[]{x,k},tempBody);
        }
    }

    //参数是基本表达式的应用
    @Override
    public Ast forUnOpApp(UnOpApp u) {
        return new UnOpApp(u.getRator(),
                u.getArg().accept(this));
    }

    @Override
    public Ast forBinOpApp(BinOpApp b) {
        return new BinOpApp(b.getRator(),
                b.getArg1().accept(this),
                b.getArg2().accept(this));
    }

    @Override
    public Ast forApp(App a) {
        Ast rator = a.getRator();
        Ast[] newArgs;
        if (a.getArgs().length==0){
            newArgs = a.getArgs();
        }else{
            newArgs = Arrays.stream(a.getArgs()).map(arg -> arg.accept(this)).toArray(Ast[]::new);
        }

        if(rator instanceof PrimFun){
            switch(((PrimFun)rator).getName()){
                case "arity":
                    return new BinOpApp(new Op("-", true, true),
                            new App(rator,newArgs),new IntConstant(1));
                default:
                    return new App(rator,newArgs);
            }
        }
        return new App(rator,newArgs);
    }

    @Override
    public Ast forMap(MapAst m) {
        Variable v = new Variable(":"+ indexBox.getAndAddIndex());
        Variable[] vars = m.getVars();
        Ast body = m.getBody();
        int varLen = vars.length;
        Variable[] newVars = new Variable[varLen +1];
        System.arraycopy(vars, 0, newVars, 0, varLen);
        newVars[varLen] = v;
        return new MapAst(newVars,body.accept(new CPSAstVisitor(indexBox,v)));
    }

    @Override
    public Ast forIf(If i) {
        return new If(i.getTest().accept(this),
                i.getConseq().accept(this),
                i.getAlt().accept(this));
    }

    @Override
    public Ast forLet(Let l) {
        Def[] defs = Arrays.stream(l.getDefs())
                .map(def -> new Def(def.lhs(),def.rhs().accept(this)))
                .toArray(Def[]::new);
        return new Let(defs,l.getBody().accept(this));
    }

    @Override
    public Ast forBlock(Block b) {
        return new Block(Arrays.stream(b.getStates())
                .map(ast -> ast.accept(this)).toArray(Ast[]::new));
    }

    @Override
    public Ast forLetRec(LetRec ml) {
        MapDef[] mapDefs = Arrays.stream(ml.getDefs())
                .map(mapDef -> new MapDef(mapDef.lhs(),mapDef.rhs().accept(this)))
                .toArray(MapDef[]::new);
        return new LetRec(mapDefs,ml.getBody().accept(this));
    }

    @Override
    public Ast forLetcc(Letcc letcc) {
        return null;
    }
}
