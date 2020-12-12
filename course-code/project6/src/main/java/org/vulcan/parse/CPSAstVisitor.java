package org.vulcan.parse;

import java.util.Arrays;

public class CPSAstVisitor implements AstVisitor<Ast> {
    private Ast kont;
    private IsSimpleVisitor isSimpleVisitor = new IsSimpleVisitor();
    private RshAstVisitor rshAstVisitor;
    private IndexBox indexBox;



    public CPSAstVisitor(IndexBox indexBox) {
        this.indexBox = indexBox;
        this.rshAstVisitor = new RshAstVisitor(indexBox);
        Variable x = new Variable("x");
        this.kont = new MapAst(new Variable[]{x},x);
    }

    public CPSAstVisitor(IndexBox indexBox, Ast kont) {
        this.indexBox = indexBox;
        this.rshAstVisitor = new RshAstVisitor(indexBox);
        this.kont = kont;
    }



    public IndexBox getIndex() {
        return indexBox;
    }



    @Override
    public Ast forBoolConstant(BoolConstant b) {
        return new App(kont, new Ast[]{b.accept(rshAstVisitor)});
    }

    @Override
    public Ast forIntConstant(IntConstant i) {
        return new App(kont, new Ast[]{i.accept(rshAstVisitor)});
    }

    @Override
    public Ast forNullConstant(NullConstant n) {
        return new App(kont, new Ast[]{n.accept(rshAstVisitor)});
    }

    @Override
    public Ast forVariable(Variable v) {
        return new App(kont, new Ast[]{v.accept(rshAstVisitor)});
    }

    @Override
    public Ast forPrimFun(PrimFun f) {
        return new App(kont,new Ast[]{f.accept(rshAstVisitor)});
    }

    //这里被当成primFun应用，需要特殊处理
    @Override
    public Ast forUnOpApp(UnOpApp u) {
        if(u.accept(isSimpleVisitor)){
            return new App(kont,new Ast[]{u.accept(rshAstVisitor)});
        }
        Op unOp = u.getRator();
        Ast arg = u.getArg();
        Variable var = new Variable(":"+ indexBox.getAndAddIndex());
        return (new Let(new Def[]{new Def(var,arg)},new UnOpApp(unOp,var))).accept(this);
    }

    @Override
    public Ast forBinOpApp(BinOpApp b) {
        if(b.accept(isSimpleVisitor)){
            return new App(kont,new Ast[]{b.accept(rshAstVisitor)});
        }
        Op binOp = b.getRator();
        Ast arg1 = b.getArg1();
        Ast arg2 = b.getArg2();
        Variable arg1Var = new Variable(":"+ indexBox.getAndAddIndex());
        Variable arg2Var = new Variable(":"+ indexBox.getAndAddIndex());
        Def arg1Def = new Def(arg1Var,arg1);
        Def arg2Def = new Def(arg2Var,arg2);
        return (new Let(new Def[]{arg1Def,arg2Def},
                new BinOpApp(binOp,arg1Var,arg2Var))).accept(this);
    }

    @Override
    public Ast forApp(App a) {
        //// TODO: 2020/12/12

        Ast rator =  a.getRator();
        Ast[] args = a.getArgs();
        if(rator instanceof MapAst) {
            return forMapApp((MapAst) rator, args);
        }
        if(a.accept(isSimpleVisitor)){
            return new App(kont,new Ast[]{a});
        }
        if(rator.accept(isSimpleVisitor)){
            if(Arrays.stream(a.getArgs()).allMatch(arg -> arg.accept(isSimpleVisitor))){
                return forAppSimple(rator,args);
            }
            return forSimpleApp(rator,args);
        }
        //命名
        Def[] defs = new Def[args.length+1];
        Ast[] newArgs = new Ast[args.length];
        Variable newRator = new Variable(":"+ indexBox.getAndAddIndex());
        Variable tempVar;
        defs[0] = new Def(newRator,rator);
        for (int i = 0; i < args.length; i++) {
           tempVar = new Variable(":"+ indexBox.getAndAddIndex());
           newArgs[i] = tempVar;
           defs[i+1] = new Def(tempVar,args[i]);
        }
        return (new Let(defs,new App(newRator,newArgs))).accept(this);
    }

    private Ast forMapApp(MapAst mapAst,Ast[] args){
        if(args.length == 0){
            return new App(kont,new Ast[]{mapAst.getBody()});
        }

        Variable[] params = mapAst.getVars();
        Ast body = mapAst.getBody();
        Def[] defs = new Def[params.length];
        for (int i = 0; i < args.length; i++) {
            defs[i] = new Def(params[i],args[i]);
        }
        return (new Let(defs,body)).accept(this);
    }

    private Ast forAppSimple(Ast simpleAst,Ast[] simpleArgs){
        Ast rshedAst = simpleAst.accept(rshAstVisitor);
        int argsLen = simpleArgs.length;
        Ast[] args = new Ast[argsLen+1];
        for (int i = 0; i < argsLen; i++) {
            args[i] = simpleArgs[i].accept(rshAstVisitor);
        }
        args[argsLen] = kont;
        return new App(rshedAst,args);
    }
    private Ast forSimpleApp(Ast simpleAst,Ast[] args){
        int argsLen = args.length;
        Def[] defs = new Def[argsLen];
        Ast[] newArgs = new Ast[argsLen];
        Variable tempVar;
        for(int i = 0; i <argsLen;i++){
            tempVar = new Variable(":"+ indexBox.getAndAddIndex());
            newArgs[i] = tempVar;
            defs[i] = new Def(tempVar,args[i]);
        }
        return (new Let(defs,new App(simpleAst,newArgs))).accept(this);
    }

    @Override
    public Ast forMap(MapAst m) {
        return new App(kont,new Ast[]{m.accept(rshAstVisitor)});
    }

    @Override
    public Ast forIf(If i) {
        if(i.accept(isSimpleVisitor)){
            return new App(kont,new Ast[]{i.accept(rshAstVisitor)});
        }
        Ast test = i.getTest();
        Ast conseq = i.getConseq();
        Ast alt = i.getAlt();
        if(test.accept(isSimpleVisitor)){
            return new If(test.accept(rshAstVisitor),
                    conseq.accept(this),
                    alt.accept(this));
        }
        Variable var = new Variable(":"+ indexBox.getAndAddIndex());
        Def def = new Def(var,test);
        return (new Let(new Def[]{def},new If(var,conseq,alt))).accept(this);
    }

    @Override
    public Ast forLet(Let l) {
        if(l.accept(isSimpleVisitor)){
            return new App(kont,new Ast[]{l.accept(rshAstVisitor)});
        }
        Def[] defs = l.getDefs();
        Ast body = l.getBody();
        if(defs.length ==1){
            Def def = defs[0];
            Variable var = def.lhs();
            Ast defBody = def.rhs();
            if(defBody.accept(isSimpleVisitor)){
                return new Let(
                        new Def[]{new Def(var,defBody.accept(rshAstVisitor))}
                        ,body.accept(this));
            }
            MapAst newMapAst = new MapAst(new Variable[]{var},body.accept(this));
            return defBody.accept(new CPSAstVisitor(indexBox,newMapAst));
        }
        Def firstDef = defs[0];
        Variable var = firstDef.lhs();
        Ast defBody = firstDef.rhs();
        if(defBody.accept(isSimpleVisitor)){
            Def newDef = new Def(var,defBody.accept(rshAstVisitor));
            Ast newLet = (new Let(Arrays.copyOfRange(defs,1,defs.length),body)).accept(this);
            return new Let(new Def[]{newDef},newLet);
        }
        Ast newLet = (new Let(Arrays.copyOfRange(defs,1,defs.length),body)).accept(this);
        MapAst newMapAst = new MapAst(new Variable[]{var},newLet);
        return defBody.accept(new CPSAstVisitor(indexBox,newMapAst));
    }

    @Override
    public Ast forBlock(Block b) {
        if(b.accept(isSimpleVisitor)){
            return new App(kont,new Ast[]{b.accept(rshAstVisitor)});
        }
        Ast[] blocks = b.getStates();
        Def[] defs = new Def[blocks.length];
        Variable tempVar;
        for (int i = 0; i < blocks.length; i++){
            tempVar = new Variable(":" + indexBox.getAndAddIndex());
            defs[i] = new Def(tempVar,blocks[i]);
        }
        Variable lastVar = defs[blocks.length - 1].lhs();
        return (new Let(defs,lastVar)).accept(this);
    }

    @Override
    public Ast forLetRec(LetRec ml) {
        if(ml.accept(isSimpleVisitor)){
            return new App(kont,new Ast[]{ml.accept(rshAstVisitor)});
        }
        MapDef[] mapDefs = ml.getDefs();
        Ast body = ml.getBody();
        MapDef[] newDefs = new MapDef[mapDefs.length];
        MapDef tempMapDef;
        for (int i = 0; i < mapDefs.length; i++){
            tempMapDef = mapDefs[i];
            newDefs[i] = new MapDef(tempMapDef.lhs(), tempMapDef.rhs().accept(rshAstVisitor));
        }
        return new LetRec(newDefs,body.accept(this)) ;
    }

    @Override
    public Ast forLetcc(Letcc letcc) {
        Variable tempVar1 = new Variable(":" + indexBox.getAndAddIndex());
        Variable tempVar2 = new Variable(":" + indexBox.getAndAddIndex());
        Def newDef = new Def(letcc.getVar(),
                new MapAst(
                        new Variable[]{tempVar1,tempVar2},
                        new App(kont,new Ast[]{tempVar1})));
        Ast newBody = letcc.getBody().accept(this);
        return new Let(new Def[]{newDef},newBody);
    }
}
