package org.vulcan.parse;

import org.vulcan.TypeChecker.*;
import org.vulcan.eval.Env;
import org.vulcan.eval.value.ClosureVal;
import org.vulcan.eval.value.JamVal;

import java.util.HashMap;

import static org.vulcan.TypeChecker.boolT.JBOOL_TYPE;
import static org.vulcan.TypeChecker.intT.JINT_TYPE;
import static org.vulcan.TypeChecker.UnitT.JUNIT_TYPE;


public class TypeCheckVisitor implements AstVisitor<JType> {
    private Env<JType> env;

    public TypeCheckVisitor(Env<JType> env) {
        this.env = env;
    }

    @Override
    public JType forBoolConstant(BoolConstant b) {
        return JBOOL_TYPE;
    }

    @Override
    public JType forIntConstant(IntConstant i) {
        return JINT_TYPE;
    }

    @Override
    public JType forNullConstant(NullConstant n) {
        return new ListT(n.getJType());
    }

    @Override
    public JType forVariable(Variable v) {
        final JType vType = this.env.lookup(v);
        if (vType == null) {
            throw new TypeException(v + " free variable");
        }
        return vType;
    }

    @Override
    public JType forPrimFun(PrimFun f) {
        return null;
    }

    @Override
    public JType forUnOpApp(UnOpApp u) {
        final Op op = u.getRator();
        if (!op.isUnOp()) {
            throw new TypeException(op + "error unop");
        }
        final JType argType = u.getArg().accept(this);
        switch (op.getSymbol()) {
            case "~":
                if(!(argType instanceof boolT)){
                    throw new TypeException(u.getArg() + "'s result type are not bool");
                }
                return JBOOL_TYPE;
            case "+":
            case "-":
                if (!(argType instanceof intT)) {
                    throw new TypeException(u.getArg()+ "'s result type are not int");
                }
                return JINT_TYPE;
            case "ref":
                return new RefT(argType);
            case "!":
                if (!(argType instanceof RefT)) {
                    throw new TypeException(u.getArg()+ "'s result type are not jref");
                }
                return ((RefT)argType).getRefType();
            default:
                throw new TypeException(u.getArg()+ "'s result type are unkown");
        }
    }

  @Override
  public JType forBinOpApp(BinOpApp b) {
      JType result = JBOOL_TYPE;
      final Op op = b.getRator();
      if (!op.isBinOp()) {
          throw new TypeException(op + "error Binop operator");
      }
      final JType arg1Type = b.getArg1().accept(this);
      final JType arg2Type = b.getArg2().accept(this);
      switch (op.getSymbol()) {
          case "=":
          case "!=":
              if (!arg1Type.equals(arg2Type)) {
                  throw new TypeException(arg1Type + "are not matched type " + arg2Type);
              }
              break;
          case "&":
          case "|":
              if (!(arg1Type instanceof boolT)) {
                  throw new TypeException(arg1Type + "are not matched type bool;");
              }
              if (!(arg2Type instanceof boolT)) {
                  throw new TypeException(arg2Type + "are not matched type bool;");
              }
              break;
          case "<-":
              if (!(arg1Type instanceof RefT)) {
                  throw new TypeException(arg1Type + "are not matched type JRef;");
              }
              JType arg1RefType = ((RefT) arg1Type).getRefType();
              if (!arg2Type.equals(arg1RefType)) {
                  throw new TypeException(arg1Type + "are not matched type " + arg2Type);
              }
              result = JUNIT_TYPE;
              break;
          case "+":
          case "-":
          case "/":
          case "*":
              if (!(arg1Type instanceof intT)) {
                  throw new TypeException(arg1Type + "are not matched type int;");
              }
              if (!(arg2Type instanceof intT)) {
                  throw new TypeException(arg2Type + "are not matched type int;");
              }
              result = JINT_TYPE;
              break;
          default:
              if (!(arg1Type instanceof intT)) {
                  throw new TypeException(arg1Type + "are not matched type int;");
              }
              if (!(arg2Type instanceof intT)) {
                  throw new TypeException(arg2Type + "are not matched type int;");
              }
              break;
      }
      return result;
  }

    @Override
    public JType forApp(App a) {
        Ast rator =  a.getRator();
        if(rator instanceof PrimFun){
            //cons  null? cons? first rest
            switch(((PrimFun)rator).getName()){
                case "cons":
                    if (a.getArgs().length != 2) {
                        throw new TypeException(a + " error number of arguments");
                    }
                    final JType arg1 = a.getArgs()[0].accept(this);
                    final JType arg2 = a.getArgs()[1].accept(this);
                    if(!(arg2 instanceof ListT)){
                        throw new TypeException(arg2 + " are not type JList");
                    }
                    JType arg2ElementType = ((ListT)arg2).getElementType();
                    if(!arg2ElementType.equals(arg1)){
                        throw new TypeException(arg1 + " are not match type "+arg2ElementType);
                    }
                    return arg2;
                case "null?":
                case "cons?":
                    if (a.getArgs().length != 1) {
                        throw new TypeException(a + " error number of arguments");
                    }
                    final JType arg = a.getArgs()[0].accept(this);
                    if(!(arg instanceof ListT)){
                        throw new TypeException(arg + " are not type JList");
                    }
                    return JBOOL_TYPE;
                case "first":
                    if (a.getArgs().length != 1) {
                        throw new TypeException(a + " error number of arguments");
                    }
                    final JType argListType = a.getArgs()[0].accept(this);
                    if(!(argListType instanceof ListT)){
                        throw new TypeException(argListType + " are not type JList");
                    }
                    return ((ListT)argListType).getElementType();
                case "rest":
                    if (a.getArgs().length != 1) {
                        throw new TypeException(a + " error number of arguments");
                    }
                    final JType restListType = a.getArgs()[0].accept(this);
                    if(!(restListType instanceof ListT)){
                        throw new TypeException(restListType + " are not type JList");
                    }
                    return restListType;
                default :
                    throw new TypeException(a + " error primitive function");
            }
        }
        final JType ratorType = a.getRator().accept(this);
        if(!(ratorType instanceof ArrowT)){
            throw new TypeException(ratorType + " are not type ArrowT");
        }
        JType[] domainTypes = ((ArrowT)ratorType).getDomainType();
        JType rangeType = ((ArrowT)ratorType).getRangeType();
        final Ast[] args = a.getArgs();
        if (domainTypes.length != args.length) {
            throw new TypeException(domainTypes +  "the number of arguments are not match" + args);
        }
        for (int i = 0; i < domainTypes.length; i++) {
            JType argType = args[i].accept(this);
            if(!domainTypes[i].equals(argType)){
                throw new TypeException(domainTypes +  "parameter type are not match" + argType);
            };
        }
        return rangeType;
    }

    @Override
    public JType forMap(Map m) {
        TypedMap typedMap = (TypedMap)m;
        JType[] domainTypes = typedMap.getJTypes();
        Variable[] vars = typedMap.getVars();
        final HashMap<Variable, JType> frame = new HashMap<>();
        final TypeCheckVisitor newVisitor =
                new TypeCheckVisitor(Env.extendEnv(frame, env));
        for (int i = 0; i < domainTypes.length; i++) {
            frame.put(vars[i],domainTypes[i]);
        }
        JType rangeType = typedMap.getBody().accept(newVisitor);
        return new ArrowT(domainTypes,rangeType);
    }

    @Override
    public JType forIf(If i) {
        if(!i.getTest().accept(this).equals(JBOOL_TYPE)){
            throw new TypeException(i+" type mismatch expected boolType");
        }
        JType jType = i.getConseq().accept(this);
        if(!jType.equals(i.getAlt().accept(this))){
            throw new TypeException(i + " type mismatch");
        }
        return jType;
    }

    @Override
    public JType forLet(Let l) {
        final HashMap<Variable, JType> frame = new HashMap<>();
        final TypeCheckVisitor newVisitor =
                new TypeCheckVisitor(Env.extendEnv(frame, env));
        for (final Def def : l.getDefs()) {
            TypedDef typedDef = (TypedDef)def;
            frame.put(typedDef.lhs(), typedDef.getJType());
            JType rhsType = typedDef.rhs().accept(newVisitor);
            if(!typedDef.getJType().equals(rhsType)){
                throw new TypeException(typedDef.getJType()+ " type mismatch " + rhsType);
            };
        }


        final JType resultType = l.getBody().accept(newVisitor);
        return resultType;
    }

    @Override
    public JType forBlock(Block b) {
        JType resultType = JUNIT_TYPE;
        Ast[] states = b.getStates();
        for(int i=0;i < states.length;i++) {
            if(i == states.length - 1){
                resultType = states[i].accept(this);
            }
            states[i].accept(this);
        }
        return resultType;
    }


}


