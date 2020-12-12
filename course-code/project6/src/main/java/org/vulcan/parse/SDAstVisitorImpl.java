package org.vulcan.parse;

import org.vulcan.eval.BinaryOperator;
import org.vulcan.eval.Env;
import org.vulcan.eval.SDClosure;
import org.vulcan.eval.SDEnv;
import org.vulcan.eval.value.*;
import org.vulcan.parse.*;
import org.vulcan.parse.sd.*;

import java.util.Arrays;
import java.util.HashMap;

import static org.vulcan.eval.value.BoolVal.FALSE_VALUE;
import static org.vulcan.eval.value.BoolVal.TRUE_VALUE;
import static org.vulcan.eval.value.NullVal.NULL_VALUE;
import static org.vulcan.eval.value.Variable.VOID;
import static org.vulcan.parse.CallByValueVisitor.extractBinaryOperator;

public class SDAstVisitorImpl  implements SDAstVisitor<JamVal> {
    private SDEnv env;
    private final HashMap<String, BinaryOperator<JamVal, NumVal>> binaryOpertors
            = new HashMap<>();

    public SDAstVisitorImpl(SDEnv env){
        this.env = env;
        extractBinaryOperator(binaryOpertors);
    }

    @Override
    public JamVal forBoolConstant(BoolConstant b) {
        return b.getValue() ? TRUE_VALUE : FALSE_VALUE;
    }

    @Override
    public JamVal forIntConstant(IntConstant i) {
        return new NumVal(i.getValue());
    }

    @Override
    public JamVal forNullConstant(NullConstant n) {
        return NULL_VALUE;
    }

    @Override
    public JamVal forVariable(Variable v) {
        throw new EvalException(v,"unsupported operation");
    }

    @Override
    public JamVal forPrimFun(PrimFun f) {
        return PrimFunVal.getFunValue(f.getName());
    }

    @Override
    public JamVal forUnOpApp(UnOpApp u) {
        final Op op = u.getRator();
        if (!op.isUnOp()) {
            throw new EvalException(u, "error unop");
        }
        final JamVal num = u.getArg().accept(this);
        switch (op.getSymbol()) {
            case "~":
                if(!(num instanceof BoolVal)){
                    throw new EvalException(num,"~ expected an arg of type bool, but got " + num);
                }
                if (num == FALSE_VALUE) {
                    return TRUE_VALUE;
                } else {
                    return FALSE_VALUE;
                }
            case "+":
                if (!(num instanceof NumVal)) {
                    throw new EvalException(u, "arg is not number");
                } else {
                    return num;
                }
            case "-":
                if (!(num instanceof NumVal)) {
                    throw new EvalException(u, "arg is not number");
                } else {
                    return new NumVal(-((NumVal) num).getValue());
                }
            case "ref":
                return new Box(num);
            case "!":
                if (!(num instanceof Box)) {
                    throw new EvalException(u, "arg is not a box");
                } else {
                    return ((Box)num).getValue();
                }
            default:
                throw new EvalException(u, "error unop expression");
        }
    }

    @Override
    public JamVal forBinOpApp(BinOpApp b) {
        final Op op = b.getRator();
        if (!op.isBinOp()) {
            throw new EvalException(b, "error Binop operator");
        }
        final JamVal arg1Value = b.getArg1().accept(this);
        final JamVal arg2Value;
        switch (op.getSymbol()) {
            case "=":
                arg2Value = b.getArg2().accept(this);
                if ((arg1Value instanceof ClosureVal) |
                        (arg2Value instanceof ClosureVal)) {
                    return arg1Value == arg2Value ?
                            TRUE_VALUE : FALSE_VALUE;
                } else {
                    return (arg1Value.equals(arg2Value)) ?
                            TRUE_VALUE : FALSE_VALUE;
                }
            case "!=":
                arg2Value = b.getArg2().accept(this);
                if ((arg1Value instanceof ClosureVal) |
                        (arg2Value instanceof ClosureVal)) {
                    return arg1Value != arg2Value ? FALSE_VALUE : TRUE_VALUE;
                } else {
                    return !(arg1Value.equals(arg2Value)) ?
                            TRUE_VALUE : FALSE_VALUE;
                }
            case "&":
                if(!(arg1Value instanceof BoolVal)){
                    throw new EvalException(arg1Value,"~ expected an arg of type bool, but got " + arg1Value);
                }
                if (arg1Value != FALSE_VALUE) {
                    arg2Value =  b.getArg2().accept(this);
                    if(!(arg2Value instanceof BoolVal)){
                        throw new EvalException(arg2Value,"~ expected an arg of type bool, but got " + arg2Value);
                    }
                    return arg2Value;
                } else {
                    return FALSE_VALUE;
                }
            case "|":
                if(!(arg1Value instanceof BoolVal)){
                    throw new EvalException(arg1Value,"~ expected an arg of type bool, but got " + arg1Value);
                }
                if (arg1Value != FALSE_VALUE) {
                    return arg1Value;
                } else {
                    arg2Value =  b.getArg2().accept(this);
                    if(!(arg2Value instanceof BoolVal)){
                        throw new EvalException(arg2Value,"~ expected an arg of type bool, but got " + arg2Value);
                    }
                    return arg2Value;
                }
            case "<-":
                if(!(arg1Value instanceof Box)){
                    throw new EvalException(arg1Value,"<- expected an arg of type box, but got " + arg1Value);
                }
                arg2Value =  b.getArg2().accept(this);
                return ((Box)arg1Value).setBox(arg2Value);
            default:
                arg2Value = b.getArg2().accept(this);
                if (!(arg1Value instanceof NumVal)
                        | !(arg2Value instanceof NumVal)) {
                    throw new EvalException(b, "error arg type");
                }
                if (this.binaryOpertors.containsKey(op.getSymbol())) {
                    return this.binaryOpertors.get(op.getSymbol())
                            .binaryOperate((NumVal) arg1Value, (NumVal) arg2Value);
                } else {
                    throw new EvalException(b, "unsupported BinaryOperate operation");
                }
        }
    }

    @Override
    public JamVal forApp(App a) {
        final JamVal rator = a.getRator().accept(this);
        if (!(rator instanceof ClosureVal) &&
                !(rator instanceof PrimFunVal) &&
                !(rator instanceof SDClosure)) {
            throw new EvalException(a, "rator is not a function");
        }
        if (rator instanceof PrimFunVal) {
            return this.forPrimApp((PrimFunVal) rator, a);
        } else {
            return this.forClosureVal((SDClosure) rator, a);
        }
    }

    private JamVal forClosureVal(final SDClosure rator, final App a) {
        final JamVal[] frame = Arrays
                .stream(a.getArgs())
                .map(arg -> arg.accept(this))
                .toArray(JamVal[]::new);

        final SDAstVisitorImpl newVisitor =
                new SDAstVisitorImpl(SDEnv.extendEnv(frame, rator.getEnv()));
        return rator.getBody().accept(newVisitor);
    }

    private JamVal forPrimApp(final PrimFunVal rator, final App a) {
        if ("cons".equals(rator.getFunValue())) {
            if (a.getArgs().length != 2) {
                throw new EvalException(a, "error number of arguments");
            }
            final JamVal arg1 = a.getArgs()[0].accept(this);
            final JamVal arg2 = a.getArgs()[1].accept(this);
            if (!(arg2 instanceof ListVal)) {
                throw new EvalException(a, "arg2 are not a list");
            }
            return new ConsVal(arg1, (ListVal) arg2);
        }
        if ("ref?".equals(rator.getFunValue())){
            if (a.getArgs().length != 1) {
                throw new EvalException(a, "error number of arguments");
            }
            final JamVal arg = a.getArgs()[0].accept(this);
            return (arg instanceof Box)?TRUE_VALUE : FALSE_VALUE;
        }
        if (a.getArgs().length != 1) {
            throw new EvalException(a, "error number of arguments");
        }
        return this.forOneArgApp(rator, a.getArgs()[0].accept(this), a);

    }

    private JamVal forOneArgApp(final PrimFunVal rator, final JamVal arg, final App a) {
        switch (rator.getFunValue()) {
            case "cons?":
                return (arg instanceof ConsVal) ? TRUE_VALUE : FALSE_VALUE;
            case "null?":
                return (arg instanceof NullVal) ? TRUE_VALUE : FALSE_VALUE;
            case "number?":
                return (arg instanceof NumVal)
                        ? TRUE_VALUE : FALSE_VALUE;
            case "asBool":
                if(arg instanceof BoolVal){
                    return arg;
                }
                throw new EvalException(a, "argValue are not boolV");
            case "function?":
                return (arg instanceof PrimFunVal |
                        arg instanceof ClosureVal)
                        ? TRUE_VALUE : FALSE_VALUE;
            case "arity":
                if (arg instanceof PrimFunVal) {
                    if ("cons".equals(((PrimFunVal) arg).getFunValue())) {
                        return new NumVal(2);
                    } else {
                        return new NumVal(1);
                    }
                } else if (arg instanceof ClosureVal) {
                    return new NumVal(((ClosureVal) arg).getVars().length);
                } else {
                    throw new EvalException(a, "arg is not a function");
                }
            case "list?":
                return (arg instanceof ListVal) ?
                        TRUE_VALUE : FALSE_VALUE;
            case "first":
                if (arg instanceof ConsVal) {
                        return ((ConsVal) arg).getFirst();
                    } else {
                        throw new EvalException(a, "a is not a list or a is null");
                    }
            case "rest":
                if (arg instanceof ConsVal) {
                    return ((ConsVal) arg).getRest();
                } else {
                    throw new EvalException(a, "a is not a list or a is null");
                }
            default:
                throw new EvalException(a, "unsupported function");
        }
    }

    @Override
    public JamVal forMap(MapAst m) {
        throw new EvalException(m,"unsupported operation");
    }

    @Override
    public JamVal forIf(If i) {
        final JamVal testV = i.getTest().accept(this);
        if (!(testV instanceof BoolVal)) {
            throw new EvalException(i, "test result are not BoolVal");
        }
        return (testV == TRUE_VALUE) ?
                i.getConseq().accept(this) : i.getAlt().accept(this);

    }

    @Override
    public JamVal forLet(Let l) {
        throw new EvalException(l,"unsupported operation");
    }

    @Override
    public JamVal forBlock(Block b) {
        throw new EvalException(b,"unsupported operation");
    }

    @Override
    public JamVal forLetRec(LetRec ml) {
        throw new EvalException(ml,"unsupported operation");
    }

    @Override
    public JamVal forSDLet(SDLet sdLet) {
        final JamVal[] frame = Arrays
                .stream(sdLet.getAsts())
                .map(arg -> arg.accept(this))
                .toArray(JamVal[]::new);

        final SDAstVisitorImpl newVisitor =
                new SDAstVisitorImpl(SDEnv.extendEnv(frame, env));

        return sdLet.getBody().accept(newVisitor);
    }

    @Override
    public JamVal forSDLetRec(SDLetRec sdLetrec) {
        SDMap[] sdMaps = sdLetrec.getMaps();
        final JamVal[] frame = new JamVal[sdMaps.length];
        for (int i = 0; i < sdMaps.length; i++) {
            frame[i] = VOID;
        }
        final SDAstVisitorImpl newVisitor =
                new SDAstVisitorImpl(SDEnv.extendEnv(frame, env));

        for (int i = 0; i < sdMaps.length; i++) {
            frame[i] = sdMaps[i].accept(newVisitor);
        }

        return sdLetrec.getBody().accept(newVisitor);
    }

    @Override
    public JamVal forSDMap(SDMap sdMap) {
        return new SDClosure(sdMap.getParamLen(),sdMap.getBody(),env);
    }

    @Override
    public JamVal forPair(Pair pair) {
        return SDEnv.lookup(env,pair);
    }

    @Override
    public JamVal forLetcc(Letcc letcc) {
        throw new EvalException("letcc","not supported in non-cps code");
    }
}
