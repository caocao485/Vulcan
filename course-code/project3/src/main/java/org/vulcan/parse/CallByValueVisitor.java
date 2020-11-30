package org.vulcan.parse;

import org.vulcan.eval.BinaryOperator;
import org.vulcan.eval.Env;
import org.vulcan.eval.value.*;

import java.util.HashMap;

import static org.vulcan.eval.value.BoolVal.FALSE_VALUE;
import static org.vulcan.eval.value.BoolVal.TRUE_VALUE;
import static org.vulcan.eval.value.NullVal.NULL_VALUE;
import static org.vulcan.eval.value.Void.VOID;

/**
 * @author Think
 */
public class CallByValueVisitor implements AstVisitor<JamVal> {

    private Env<JamVal> env;
    private final HashMap<String, BinaryOperator<JamVal, NumVal>> binaryOpertors = new HashMap<>();

    private final boolean shouldListCached;
    private final boolean isLazyCons;


    public CallByValueVisitor(Env<JamVal> env) {
        this.env = env;
        this.isLazyCons = false;
        this.shouldListCached = false;
        this.initEnv();
    }

    public CallByValueVisitor(Env<JamVal> env,  boolean isLazyCons,boolean shouldListCached) {
        this.env = env;
        this.shouldListCached = shouldListCached;
        this.isLazyCons = isLazyCons;
        this.initEnv();
    }

    private void initEnv() {
        extractBinaryOperator(this.binaryOpertors);
    }

    static void extractBinaryOperator(HashMap<String, BinaryOperator<JamVal, NumVal>> binaryOpertors) {
        binaryOpertors.put("+", (NumVal a, NumVal b) -> {
            return new NumVal(a.getValue() + b.getValue());
        });
        binaryOpertors.put("-", (NumVal a, NumVal b) -> {
            return new NumVal(a.getValue() - b.getValue());
        });
        binaryOpertors.put("*", (NumVal a, NumVal b) -> {
            return new NumVal(a.getValue() * b.getValue());
        });
        binaryOpertors.put("/", (NumVal a, NumVal b) -> {
            if (b.getValue() == 0) {
                throw new EvalException(b, "denominator should not be zero");
            }
            return new NumVal(a.getValue() / b.getValue());
        });
        binaryOpertors.put(">", (NumVal a, NumVal b) -> {
            return (a.getValue() > b.getValue()) ? TRUE_VALUE : FALSE_VALUE;
        });
        binaryOpertors.put(">=", (NumVal a, NumVal b) -> {
            return (a.getValue() >= b.getValue()) ? TRUE_VALUE : FALSE_VALUE;
        });
        binaryOpertors.put("<", (NumVal a, NumVal b) -> {
            return (a.getValue() < b.getValue()) ? TRUE_VALUE : FALSE_VALUE;
        });
        binaryOpertors.put("<=", (NumVal a, NumVal b) -> {
            return (a.getValue() <= b.getValue()) ? TRUE_VALUE : FALSE_VALUE;
        });
    }

    @Override
    public JamVal forBoolConstant(final BoolConstant b) {
        return b.getValue() ? TRUE_VALUE : FALSE_VALUE;
    }

    @Override
    public JamVal forIntConstant(final IntConstant i) {
        return new NumVal(i.getValue());
    }

    @Override
    public JamVal forNullConstant(final NullConstant n) {
        return NULL_VALUE;
    }

    @Override
    public JamVal forVariable(final Variable v) {
        final JamVal value = this.env.lookup(v);
        if (value == null) {
            throw new EvalException(v, "free variable");
        }
        return value;
    }

    @Override
    public JamVal forPrimFun(final PrimFun f) {
        return PrimFunVal.getFunValue(f.getName());
    }

    @Override
    public JamVal forUnOpApp(final UnOpApp u) {
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
            default:
                throw new EvalException(u, "error unop expression");
        }
    }

    @Override
    public JamVal forBinOpApp(final BinOpApp b) {
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
    public JamVal forApp(final App a) {
        final JamVal rator = a.getRator().accept(this);
        if (!(rator instanceof ClosureVal) &&
                !(rator instanceof PrimFunVal)) {
            throw new EvalException(a, "rator is not a function");
        }
        if (rator instanceof PrimFunVal) {
            return this.forPrimApp((PrimFunVal) rator, a);
        } else {
            return this.forClosureVal((ClosureVal) rator, a);
        }
    }


    private JamVal forClosureVal(final ClosureVal rator, final App a) {
        final Variable[] params = rator.getVars();
        final Ast[] args = a.getArgs();
        if (params.length != args.length) {
            throw new EvalException(a, "the number of arguments are not match");
        }
        final HashMap<Variable, JamVal> frame = new HashMap<>();
        //for recursion definition
        for (int i = 0; i < params.length; i++) {
            frame.put(params[i], VOID);
        }

        for (int i = 0; i < params.length; i++) {
            frame.put(params[i], args[i].accept(this));
        }
        final CallByValueVisitor newVisitor =
                new CallByValueVisitor(Env.extendEnv(frame, rator.getEnv()), isLazyCons,shouldListCached);
        final JamVal result = rator.getBody().accept(newVisitor);
        return result;
    }

    private JamVal forPrimApp(final PrimFunVal rator, final App a) {
        if ("cons".equals(rator.getFunValue())) {
            if (a.getArgs().length != 2) {
                throw new EvalException(a, "error number of arguments");
            }
            if (isLazyCons) {
                return new LazyConsVal(a.getArgs()[0], a.getArgs()[1], this, shouldListCached);
            }
            final JamVal arg1 = a.getArgs()[0].accept(this);
            final JamVal arg2 = a.getArgs()[1].accept(this);
            if (!(arg2 instanceof ListVal)) {
                throw new EvalException(a, "arg2 are not a list");
            }
            return new ConsVal(arg1, (ListVal) arg2);
        }
        if (a.getArgs().length != 1) {
            throw new EvalException(a, "error number of arguments");
        }
        return this.forOneArgApp(rator, a.getArgs()[0].accept(this), a);

    }

    private JamVal forOneArgApp(final PrimFunVal rator, final JamVal arg, final App a) {
        switch (rator.getFunValue()) {
            case "cons?":
                if (isLazyCons) {
                    return (arg instanceof LazyConsVal) ? TRUE_VALUE : FALSE_VALUE;
                } else {
                    return (arg instanceof ConsVal) ? TRUE_VALUE : FALSE_VALUE;
                }
            case "null?":
                if (isLazyCons) {
                    return (arg instanceof NullVal) ? TRUE_VALUE : FALSE_VALUE;
                } else {
                    return (arg instanceof NullVal) ? TRUE_VALUE : FALSE_VALUE;
                }
            case "number?":
                return (arg instanceof NumVal)
                        ? TRUE_VALUE : FALSE_VALUE;
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
                if (isLazyCons) {
                    if (arg instanceof LazyConsVal) {
                        return ((LazyConsVal) arg).getFirstValue();
                    } else {
                        throw new EvalException(a, "a is not a list or a is null");
                    }
                } else {
                    if (arg instanceof ConsVal) {
                        return ((ConsVal) arg).getFirst();
                    } else {
                        throw new EvalException(a, "a is not a list or a is null");
                    }
                }
            case "rest":
                if (isLazyCons) {
                    if (arg instanceof LazyConsVal) {
                        return ((LazyConsVal) arg).getRestValue();
                    } else {
                        throw new EvalException(a, "a is not a list or a is null");
                    }
                }
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
    public JamVal forMap(final Map m) {
        return new ClosureVal<>(m.getVars(), m.getBody(), env);
    }

    @Override
    public JamVal forIf(final If i) {
        final JamVal testV = i.getTest().accept(this);
        if (!(testV instanceof BoolVal)) {
            throw new EvalException(i, "test result are not BoolVal");
        }
        return (testV == TRUE_VALUE) ?
                i.getConseq().accept(this) : i.getAlt().accept(this);
    }

    @Override
    public JamVal forLet(final Let letAst) {
        final HashMap<Variable, JamVal> frame = new HashMap<>();
        final CallByValueVisitor newVisitor =
                new CallByValueVisitor(Env.extendEnv(frame, env), isLazyCons,shouldListCached);
        for (final Def def : letAst.getDefs()) {
            frame.put(def.lhs(), VOID);
        }
        for (final Def def : letAst.getDefs()) {
            frame.put(def.lhs(), def.rhs().accept(newVisitor));
        }

        final JamVal result = letAst.getBody().accept(newVisitor);
        return result;
    }
}
