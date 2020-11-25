package org.vulcan.parse;

import org.vulcan.eval.BinaryOperator;
import org.vulcan.eval.Env;
import org.vulcan.eval.value.*;

import java.util.HashMap;

import static org.vulcan.eval.value.BoolVal.FALSE_VALUE;
import static org.vulcan.eval.value.BoolVal.TRUE_VALUE;
import static org.vulcan.eval.value.NullVal.NULL_VALUE;

/**
 * @author Think
 */
public class OtherCallVisitor implements AstVisitor<JamVal> {

    private final Env<JamVal> env;
    private final HashMap<String, BinaryOperator<JamVal, NumVal>> binaryOpertors = new HashMap<>();
    private final boolean shouldCached;

    public static JamVal forceEval(final JamVal thunk) {
        if (thunk instanceof Thunk) {
            return ((Thunk) thunk).value();
        } else {
            return thunk;
        }
    }


    public OtherCallVisitor(Env<JamVal> env, boolean shouldCached) {
        this.env = env;
        this.shouldCached = shouldCached;
        this.initEnv();
    }

    private void initEnv() {
        CallByValueVisitor.extractBinaryOperator(this.binaryOpertors);
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
        final JamVal num = OtherCallVisitor.forceEval(u.getArg().accept(this));
        switch (op.getSymbol()) {
            case "~":
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
        final JamVal arg1Value = OtherCallVisitor.forceEval(b.getArg1().accept(this));
        final JamVal arg2Value;
        switch (op.getSymbol()) {
            case "=":
                arg2Value = OtherCallVisitor.forceEval(b.getArg2().accept(this));
                if ((arg1Value instanceof ClosureVal) |
                        (arg2Value instanceof ClosureVal)) {
                    return arg1Value == arg2Value ?
                            TRUE_VALUE : FALSE_VALUE;
                } else {
                    return (arg1Value.equals(arg2Value)) ?
                            TRUE_VALUE : FALSE_VALUE;
                }
            case "!=":
                arg2Value = OtherCallVisitor.forceEval(b.getArg2().accept(this));
                if ((arg1Value instanceof ClosureVal) |
                        (arg2Value instanceof ClosureVal)) {
                    return arg1Value != arg2Value ? FALSE_VALUE : TRUE_VALUE;
                } else {
                    return !(arg1Value.equals(arg2Value)) ?
                            TRUE_VALUE : FALSE_VALUE;
                }
            case "&":
                if (arg1Value != FALSE_VALUE) {
                    return OtherCallVisitor.forceEval(b.getArg2().accept(this));
                } else {
                    return FALSE_VALUE;
                }
            case "|":
                if (arg1Value != FALSE_VALUE) {
                    return arg1Value;
                } else {
                    return OtherCallVisitor.forceEval(b.getArg2().accept(this));
                }
            default:
                arg2Value = OtherCallVisitor.forceEval(b.getArg2().accept(this));
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
        final JamVal rator = OtherCallVisitor.forceEval(a.getRator().accept(this));
        if (!(rator instanceof ClosureVal) &&
                !(rator instanceof PrimFunVal)) {
            throw new EvalException(a, "rator is not a function");
        }
        if (rator instanceof PrimFunVal) {
            return this.forPrimApp((PrimFunVal) rator, a);
        } else {
            return this.forClosureValEval((ClosureVal) rator, a);
        }
    }


    private JamVal forClosureValEval(final ClosureVal rator, final App a) {
        final Variable[] params = rator.getVars();
        final Ast[] args = a.getArgs();
        if (params.length != args.length) {
            throw new EvalException(a, "the number of arguments are not match");
        }
        final HashMap<Variable, JamVal> frame = new HashMap<>();
        for (int i = 0; i < params.length; i++) {
            final int finalI = i;
            frame.put(params[i], new Thunk<JamVal>(() -> {
                return OtherCallVisitor.forceEval(args[finalI].accept(this));
            }, shouldCached));
        }

        final OtherCallVisitor newVisitor = new OtherCallVisitor(Env.extendEnv(frame, rator.getEnv()), shouldCached);
        final JamVal result = rator.getBody().accept(newVisitor);
        return result;
    }

    private JamVal forPrimApp(final PrimFunVal rator, final App a) {
        if ("cons".equals(rator.getFunValue())) {
            if (a.getArgs().length != 2) {
                throw new EvalException(a, "error number of arguments");
            }
            final JamVal arg1 = OtherCallVisitor.forceEval(a.getArgs()[0].accept(this));
            final JamVal arg2 = OtherCallVisitor.forceEval(a.getArgs()[1].accept(this));
            if (!(arg2 instanceof ListVal)) {
                throw new EvalException(a, "arg2 are not a list");
            }
            return new ConsVal(arg1, (ListVal) arg2);
        }
        if (a.getArgs().length != 1) {
            throw new EvalException(a, "error number of arguments");
        }
        return this.forOneArgApp(rator, OtherCallVisitor.forceEval(a.getArgs()[0].accept(this)), a);

    }

    private JamVal forOneArgApp(final PrimFunVal rator, final JamVal arg, final App a) {
        switch (rator.getFunValue()) {
            case "cons?":
                if (arg instanceof ConsVal) {
                    return TRUE_VALUE;
                } else {
                    return FALSE_VALUE;
                }
            case "null?":
                return (arg instanceof ConsVal)
                        ? TRUE_VALUE : FALSE_VALUE;
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
    public JamVal forMap(final Map m) {
        return new ClosureVal(m.getVars(), m.getBody(), env);
    }

    @Override
    public JamVal forIf(final If i) {
        final JamVal testV = OtherCallVisitor.forceEval(i.getTest().accept(this));
        if (!(testV instanceof BoolVal)) {
            throw new EvalException(i, "test result are not BoolVal");
        }
        return (testV == TRUE_VALUE) ?
                OtherCallVisitor.forceEval(i.getConseq().accept(this)) : OtherCallVisitor.forceEval(i.getAlt().accept(this));
    }

    @Override
    public JamVal forLet(final Let letAST) {
        final HashMap<Variable, JamVal> frame = new HashMap<>();
        for (final Def def : letAST.getDefs()) {
            frame.put(def.lhs(), new Thunk<JamVal>(() -> {
                return def.rhs().accept(this);
            }, shouldCached));
        }
        final OtherCallVisitor newVisitor = new OtherCallVisitor(Env.extendEnv(frame, env), shouldCached);
        final JamVal result = letAST.getBody().accept(newVisitor);
        return result;
    }


}
