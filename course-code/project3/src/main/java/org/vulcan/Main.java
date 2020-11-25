package org.vulcan;

import java.util.function.Function;

/**
 * @author Think
 */
public class Main {

    interface RecursiveFunction<F> extends Function<RecursiveFunction<F>, F> {
    }

    public static void main(String[] argv) {
        runY();
    }

    public static void runY() {
        Function<Integer, Integer> fib = Y(f -> n ->
                (n <= 2)
                        ? 1
                        : (f.apply(n - 1) + f.apply(n - 2))
        );
        Function<Integer, Integer> fac = Y(f -> n ->
                (n <= 1)
                        ? 1
                        : (n * f.apply(n - 1))
        );
        System.out.println("fib(10) = " + fib.apply(10));
        System.out.println("fac(10) = " + fac.apply(10));
    }


    public static <A, B> Function<A, B> Y(Function<Function<A, B>, Function<A, B>> ff) {
        return ff.apply(a -> Y(ff).apply(a));
    }
}

