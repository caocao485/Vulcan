package org.vulcan;

import org.vulcan.eval.Interpreter;

import java.io.StringReader;
import java.util.Arrays;
import java.util.Scanner;
import java.util.function.Function;

/**
 * @author Think
 */
public class Main {

    interface RecursiveFunction<F> extends Function<RecursiveFunction<F>, F> {
    }

    public static void main(String[] argv) {
        while(true) {
            try{
                System.out.print("> ");
                Scanner in = new Scanner(System.in);
                String inputString = in.nextLine();
                if("exit()".equals(inputString)){
                    break;
                }
                final Interpreter interp1 = new Interpreter(new StringReader(inputString));
                System.out.println("<=> " + interp1.CpsSdEval().toString());
            }catch (Exception e) {
                e.printStackTrace();
            }

        }

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

    public static <A, B> Function<A, B> generateY(Function<Function<A, B>, Function<A, B>> f) {
        RecursiveFunction<Function<A, B>> g = x -> f.apply(y -> x.apply(x).apply(y)
        );
        return g.apply(g);
    }
}

