package org.vulcan;

import org.vulcan.eval.Interpreter;

import java.io.StringReader;
import java.util.Arrays;
import java.util.function.Function;

/**
 * @author Think
 */
public class Main {

    interface RecursiveFunction<F> extends Function<RecursiveFunction<F>, F> {
    }

    public static void main(String[] argv) {
    String exp =
        "let susp? := map l to cons?(l) & function?(first(l));\n"
            + " makeSusp := map f to cons(f, null);\n"
            + "in let block2 := map x,y to y;\n"
            + "           fo := map prom to if susp?(prom) then (first(prom))() else prom;\n"
            + "            Y := map f to\n"
            + "                   let g := map x to f(x(x));\n"
            + "                   in g(g);\n"
            + "   in let MAPSTREAM := map mapStream to\n"
            + "                          map f,l to let fol := fo(l);\n"
            + "                                     in if (fol = null) then null\n"
            + "                                     else cons(f(first(fol)), makeSusp(map  to mapStream(f, rest(fol))));\n"
            + "             FILTER := map filter to\n"
            + "                          map p,l to let fol := fo(l);\n"
            + "                                     in if (fol = null) then null\n"
            + "                                        else if p(first(fol)) then filter(p, rest(fol))\n"
            + "                                        else cons(first(fol), makeSusp(map  to filter(p, rest(fol))));\n"
            + "            divides := map a,b to (((b / a) * a) = b);\n"
            + "            INITSEG := map initSeg to\n"
            + "                          map l,n to if (n <= 0) then null\n"
            + "                                     else let fol := fo(l);\n"
            + "                                          in cons(first(fol), initSeg(rest(fol), (n - 1)));\n"
            + "       in let PRIMES := map primes to\n"
            + "                          map l to let fol := fo(l);\n"
            + "                                   in let l1 := (Y(FILTER))(map x to divides(first(fol), x), rest(fol));\n"
            + "                                      in cons(first(fol), makeSusp(map  to primes(l1)));\n"
            + "             ODDNUMS := map oddNums to\n"
            + "                           map  to cons(3, makeSusp(map  to (Y(MAPSTREAM))(map i to (i + 2), oddNums())));\n"
            + "          in (Y(INITSEG))(((Y(PRIMES))((Y(ODDNUMS))())), 20)";
        final Interpreter interp = new Interpreter(new StringReader(exp));
        System.out.println("argv = " + interp.callByNameLazyCons().toString());

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

    public static <A, B> Function<A, B> generateY(Function<Function<A, B>, Function<A, B>> f) {
        RecursiveFunction<Function<A, B>> g = x -> f.apply(y -> x.apply(x).apply(y)
        );
        return g.apply(g);
    }
}

