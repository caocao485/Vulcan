package org.vulcan.eval;

import junit.framework.TestCase;
import org.vulcan.parse.EvalException;
import org.vulcan.parse.ParseException;
import org.vulcan.parse.Parser;

import java.io.StringReader;


public class Assign2Test extends TestCase {

    public Assign2Test(final String name) {
        super(name);
    }

    /**
     * The following 3 check methods create an interpreter object with the
     * specified String as the program, invoke the respective evaluation
     * method (callByValue, callByName, callByNeed), and check that the
     * result matches the (given) expected output.  If the test fails,
     * the method prints a report as to which test failed and how many
     * points should be deducted.
     */

    private void valueCheck(final String name, final String answer, final String program) {
        final Interpreter interp = new Interpreter(new StringReader(program));
        TestCase.assertEquals("by-value " + name, answer, interp.callByValue().toString());
    }

    private void nameCheck(final String name, final String answer, final String program) {
        final Interpreter interp = new Interpreter(new StringReader(program));
        TestCase.assertEquals("by-name " + name, answer, interp.callByName().toString());
    }

    private void needCheck(final String name, final String answer, final String program) {
        final Interpreter interp = new Interpreter(new StringReader(program));
        TestCase.assertEquals("by-need " + name, answer, interp.callByNeed().toString());
    }

    public void nameLazyConsCheck(final String name, final String answer, final String program) {
        final Interpreter interp = new Interpreter(new StringReader(program));
        TestCase.assertEquals("by-need " + name, answer, interp.callByNameLazyCons().toString());
    }

    public void needLazyConsCheck(final String name, final String answer, final String program) {
        final Interpreter interp = new Interpreter(new StringReader(program));
        TestCase.assertEquals("by-need " + name, answer, interp.callByNeedLazyCons().toString());
    }


    private void allCheck(final String name, final String answer, final String program) {
        this.valueCheck(name, answer, program);
        this.nameCheck(name, answer, program);
        this.needCheck(name, answer, program);
        this.nameLazyConsCheck(name,answer,program);
        this.needLazyConsCheck(name,answer,program);
    }


    public void testNumberP() {
        try {
            final String output = "number?";
            final String input = "number?";
            this.allCheck("numberP", output, input);

        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("numberP threw " + e);
        }
    } //end of func


    public void testMathOp() {
        try {
            final String output = "18";
            final String input = "2 * 3 + 12";
            this.allCheck("mathOp", output, input);

        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("mathOp threw " + e);
        }
    } //end of func


    public void testParseException() {
        try {
            final String output = "haha";
            final String input = " 1 +";
            this.allCheck("parseException", output, input);

            TestCase.fail("parseException did not throw ParseException exception");
        } catch (final ParseException e) {
            //e.printStackTrace();

        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("parseException threw " + e);
        }
    } //end of func


    public void testEvalException() {
        try {
            final String output = "mojo";
            final String input = "1 + number?";
            this.allCheck("evalException", output, input);

            TestCase.fail("evalException did not throw EvalException exception");
        } catch (final EvalException e) {
            //e.printStackTrace();

        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("evalException threw " + e);
        }
    } //end of func


    public void testAppend() {
        try {
            final String output = "(1 2 3 1 2 3)";
            final String input = "let Y    := map f to              let g := map x to f(map z1,z2 to (x(x))(z1,z2));     in g(g);  APPEND := map ap to            map x,y to               if x = null then y else cons(first(x), ap(rest(x), y)); l      := cons(1,cons(2,cons(3,null))); in (Y(APPEND))(l,l)";
            this.allCheck("append", output, input);

        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("append threw " + e);
        }
    } //end of func

    public void testFact() {
        try {
            final String output = "6";
            final String input = "let Y    := map f to   let g := map x to f(map z to (x(x))(z));  in g(g); FACT := map f to  map n to if n = 0 then 1 else n * f(n - 1); in (Y(FACT))(3)";
            this.allCheck("fact", output, input);

        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("fact threw " + e);
        }
    }


    public void testNameFact() {
        try {
            final String output = "6";
            final String input = "let Y    := map f to   let g := map x to f(x(x)); in g(g); FACT := map f to map n to if n = 0 then 1 else n * f(n - 1); in (Y(FACT))(3)";
            this.needCheck("Fact-need", output, input);
            this.nameCheck("Fact-name", output, input);
            this.nameLazyConsCheck("Fact-lazycons-name", output, input);
            this.needLazyConsCheck("Fact-lazycons-need", output, input);
            //valueCheck("fib-value", output, input);//stackoverflow

        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("fact name threw " + e);
        }
    }


    public void testFib() {
        try {
            final String output
                    = "((0 1) (1 1) (2 2) (3 3) (4 5) (5 8) (6 13) (7 21) (8 34) (9 55) (10 89))";
            final String input =
                    "let Y      := map f to " +
                            "                let g := map x to f(x(x)); " +
                            "                in g(g); " +
                            "      pair := map x,y to cons(x, cons(y, null));" +
                            "   FIBHELP := map fibhelp to map k,fn,fnm1 to if k = 0 then fn else fibhelp(k - 1, fn + fnm1, fn); " +
                            "in let FFIB := map ffib to map n to if n = 0 then 1 else (Y(FIBHELP))(n - 1,1,1); " +
                            "   in let FIBS := map fibs to map k,l to " +
                            "                    let fibk := (Y(FFIB))(k);" +
                            "                    in if k >= 0 then fibs(k - 1, cons(pair(k,fibk), l)) else l; " +
                            "      in (Y(FIBS))(10, null)";
            //valueCheck("fib-value", output, input);//stackoverflow
            this.needCheck("fib-need", output, input);
            this.nameCheck("fib-name", output, input);
            this.nameLazyConsCheck("fib-lazycons-name", output, input);
            this.needLazyConsCheck("fib-lazycons-need", output, input);
        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("[3.00] fib threw " + e);
        }
    } //end

    public void testYFib() {
        try {
            final String output
                    = "((0 1) (1 1) (2 2) (3 3) (4 5) (5 8) (6 13) (7 21) (8 34) (9 55) (10 89))";
            final String input =
                    "let Y      := map f to  (map x to f(x(x)))(map x to f(x(x)));" +
                            //"                let g := map x to f(x(x)); " +
                            "      pair := map x,y to cons(x, cons(y, null));" +
                            "   FIBHELP := map fibhelp to map k,fn,fnm1 to if k = 0 then fn else fibhelp(k - 1, fn + fnm1, fn); " +
                            "in let FFIB := map ffib to map n to if n = 0 then 1 else (Y(FIBHELP))(n - 1,1,1); " +
                            "   in let FIBS := map fibs to map k,l to " +
                            "                    let fibk := (Y(FFIB))(k);" +
                            "                    in if k >= 0 then fibs(k - 1, cons(pair(k,fibk), l)) else l; " +
                            "      in (Y(FIBS))(10, null)";
            //valueCheck("fib-value", output, input);//stackoverflow
            this.needCheck("fib-need", output, input);
            this.nameCheck("fib-name", output, input);
            this.nameLazyConsCheck("fib-lazycons-name", output, input);
            this.needLazyConsCheck("fib-lazycons-need", output, input);
        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("[3.00] fib threw " + e);
        }
    }

    public void testZFact() {
        try {
            final String output
                    = "6";
            final String input =
                    "let Y    := map f to  (map x to f(map y to  (x(x))(y)))(map x to f(map y to (x(x))(y))); FACT := map f to map n to if n = 0 then 1 else n * f(n - 1); in (Y(FACT))(3) " ;
            //valueCheck("fib-value", output, input);//stackoverflow
            this.needCheck("fib-need", output, input);
            this.nameCheck("fib-name", output, input);
            this.nameLazyConsCheck("fib-lazycons-name", output, input);
            this.needLazyConsCheck("fib-lazycons-need", output, input);
        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("[3.00] fib threw " + e);
        }
    }


    public void testZConsFact() {
        try {
            final String output
                    = "0";
            final String input =
                    "let Ycons := map F to let g := map x to cons(first(F(x(x))),rest(F(x(x)))); in g(g); in first(Ycons(map zeros to cons(0, zeros)))" ;
            //valueCheck("fib-value", output, input);//stackoverflow
            //this.needCheck("fib-need", output, input);
            //this.nameCheck("fib-name", output, input);
            this.nameLazyConsCheck("fib-l-name", output, input);
            this.needLazyConsCheck("fib-l-need", output, input);
        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("[3.00] fib threw " + e);
        }
    }

    public void testLazyConsFact() {
        try {
            final String output
                    = "55";
      final String input =
          "let Ycons := map F to let g := map x to cons(first(F(x(x))),rest(F(x(x)))); in\n"
              + "g(g);\n"
              + "Yv := map F to let g := map x to map y to (F(x(x)))(y); in g(g);\n"
              + "in let sum := Yv(map s to map l to map k to if k = 0 then 0\n"
              + "else first(l) + (s(rest(l)))(k-1));\n"
              + "countup := Yv(map cu to map k to cons(k, cu(k+1)));\n"
              + "in (sum(countup(1)))(10)";
            //valueCheck("fib-value", output, input);//stackoverflow
            //this.needCheck("fib-need", output, input);
            //this.nameCheck("fib-name", output, input);
            this.nameLazyConsCheck("Ycons-name", output, input);
            this.needLazyConsCheck("Ycons-need", output, input);
        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("[3.00] fib threw " + e);
        }
    }

    public void testLazyConsSieve() {
        try {
            final String output
                    = "(3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73)";
      final String input =
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
      // valueCheck("fib-value", output, input);//stackoverflow
      this.needCheck("fib-need", output, input);
      // this.nameCheck("fib-name", output, input);
      //this.nameLazyConsCheck("Ycons-name", output, input);
      //this.needLazyConsCheck("Ycons-need", output, input);
      System.out.println(new Parser(new StringReader(input)).parse().toString());
      new Parser(new StringReader(input)).parse().toString();
        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("[3.00] fib threw " + e);
        }
    }

}
