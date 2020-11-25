package org.vulcan.eval;

import junit.framework.TestCase;
import org.vulcan.parse.EvalException;
import org.vulcan.parse.ParseException;

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

    private void allCheck(final String name, final String answer, final String program) {
        this.valueCheck(name, answer, program);
        this.nameCheck(name, answer, program);
        this.needCheck(name, answer, program);
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
        } catch (final Exception e) {
            e.printStackTrace();
            TestCase.fail("[3.00] fib threw " + e);
        }
    }

}
