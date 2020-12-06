package org.vulcan.eval;

import java.util.StringTokenizer;
import junit.framework.TestCase;
import org.vulcan.parse.EvalException;
import org.vulcan.parse.ParseException;

import java.io.*;

public class Assign3Test extends TestCase {

    public Assign3Test (String name) {
        super(name);
    }

    /**
     * The following 9 check methods create an interpreter object with the
     * specified String as the program, invoke the respective evaluation
     * method (valueValue, valueName, valueNeed, etc.), and check that the
     * result matches the (given) expected output.
     */

    private void valueValueCheck(String name, String answer, String program) {
        Interpreter interp = new Interpreter(new StringReader(program));
        assertEquals("by-value-value " + name, answer, interp.valueValue().toString());
    }

    private void valueNameCheck(String name, String answer, String program) {
        Interpreter interp = new Interpreter(new StringReader(program));
        assertEquals("by-value-name " + name, answer, interp.valueName().toString());
    }

    private void valueNeedCheck(String name, String answer, String program) {
        Interpreter interp = new Interpreter(new StringReader(program));
        assertEquals("by-value-need " + name, answer, interp.valueNeed().toString());
    }

    private void nameValueCheck(String name, String answer, String program) {
        Interpreter interp = new Interpreter(new StringReader(program));
        assertEquals("by-value " + name, answer, interp.nameValue().toString());
    }

    private void nameNameCheck(String name, String answer, String program) {
        Interpreter interp = new Interpreter(new StringReader(program));
        assertEquals("by-name " + name, answer, interp.nameName().toString());
    }

    private void nameNeedCheck(String name, String answer, String program) {
        Interpreter interp = new Interpreter(new StringReader(program));
        assertEquals("by-need " + name, answer, interp.nameNeed().toString());
    }

    private void needValueCheck(String name, String answer, String program) {
        Interpreter interp = new Interpreter(new StringReader(program));
        assertEquals("by-value " + name, answer, interp.needValue().toString());
    }

    private void needNameCheck(String name, String answer, String program) {
        Interpreter interp = new Interpreter(new StringReader(program));
        assertEquals("by-name " + name, answer, interp.needName().toString());
    }

    private void needNeedCheck(String name, String answer, String program) {
        Interpreter interp = new Interpreter(new StringReader(program));
        assertEquals("by-need " + name, answer, interp.needNeed().toString());
    }

    private void allCheck(String name, String answer, String program) {
        valueValueCheck(name, answer, program);
        valueNameCheck(name, answer, program);
        valueNeedCheck(name, answer, program);
        nameValueCheck(name, answer, program);
        nameNameCheck(name, answer, program);
        nameNeedCheck(name, answer, program);
        needValueCheck(name, answer, program);
        needNameCheck(name, answer, program);
        needNeedCheck(name, answer, program);
    }

    private void noNameCheck(String name, String answer, String program) {
        valueValueCheck(name, answer, program);
        valueNameCheck(name, answer, program);
        valueNeedCheck(name, answer, program);
        needValueCheck(name, answer, program);
        needNameCheck(name, answer, program);
        needNeedCheck(name, answer, program);
    }

    private void noValueConsCheck(String name, String answer, String program) {
        valueNameCheck(name, answer, program);
        valueNeedCheck(name, answer, program);
        nameNameCheck(name, answer, program);
        nameNeedCheck(name, answer, program);
        needNameCheck(name, answer, program);
        needNeedCheck(name, answer, program);
    }

    private void needCheck(String name, String answer, String program) {
        needNameCheck(name, answer, program);
        needNeedCheck(name, answer, program);
    }


    private void lazyCheck(String name, String answer, String program) {
        valueNameCheck(name, answer, program);
        valueNeedCheck(name, answer, program);
        nameNameCheck(name, answer, program);
        nameNeedCheck(name, answer, program);
        needNameCheck(name, answer, program);
        needNeedCheck(name, answer, program);
    }

    public void testSyntaxExceptionExists() {
        new SyntaxException("Is it defined?");
    } //end of func

    public void testNumberP() {
        try {
            String output = "number?";
            String input = "number?";
            allCheck("numberP", output, input );

        } catch (Exception e) {
            e.printStackTrace();
            fail("numberP threw " + e);
        }
    } //end of func


    public void testMathOp() {
        try {
            String output = "18";
            String input = "2 * 3 + 12";
            allCheck("mathOp", output, input );

        } catch (Exception e) {
            e.printStackTrace();
            fail("mathOp threw " + e);
        }
    } //end of func


    public void testParseException() {
        try {
            String output = "haha";
            String input = " 1 +";
            allCheck("parseException", output, input );

            fail("parseException did not throw ParseException exception");
        } catch (ParseException e) {
            //e.printStackTrace();

        } catch (Exception e) {
            e.printStackTrace();
            fail("parseException threw " + e);
        }
    } //end of func


    public void testEvalException() {
        try {
            String output = "mojo";
            String input = "1 + number?";
            allCheck("evalException", output, input );

            fail("evalException did not throw EvalException exception");
        } catch (EvalException e) {
            //e.printStackTrace();

        } catch (Exception e) {
            e.printStackTrace();
            fail("evalException threw " + e);
        }
    } //end of func


    public void testAppend() {
        try {
            String output = "(1 2 3 1 2 3)";
            String input = "let Y    := map f to              let g := map x to f(map z1,z2 to (x(x))(z1,z2));     in g(g);  APPEND := map ap to            map x,y to               if x = null then y else cons(first(x), ap(rest(x), y)); l      := cons(1,cons(2,cons(3,null))); in (Y(APPEND))(l,l)";
            allCheck("append", output, input );

        } catch (Exception e) {
            e.printStackTrace();
            fail("append threw " + e);
        }
    } //end of func


    public void testLetRec() {
        try {
            String output = "(1 2 3 1 2 3)";
            String input = "let append :=       map x,y to          if x = null then y else cons(first(x), append(rest(x), y));    l      := cons(1,cons(2,cons(3,null))); in append(l,l)";
            allCheck("letRec", output, input );

        } catch (Exception e) {
            e.printStackTrace();
            fail("letRec threw " + e);
        }
    } //end of func


    public void testLazyCons() {
        try {
            String output = "0";
            String input = "let zeroes := cons(0,zeroes);in first(rest(zeroes))";
            lazyCheck("lazyCons", output, input );

        } catch (Exception e) {
            e.printStackTrace();
            fail("lazyCons threw " + e);
        }
    } //end of func

    public void testFreeVariable() {
        try {
            String output = "0";
            String input = "x";
            allCheck("free variable", output, input );

            fail("SyntaxException did not throw SyntaxException  exception");
        } catch (SyntaxException e) {
           // e.printStackTrace();
            //fail("SyntaxException threw " + e);
        }
    }

    public void testMapDuplicate() {
        try {
            String output = "0";
            String input = "map x,x to (x + y)";
            allCheck("duplicate elements in map", output, input );

            fail("SyntaxException did not throw SyntaxException  exception");
        } catch (SyntaxException e) {
            // e.printStackTrace();
            //fail("SyntaxException threw " + e);
        }
    }

    public void testLetDuplicate() {
        try {
            String output = "0";
            String input = "let x :=  1; x:=6; in (x + x)";
            allCheck("duplicate elements in let", output, input );

            fail("SyntaxException did not throw SyntaxException  exception");
        } catch (SyntaxException e) {
            // e.printStackTrace();
            //fail("SyntaxException threw " + e);
        }
    }

    public void testLetFreeVariables() {
        try {
            String output = "0";
            String input = "let x := 1; in (x + y)";
            allCheck("free variable in let", output, input );

            fail("SyntaxException did not throw SyntaxException  exception");
        } catch (SyntaxException e) {
            //e.printStackTrace();
            //fail("SyntaxException threw " + e);
        }
    }

    public void testMathOperate(){
        try {
            String output = "0";
            String input = "~2";
            allCheck("expected an arg of type bool", output, input );

            fail("EvalException did not throw EvalException  exception");
        } catch (EvalException e) {
            //e.printStackTrace();
            //fail("SyntaxException threw " + e);
        }
    }

    public void testConsEqualityOnes(){
        try {
            String output = "true";
            String input = "let ones := cons(1, ones); in ones = cons(1, ones)";
            needCheck("expected an arg of type bool", output, input );


        } catch (EvalException e) {
            //e.printStackTrace();
            //fail("SyntaxException threw " + e);
        }
    }

    public void testConsEqualityOnesZero(){
        try {
            String output = "false";
      String input =
          "let xs := cons(0, xs);\n" + "    ys := cons(0, cons(0, null));\n" + "in xs = ys";
            noValueConsCheck("expected an arg of type bool", output, input );


        } catch (EvalException e) {
            e.printStackTrace();
            //fail("SyntaxException threw " + e);
        }
    }

    public void testConsEqualityOnesNull(){
        try {
            String output = "true";
      String input = "let xs := cons(1, 2);\n" + "    ys := cons(3, null);\n" + "in xs != ys";
            noValueConsCheck("expected an arg of type bool", output, input );


        } catch (EvalException e) {
            //e.printStackTrace();
            //fail("SyntaxException threw " + e);
        }
    }

    public void testCountUp(){
        try {
            String output = "55";
      String input =
          "let Ycons := map F to let g := map x to cons(first(F(x(x))),rest(F(x(x)))); in\n"
              + "g(g);\n"
              + "Yv := map F to let g := map x to map y to (F(x(x)))(y); in g(g);\n"
              + "in let sum := Yv(map s to map l to map k to if k = 0 then 0\n"
              + "else first(l) + (s(rest(l)))(k-1));\n"
              + "countup := Yv(map cu to map k to cons(k, cu(k+1)));\n"
              + "in (sum(countup(1)))(10)";
            noValueConsCheck("expected an arg of type bool", output, input );


        } catch (EvalException e) {
            //e.printStackTrace();
            //fail("SyntaxException threw " + e);
        }
    }

}




