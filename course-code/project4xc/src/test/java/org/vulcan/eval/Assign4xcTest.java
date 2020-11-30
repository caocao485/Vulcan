package org.vulcan.eval;


import junit.framework.TestCase;
import org.vulcan.parse.EvalException;
import org.vulcan.parse.ParseException;

import java.io.*;

public class Assign4xcTest extends TestCase {

    public Assign4xcTest (String name) {
        super(name);
    }

    private void algolCheck(String name, String answer, String program) {
        Interpreter interp = new Interpreter(new StringReader(program));
        assertEquals("by-value-value " + name, answer, interp.valueValue().toString());
    }



    public void testNumberP() {
        try {
            String output = "number?";
            String input = "number?";
            algolCheck("numberP", output, input );

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("numberP threw " + e);
        }
    } //end of func


    public void testMathOp() {
        try {
            String output = "18";
            String input = "2 * 3 + 12";
            algolCheck("mathOp", output, input );

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("mathOp threw " + e);
        }
    } //end of func


    public void testParseException() {
        try {
            String output = "haha";
            String input = " 1 +";
            algolCheck("parseException", output, input );

            fail("parseException did not throw ParseException exception");
        } catch (ParseException e) {
            //e.printStackTrace();

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("parseException threw " + e);
        }
    } //end of func


    public void testEvalException() {
        try {
            String output = "mojo";
            String input = "1 + number?";
            algolCheck("evalException", output, input );

            fail("evalException did not throw EvalException exception");
        } catch (EvalException e) {
            //e.printStackTrace();

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("evalException threw " + e);
        }
    } //end of func


    public void testAppend() {
        try {
            String output = "(1 2 3 1 2 3)";
            String input = "let Y    := map f to              let g := map x to f(map z1,z2 to (x(x))(z1,z2));     in g(g);  APPEND := map ap to            map x,y to               if x = null then y else cons(first(x), ap(rest(x), y)); l      := cons(1,cons(2,cons(3,null))); in (Y(APPEND))(l,l)";
            algolCheck("append", output, input );

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("append threw " + e);
        }
    } //end of func


    public void testLetRec() {
        try {
            String output = "(1 2 3 1 2 3)";
            String input = "let append :=       map x,y to          if x = null then y else cons(first(x), append(rest(x), y));    l      := cons(1,cons(2,cons(3,null))); in append(l,l)";
            algolCheck("letRec", output, input );

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("letRec threw " + e);
        }
    } //end of func


    public void testEmptyBlock() {
        try {
            String output = "0";
            String input = "{ }";
            algolCheck("emptyBlock", output, input );

            fail("emptyBlock did not throw ParseException exception");
        } catch (ParseException e) {
            //e.printStackTrace();

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("emptyBlock threw " + e);
        }
    } //end of func


    public void testBlock() {
        try {
            String output = "1";
            String input = "{3; 2; 1}";
            algolCheck("block", output, input );

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("block threw " + e);
        }
    } //end of func


    public void testDupVar() {
        try {
            String output = "ha!";
            String input = "let x:=3; x:=4; in x";
            algolCheck("dupVar", output, input );

            fail("dupVar did not throw SyntaxException exception");
        } catch (SyntaxException e) {
            //e.printStackTrace();

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("dupVar threw " + e);
        }
    } //end of func


    public void testSwap() {
        try {
            String output = "2500";
            String input = "\n                              let\n                                ref x := 20;\n			        ref y := 5;\n				z := 10;\n				swap := map ref x, ref y to\n				  let z := x;\n				  in {x <- y; y <- z};\n			      in\n			      { x <- (x + y);\n			        swap(x,y);\n				swap(x,z);\n				x * y * z}\n				";
            algolCheck("swap", output, input );

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("swap threw " + e);
        }
    } //end of func

    public void testRefReturn() {
        try {
            String output = "50";
            String input = "let ref x :=  10; ref y :=  x; in {y <- 40;x+10} //50";
            algolCheck("dupVar", output, input );


        } catch (SyntaxException e) {
            //e.printStackTrace();

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("dupVar threw " + e);
        }
    } //end of func


    public void testRefRef(){
        try {
            String output = "20";
            String input = "let ref x :=  10; ref y :=  40; in {y <- x;y <- 3444;x+10} //20";
            algolCheck("dupVar", output, input );


        } catch (SyntaxException e) {
            //e.printStackTrace();

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("dupVar threw " + e);
        }

    }

    public void testRefLet(){
        try {
            String output = "10";
      String input =
          " let ref x0 := 5;\n"
              + "in \n"
              + "let ref zz :=\n"
              + "let\n"
              + " ref x := x0;\n"
              + " y := x;\n"
              + " z := 10;\n"
              + " swap := map ref x, ref y to\n"
              + "            let z := x;\n"
              + "            in {x <- y; y <- z};\n"
              + "in\n"
              + " {x <- (x + y);\n"
              + "  swap(x,y);\n"
              + "  swap(x,z);\n"
              + "  x};\n"
              + " in {x0} //10";
            algolCheck("dupVar", output, input );


        } catch (SyntaxException e) {
            //e.printStackTrace();

        } catch (Exception e) {
            e.printStackTrace(System.err);
            fail("dupVar threw " + e);
        }

    }


}