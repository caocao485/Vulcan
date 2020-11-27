package org.vulcan.parse;

import junit.framework.TestCase;

import java.io.*;

public class Assign1Test extends TestCase {

    public Assign1Test(final String name) {
        super(name);
    }

    protected void checkString(final String name, final String answer, final String program) {
        final Parser p = new Parser(new StringReader(program));
        TestCase.assertEquals(name, answer, p.parse().toString());
    }

    protected void checkFile(final String name,
                             final String answerFilename,
                             final String programFilename) {
        System.out.println(answerFilename);
        try {
            final File answerFile = new File(answerFilename);
            final InputStream fin = new BufferedInputStream(new FileInputStream(answerFile));

            final int size = (int) answerFile.length();
            final byte[] data = new byte[size];
            fin.read(data, 0, size);
            final String answer = new String(data);


            final Parser p = new Parser(programFilename);
            System.out.println(p.parse());
            TestCase.assertEquals(name, "let f := map n to if (n = 0) then 1 else (n * f((n - 1))); in f(3)", p.parse().toString());
        } catch (final IOException e) {
            e.printStackTrace();
            TestCase.fail("Critical error: IOException caught while reading input file");
        }

    }

    public void testFile() {
        //this.checkFile("test ", "F:\\good-newline.text", "F:\\good-newline.text");
    }


    public void testAdd() {
        try {
            final String output = "(2 + 3)";
            final String input = "2+3";
            this.checkString("add", output, input);

        } catch (final Exception e) {
            TestCase.fail("add threw " + e);
        }
    } //end of func

    public void testPrim() {
        try {
            final String output = "first";
            final String input = "first";
            this.checkString("prim  ", output, input);

        } catch (final Exception e) {
            TestCase.fail("prim   threw " + e);
        }
    } //end of func

    public void testParseException() {
        try {
            final String output = "doh!";
            final String input = "map a, to 3";
            this.checkString("parseException", output, input);

            TestCase.fail("parseException did not throw ParseException exception");
        } catch (final ParseException e) {
            //e.printStackTrace();

        } catch (final Exception e) {
            TestCase.fail("parseException threw " + e);
        }
    } //end of func

    public void testLet() {
        try {
            final String output = "let a := 3; in (a + a)";
            final String input = "let a:=3; in a + a";
            this.checkString("let", output, input);

        } catch (final Exception e) {
            TestCase.fail("let threw " + e);
        }
    } //end of func

    public void testMap() {
        try {
            final String output = "map f to (map x to f(x(x)))(map x to f(x(x)))";
            final String input = "map f to (map x to f( x( x ) ) ) (map x to f(x(x)))";
            this.checkString("map", output, input);

        } catch (final Exception e) {
            TestCase.fail("map threw " + e);
        }
    } //end of func


    public void testMath() {
        try {
            final String input = "2 * 3 + 12";
            final String output = "((2 * 3) + 12)";
            System.out.println(input);
            this.checkString("let", output, input);

        } catch (final Exception e) {
            TestCase.fail("let threw " + e);
        }
    } //end of func


}