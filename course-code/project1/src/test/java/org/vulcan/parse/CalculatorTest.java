package org.vulcan.parse;

import junit.framework.TestCase;

public class CalculatorTest extends TestCase {

    public void testEvaluate() {
        Calculator calculator = new Calculator();
        int sum = calculator.evaluate("1+2+3");
        assertEquals(6, sum);
    }
}