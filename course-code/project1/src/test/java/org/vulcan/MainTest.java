package org.vulcan;

import junit.framework.TestCase;
import org.vulcan.parse.Calculator;

public class MainTest extends TestCase {
    public void testEvaluate() {
        Calculator calculator = new Calculator();
        int sum = calculator.evaluate("1+2+3");
        assertEquals(6, sum);
    }
}