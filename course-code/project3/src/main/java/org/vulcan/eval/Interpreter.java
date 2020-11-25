package org.vulcan.eval;


import org.vulcan.eval.value.JamVal;
import org.vulcan.parse.Ast;
import org.vulcan.parse.CallByValueVisitor;
import org.vulcan.parse.OtherCallVisitor;
import org.vulcan.parse.Parser;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import static org.vulcan.parse.OtherCallVisitor.forceEval;

/**
 * @author Think
 */
public class Interpreter {

    private final Parser parser;
    private final Ast astResult;

    public Interpreter(final String fileName) throws IOException {
        this.parser = new Parser(fileName);
        this.astResult = this.parser.parse();
    }


    public Interpreter(final Reader reader) {
        this.parser = new Parser(reader);
        this.astResult = this.parser.parse();
    }


    public JamVal callByValue() {
        return this.astResult.accept(new CallByValueVisitor(new Env<>()));
    }


    public JamVal callByName() {
        return forceEval(this.astResult.accept(new OtherCallVisitor(new Env<>(), false)));
    }

    public JamVal callByNeed() {
        return forceEval(this.astResult.accept(new OtherCallVisitor(new Env<>(), true)));
    }

    public static void main(String[] args) {
        final Interpreter interp = new Interpreter(new StringReader("let m:=(map x to x); in m"));
        //false
        System.out.println(interp.callByName().toString());
        //true
        System.out.println(interp.callByValue().toString());
    }
}

