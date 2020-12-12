package org.vulcan.eval;


import org.vulcan.eval.value.JamVal;
import org.vulcan.parse.*;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;


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

    public JamVal valueValue(){
        return this.astResult.accept(new CheckAstVisitor(new Env<>())).accept(new CallByValueVisitor(new Env<>()));
    }

    ////TODO
    public Ast unshadow() {
        return  astResult.accept(new CheckAstVisitor(new Env<>()));
    }

    //TODO
    public Ast convertToCPS() {
        return astResult
                .accept(new CheckAstVisitor(new Env<>()))
                .accept(new CPSAstVisitor(new IndexBox(0)));
    }

    //TODO
    public JamVal cpsEval() {
        return astResult
                .accept(new CheckAstVisitor(new Env<>()))
                .accept(new CPSAstVisitor(new IndexBox(0)))
                .accept(new CallByValueVisitor(new Env<>()));
    }

    public Ast convertToSD(){
        return astResult
                .accept(new CheckAstVisitor(new Env<>()))
                .accept(new ToSDAstVisitor(new Env<>()));
    }

    public JamVal sdEval(){
        Ast tempAst = astResult
                .accept(new CheckAstVisitor(new Env<>()))
                .accept(new ToSDAstVisitor(new Env<>()));
        //System.out.println(tempAst);
        return tempAst
                .accept(new SDAstVisitorImpl(new SDEnv()));
    }

    public Ast convertCpsToSD(){
        return astResult
                .accept(new CheckAstVisitor(new Env<>()))
                .accept(new CPSAstVisitor(new IndexBox(0)))
                .accept(new ToSDAstVisitor(new Env<>()));
    }

    public JamVal CpsSdEval(){
        Ast tempAst = astResult
                .accept(new CheckAstVisitor(new Env<>()))
                .accept(new CPSAstVisitor(new IndexBox(0)))
                .accept(new ToSDAstVisitor(new Env<>()));
        System.out.println(tempAst);
        return tempAst
                .accept(new SDAstVisitorImpl(new SDEnv()));
    }

    public static void main(String[] args) {
        final Interpreter interp = new Interpreter(new StringReader("let m:=(map x to x); in m"));
        //false
        //System.out.println(interp.callByName().toString());
        //true
        //System.out.println(interp.callByValue().toString());
    }


}



