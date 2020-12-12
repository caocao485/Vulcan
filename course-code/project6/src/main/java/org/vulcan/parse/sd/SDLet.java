package org.vulcan.parse.sd;

import org.vulcan.parse.Ast;

import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;

public class SDLet implements SDAst {
    private Ast[] asts;
    private Ast body;

    public SDLet(Ast[] asts, Ast body) {
        this.asts = asts;
        this.body = body;
    }

    public Ast[] getAsts() {
        return asts;
    }

    public void setAsts(Ast[] asts) {
        this.asts = asts;
    }

    public Ast getBody() {
        return body;
    }

    public void setBody(Ast body) {
        this.body = body;
    }

    @Override
    public String toString() {
        return "let " +
                "[*" + asts.length +
                "*] " +
                Arrays
                .stream(asts)
                .map(Objects::toString)
                .collect(Collectors.joining("; ")) +
                "; in " + body ;
    }

    @Override
    public <T> T accept(SDAstVisitor<T> v) {
        return v.forSDLet(this);
    }
}
