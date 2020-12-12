package org.vulcan.parse.sd;

import org.vulcan.parse.Ast;

import java.util.Arrays;
import java.util.Objects;
import java.util.stream.Collectors;

public class SDLetRec implements SDAst {
    private SDMap[] maps;
    private Ast body;

    public SDLetRec(SDMap[] maps, Ast body) {
        this.maps = maps;
        this.body = body;
    }

    public SDMap[] getMaps() {
        return maps;
    }

    public void setMaps(SDMap[] maps) {
        this.maps = maps;
    }

    public Ast getBody() {
        return body;
    }

    public void setBody(Ast body) {
        this.body = body;
    }

    @Override
    public String toString() {
        return "letrec " +
                "[*" + maps.length +
                "*] " +
                Arrays
                        .stream(maps)
                        .map(Objects::toString)
                        .collect(Collectors.joining("; ")) +
                "; in " + body ;
    }

    @Override
    public <T> T accept(SDAstVisitor<T> v) {
        return v.forSDLetRec(this);
    }
}
