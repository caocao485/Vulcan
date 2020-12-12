package org.vulcan.parse.sd;

import org.vulcan.parse.Ast;


public class SDMap implements SDAst {
    private int paramLen;
    private Ast body;

    public SDMap(int paramLen, Ast body) {
        this.paramLen = paramLen;
        this.body = body;
    }

    public int getParamLen() {
        return paramLen;
    }

    public void setParamLen(int paramLen) {
        this.paramLen = paramLen;
    }

    public Ast getBody() {
        return body;
    }

    public void setBody(Ast body) {
        this.body = body;
    }

    @Override
    public <T> T accept(SDAstVisitor<T> v) {
        return v.forSDMap(this);
    }

    @Override
    public String toString() {
        return "map " +
                "[*" +
                paramLen +
                "*] to "
                + this.body;
    }
}
