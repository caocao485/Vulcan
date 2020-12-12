package org.vulcan.parse.sd;

import org.vulcan.parse.Ast;
import org.vulcan.parse.AstVisitor;

public interface SDAst extends Ast {

    @Override
    default <T> T accept(AstVisitor<T> v){
        return this.accept((SDAstVisitor<T>)v);
    }

    <T> T accept(SDAstVisitor<T> v);
}
