package org.vulcan.parse.sd;

import org.vulcan.parse.AstVisitor;

public interface SDAstVisitor<T> extends AstVisitor<T> {

    T forSDLet(SDLet sdLet);

    T forSDLetRec(SDLetRec sdLetrec);

    T forSDMap(SDMap sdMap);

    T forPair(Pair pair);
}
