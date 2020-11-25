package org.vulcan.eval.value;


/**
 * @author Think
 */
public interface JamValVisitor<T> {

    /**
     * for boolVal
     * @param boolVal
     * @return
     */
    T forBoolVal(BoolVal boolVal);

    /**
     * for closureVal
     * @param closureVal
     * @return
     */
    T forClosureVal(ClosureVal closureVal);

    /**
     * for ConsVal
     * @param consVal
     * @return
     */
    T forConsVal(ConsVal consVal);

    /**
     * for NullVal
     * @param nullVal
     * @return
     */
    T forNullVal(NullVal nullVal);

    /**
     * for NumVal
     * @param numVal
     * @return
     */
    T forNumVal(NumVal numVal);

    /**
     * for primFunVal
     * @param primFunVal
     * @return
     */
    T forPrimFunVal(PrimFunVal primFunVal);

}
