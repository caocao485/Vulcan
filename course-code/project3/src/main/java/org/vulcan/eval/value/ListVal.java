package org.vulcan.eval.value;

import java.util.Collection;

/**
 * @author Think
 */
public interface ListVal extends JamVal {

    /**
     * for toString
     * @param <T>
     * @return Collection
     */
    <T> Collection<? extends T> getValues();

    /**
     * for judge type
     * @return ValueType
     */
    ValueType getType();
}
