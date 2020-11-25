package org.vulcan.eval.value;


import java.util.function.Supplier;

/**
 * @author Think
 */
public class Thunk<T extends JamVal> implements JamVal {
    private T value;
    private final Supplier<T> supplier;
    private final boolean shouldCached;

    public Thunk(final Supplier<T> supplier, boolean shouldCached) {
        this.supplier = supplier;
        this.shouldCached = shouldCached;
    }


    @Override
    public <T> T accept(final JamValVisitor<T> jamValVisitor) {
        throw new UnsupportedOperationException("thunk");
    }

    public T value() {
        if (this.shouldCached) {
            if (this.value == null) {
                this.value = this.supplier.get();
            }
            return this.value;
        } else {
            return this.supplier.get();
        }
    }

}

