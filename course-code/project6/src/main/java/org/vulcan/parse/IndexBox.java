package org.vulcan.parse;

public class IndexBox {
    private int index;

    public IndexBox(int index) {
        this.index = index;
    }

    public int getIndex() {
        return index;
    }

    public int getAndAddIndex() {
        return index++;
    }

    public void setIndex(int index) {
        this.index = index;
    }
}
