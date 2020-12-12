package org.vulcan.parse.sd;

public class Pair implements SDAst {
    private int level;
    private int offset;

    public Pair(int level, int offset) {
        this.level = level;
        this.offset = offset;
    }

    public int getLevel() {
        return level;
    }

    public void setLevel(int level) {
        this.level = level;
    }

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset;
    }

    @Override
    public String toString() {
        return "[" +
                 level +
                "," + offset +
                ']';
    }

    @Override
    public <T> T accept(SDAstVisitor<T> v) {
        return v.forPair(this);
    }
}
