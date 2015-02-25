package net.ninjacat.semblance.data;

/**
 * Number atom
 */
public class NumberAtom extends Atom {

    private final long value;

    public NumberAtom(long value) {
        this.value = value;
    }

    @Override
    public String repr() {
        return String.valueOf(value);
    }
}
