package net.ninjacat.semblance.data;

/**
 * Lisp atom
 * <br>
 * All that is not a list is an atom. Atom evaluates to itself.
 */
public abstract class Atom implements LispValue {
    @Override
    public LispValue evaluate() {
        return this;
    }

    @Override
    public LispValue self() {
        return this;
    }
}
