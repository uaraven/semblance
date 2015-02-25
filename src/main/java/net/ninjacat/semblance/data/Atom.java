package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.java.JavaConvertible;

/**
 * Lisp atom
 * <br>
 * All that is not a list is an atom. Atom evaluates to itself.
 */
public abstract class Atom implements LispValue, DebugInfoProvider, JavaConvertible {

    private final SourceInfo sourceInfo;

    public Atom() {
        this.sourceInfo = SourceInfo.UNKNOWN;
    }

    public Atom(SourceInfo sourceInfo) {
        this.sourceInfo = sourceInfo;
    }

    @Override
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }

    @Override
    public LispValue evaluate() {
        return this;
    }

    @Override
    public LispValue self() {
        return this;
    }
}
