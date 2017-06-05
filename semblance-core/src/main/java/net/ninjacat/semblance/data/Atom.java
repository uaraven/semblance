package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.java.JavaConvertible;

/**
 * Lisp atom
 * <br>
 * All that is not a list is an atom. Atom evaluates to itself.
 */
@SuppressWarnings("ClassNamingConvention")
public abstract class Atom implements LispValue, DebugInfoProvider, JavaConvertible {

    private static final long serialVersionUID = 8332913213026062187L;

    private final SourceInfo sourceInfo;

    Atom() {
        sourceInfo = SourceInfo.UNKNOWN;
    }

    Atom(final SourceInfo sourceInfo) {
        this.sourceInfo = sourceInfo;
    }

    @Override
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }


}
