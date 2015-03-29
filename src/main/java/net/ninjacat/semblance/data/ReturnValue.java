package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.utils.Values;

/**
 * Represents special value of return operation
 */
public class ReturnValue implements LispValue, DebugInfoProvider {

    private final LispValue value;
    private final SymbolAtom scope;
    private final SourceInfo sourceInfo;

    /**
     * Creates a new returned value.
     *
     * @param value returned value.
     * @param scope scope of return operation - name of the block to return from.
     */
    public ReturnValue(final LispValue value, final SymbolAtom scope) {
        this.value = value;
        this.scope = scope;
        sourceInfo = Values.getSourceInfo(value);
    }

    /**
     * Creates a new returned value.
     *
     * @param value returned value.
     */

    public ReturnValue(final LispValue value) {
        this(value, Constants.NONE);
    }

    /**
     * @return Scope of return operation.
     */
    public SymbolAtom getScope() {
        return scope;
    }

    /**
     * @return wrapped returned value.
     */
    public LispValue getValue() {
        return value;
    }

    /**
     * @return {@code true} if return is scoped
     */
    public boolean isScoped() {
        return !scope.equals(Constants.NONE);
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.RETURN;
    }

    @Override
    public String repr() {
        return "return " + value.repr();
    }

    @Override
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }
}
