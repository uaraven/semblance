package net.ninjacat.semblance.data.special;

import net.ninjacat.semblance.data.Constants;
import net.ninjacat.semblance.data.SemblanceType;
import net.ninjacat.semblance.data.SymbolAtom;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.utils.Values;

import javax.annotation.Nonnull;

/**
 * Represents special value of return operation
 */
public class ReturnValue implements LispValue, DebugInfoProvider, WrappedValue {

    private static final long serialVersionUID = 7400610434601441656L;

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
    @Override
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
    public String printIt() {
        return "return " + value.printIt();
    }


    @Override
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }

    @Override
    public int compareTo(@Nonnull final LispValue other) {
        if (other.getClass().equals(getClass())) {
            return ((ReturnValue) other).value.compareTo(value);
        } else {
            throw new TypeMismatchException(getType(), other, SourceInfo.UNKNOWN);
        }
    }

    @Override
    public boolean equals(final Object other) {
        if (this == other) {
            return true;
        }
        if (other == null || getClass() != other.getClass()) {
            return false;
        }

        final ReturnValue thatValue = (ReturnValue) other;

        return value.equals(thatValue.value) && scope.equals(thatValue.scope);
    }

    @Override
    public int hashCode() {
        int result = value.hashCode();
        result = 31 * result + scope.hashCode();
        return result;
    }
}
