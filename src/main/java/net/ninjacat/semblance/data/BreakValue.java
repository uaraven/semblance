package net.ninjacat.semblance.data;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;
import net.ninjacat.semblance.utils.Values;

/**
 * Represents special value of break operation
 */
public class BreakValue implements LispValue, DebugInfoProvider {

    private final LispValue value;
    private final SourceInfo sourceInfo;

    /**
     * Creates a new break value.
     *
     * @param value returned value.
     */
    public BreakValue(final LispValue value) {
        this.value = value;
        sourceInfo = Values.getSourceInfo(value);
    }

    /**
     * @return wrapped returned value.
     */
    public LispValue getValue() {
        return value;
    }

    @Override
    public SemblanceType getType() {
        return SemblanceType.BREAK;
    }

    @Override
    public String repr() {
        return "break " + value.repr();
    }

    @Override
    public SourceInfo getSourceInfo() {
        return sourceInfo;
    }


    @Override
    public int compareTo(final LispValue other) {
        if (other.getClass().equals(getClass())) {
            return ((BreakValue) other).value.compareTo(value);
        } else {
            throw new TypeMismatchException(getType(), other, SourceInfo.UNKNOWN);
        }
    }
}
