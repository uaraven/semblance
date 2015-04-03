package net.ninjacat.semblance.data;

import net.ninjacat.semblance.debug.DebugInfoProvider;
import net.ninjacat.semblance.debug.SourceInfo;
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
}
