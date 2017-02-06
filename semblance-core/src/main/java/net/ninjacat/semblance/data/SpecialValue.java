package net.ninjacat.semblance.data;

import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;

import javax.annotation.Nonnull;

@SuppressWarnings("ComparableImplementedButEqualsNotOverridden")
public final class SpecialValue implements LispValue {

    public static final SpecialValue LIST_END = new SpecialValue();
    public static final SpecialValue PROGRAM_END = new SpecialValue();
    public static final SpecialValue MAP_END = new SpecialValue();
    private static final long serialVersionUID = 8433978072647298899L;

    @Override
    public SemblanceType getType() {
        return SemblanceType.SPECIAL;
    }

    @Override
    public String repr() {
        return "special";
    }

    @Override
    public String printIt() {
        return "special";
    }

    @SuppressWarnings("ObjectEquality")
    @Override
    public int compareTo(@Nonnull final LispValue other) {
        if (other.getClass().equals(getClass())) {
            return other == this ? 0 : 1;
        } else {
            throw new TypeMismatchException(getType(), other, SourceInfo.UNKNOWN);
        }
    }

}
