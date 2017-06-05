package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.LispCallable;
import net.ninjacat.semblance.data.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;

import javax.annotation.Nonnull;

/**
 * Abstract callable class which implements identity comparison for callables
 */
@SuppressWarnings("ComparableImplementedButEqualsNotOverridden")
public abstract class AbstractCallable implements LispCallable {

    private static final long serialVersionUID = 7194540722138030797L;

    @Override
    public int compareTo(@Nonnull final LispValue other) {
        //noinspection ObjectEquality
        if (other == this) {
            return 0;
        } else {
            throw new TypeMismatchException(getType(), other, SourceInfo.UNKNOWN);
        }
    }

}
