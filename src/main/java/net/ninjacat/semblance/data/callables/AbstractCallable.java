package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.Callable;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.debug.SourceInfo;
import net.ninjacat.semblance.errors.runtime.TypeMismatchException;

import javax.annotation.Nonnull;

/**
 * TODO: Write JavaDoc
 */
public abstract class AbstractCallable implements Callable {

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
