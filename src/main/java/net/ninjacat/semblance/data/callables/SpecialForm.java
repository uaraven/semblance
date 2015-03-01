package net.ninjacat.semblance.data.callables;

import net.ninjacat.semblance.data.Callable;
import net.ninjacat.semblance.data.SemblanceType;

/**
 * Created on 28/02/15.
 */
public abstract class SpecialForm implements Callable {

    @Override
    public String repr() {
        return null;
    }

    @Override
    public SemblanceType getType() {
        return null;
    }
}
