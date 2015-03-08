package net.ninjacat.semblance.utils;

import net.ninjacat.semblance.data.LispCollection;

/**
 * @author oleksiivoronin, date: 15-03-07.
 */
public final class Require {
    private Require() {
    }

    public static CollectionVerificator that(final LispCollection collection) {
        return new CollectionVerificator(collection);
    }
}
