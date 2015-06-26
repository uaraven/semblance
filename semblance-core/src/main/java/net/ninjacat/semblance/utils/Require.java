package net.ninjacat.semblance.utils;

import net.ninjacat.semblance.data.collections.LispCollection;

/**
 * @author oleksiivoronin, date: 15-03-07.
 */
public final class Require {
    private Require() {
    }

    /**
     * Require dsl 'that' keyword
     *
     * @param collection Collection to get verificator for
     * @return {@link CollectionVerificator}
     */
    @SuppressWarnings("QuestionableName")
    public static CollectionVerificator that(final LispCollection collection) {
        return new CollectionVerificator(collection);
    }
}
