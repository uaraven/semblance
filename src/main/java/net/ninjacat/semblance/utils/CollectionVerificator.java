package net.ninjacat.semblance.utils;

import net.ninjacat.semblance.data.LispCollection;
import net.ninjacat.semblance.errors.runtime.ValidationException;

/**
 * @author oleksiivoronin, date: 15-03-07.
 */
@SuppressWarnings("NonBooleanMethodNameMayNotStartWithQuestion")
public class CollectionVerificator implements Verificator {
    private final LispCollection collection;

    CollectionVerificator(final LispCollection collection) {
        this.collection = collection;
    }

    /**
     * Verifies that collection has at least specified number of elements.
     *
     * @param length Number of elements.
     */
    public void hasAtLeast(final int length) {
        if (collection.length() < length) {
            throw new ValidationException(
                    String.format("Required at least %d elements", length), collection.getSourceInfo());
        }
    }

    /**
     * Verifies that collection has exactly specified number of elements.
     *
     * @param length Number of elements.
     */
    public void hasExactly(final int length) {
        if (collection.length() != length) {
            throw new ValidationException(
                    String.format("Required exactly %d elements", length), collection.getSourceInfo());
        }
    }
}
