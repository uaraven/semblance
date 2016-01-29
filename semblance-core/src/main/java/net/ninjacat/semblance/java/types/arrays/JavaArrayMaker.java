package net.ninjacat.semblance.java.types.arrays;

import net.ninjacat.semblance.data.collections.LispCollection;

import javax.annotation.Nonnull;

/**
 * Interface to make arrays from Lists and Vectors
 */
public interface JavaArrayMaker {

    /**
     * Converts {@link LispCollection} into Java array
     *
     * @param collection Source collection.
     * @return Java array.
     */
    @Nonnull
    Object covertToJavaArray(@Nonnull LispCollection collection);
}
