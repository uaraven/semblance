package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.data.collections.LispValue;
import net.ninjacat.semblance.data.collections.SList;

import java.lang.reflect.Array;
import java.util.ArrayList;


/**
 * Utility methods to convert Semblance values to and from Java arrays
 */
public final class ArrayHelpers {

    private ArrayHelpers() {
    }

    /**
     * Converts {@link LispCollection} into Java array
     *
     * @param elementType Type of array elements
     * @param source      Source collection
     * @return Java array.
     */
    public static Object convertToArray(final Class elementType, final LispCollection source) {
        final Object array = Array.newInstance(elementType, source.length());
        int index = 0;
        for (final LispValue value : source) {
            Array.set(array, index, CallHelpers.convertValue(elementType, value));
            index += 1;
        }
        return array;
    }


    /**
     * Converts array into Semblance SList
     *
     * @param array Array
     * @return SList
     */
    public static LispCollection convertFromArray(final Object array) {
        final ArrayList<LispValue> values = new ArrayList<>();
        for (int i = 0; i < Array.getLength(array); i++) {
            values.add(CallHelpers.toLispValue(Array.get(array, i)));
        }
        return new SList(values);
    }

}
