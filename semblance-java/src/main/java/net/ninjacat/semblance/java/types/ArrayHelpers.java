package net.ninjacat.semblance.java.types;

import net.ninjacat.semblance.data.collections.LispCollection;
import net.ninjacat.semblance.java.types.arrays.*;
import net.ninjacat.smooth.collections.Maps;

import java.math.BigInteger;
import java.util.Map;


/**
 * Utility methods to convert Semblance values to and from Java arrays
 */
public final class ArrayHelpers {

    private static final Map<Class, JavaArrayMaker> ARRAY_MAKERS = Maps.of(
            int.class, new IntArrayMaker(),
            long.class, new LongArrayMaker(),
            short.class, new ShortArrayMaker(),
            byte.class, new ByteArrayMaker(),
            char.class, new CharArrayMaker(),
            float.class, new FloatArrayMaker(),
            double.class, new DoubleArrayMaker(),
            BigInteger.class, new BigIntArrayMaker()
    );

    private static final JavaArrayMaker DEFAULT = new ObjectArrayMaker();

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
        if (ARRAY_MAKERS.containsKey(elementType)) {
            return ARRAY_MAKERS.get(elementType).covertToJavaArray(source);
        } else {
            return DEFAULT.covertToJavaArray(source);
        }
    }

}
